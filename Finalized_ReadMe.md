
# Transaction Anomaly Detection

## Overview

Given: the following data sheet (abbreviated) containing transaction observations grouped by categorical variables of which those transactions were described. Each observation (grouping) includes the number of those transactions that occurred at each hour, and how many of those transactions were successful.

**Goal:** Merchants started reporting customer complaints about failed transactions on February 14th. Given data on the previous 48 hours, find out what happened.

![Data Sheet](https://github.com/user-attachments/assets/91a3e897-2ff0-40ea-aab9-697aa93aed8c)

---

## Initial Setup

```r
library('ggplot2')
setwd("/Users/jacobrichards/Desktop/DS assessment/DS_exam_2")
transactions <- read.csv(file="trans.csv", na.strings = c("", "NA"))
transactions[is.na(transactions)] <- "notprovided"
```

```r
unique_hours <- unique(data$hr)
t <- aggregate(data$t, by = list(data$hr), sum)
s <- aggregate(data$s, by = list(data$hr), sum)

f <- t[,2] - s[,2]
failure_rate <- f / t[,2]
failure_count <- f

unique_hours <- sort(unique_hours)
```

---

## Initial Investigation

### Failed Transaction Rate by Hour

```r
failed_transactions_rate <- data.frame(hours = unique_hours, failedTransactions = failure_rate, x_index = seq(1, 72, by = 1))
ggplot(data = failed_transactions_rate, aes(x = x_index, y = failedTransactions)) + 
  geom_area(fill = "blue", alpha = 0.25) + geom_line(color = "black") +  
  scale_x_continuous(breaks = seq(1, 72, by = 6), minor_breaks = 1:72, labels = unique_hours[seq(1, length(unique_hours), by = 6)]) + 
  coord_cartesian(ylim = range(failed_transactions_rate$failedTransactions, na.rm = TRUE)) +  
  labs(title = "Failed Transactions Percentage by Hour", x = "Hour (72)", y = "Failed Transactions Per Hour") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8))
```
![Percent Failed Preliminary](https://github.com/user-attachments/assets/810910d9-7b10-42ac-ae60-197fc0089c2c)

### Failed Transaction Count by Hour

```r
failed_transactions <- data.frame(hours = unique_hours, failedTransactions = failure_count, x_index = seq(1, 72, by = 1))
ggplot(data = failed_transactions, aes(x = x_index, y = failedTransactions)) + 
  geom_area(fill = "blue", alpha = 0.25) + geom_line(color = "black") +  
  scale_x_continuous(breaks = seq(1, 72, by = 6), minor_breaks = 1:72, labels = unique_hours[seq(1, length(unique_hours), by = 6)]) + 
  coord_cartesian(ylim = range(failed_transactions$failedTransactions, na.rm = TRUE)) +  
  labs(title = "Failed Transactions Counts by Hour", x = "Hour (72)", y = "Failed Transactions Per Hour") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8))
```
![Percent Count Preliminary](https://github.com/user-attachments/assets/d9a09a24-65ab-4b86-b166-1573b55a585a)

---

## Anomaly Detection

### Weighted Failure Rate

```r
data <- transactions
colnames(data) <- c("t","s","mid","pmt","pg","subtype","hr","bank")
weighted_failure_rate <- numeric(nrow(data))
weighted_failure_rate <- data[,1] - data[,2] / data[,1] * log(1 + data[,1])
data$weighted <- weighted_failure_rate; data_original <- data 
```

Appending a weighted failure rate to each observation, which is simply the failure rate of the observation multiplied by `log(1 + transactions)`, so that observations with more transactions have more significance than another observation with equal failure rate but fewer transactions.

### Mahalanobis Distance

```r
data$pmt <- as.numeric(as.factor(data$pmt)); data$pg <- as.numeric(as.factor(data$pg))
data$bank <- as.numeric(as.factor(data$bank)); data$subtype <- as.numeric(as.factor(data$subtype))

features <- data[, c("weighted", "pmt", "pg", "bank", "subtype")]
center <- colMeans(features); cov_matrix <- cov(features)
mahalanobis_distances <- mahalanobis(features, center, cov_matrix)
data$mahalanobis_score <- mahalanobis_distances
data <- data[order(data$mahalanobis_score, decreasing = TRUE),]
top_quartile <- quantile(data$weighted, 0.999)
filtered_data <- data[data$weighted >= top_quartile, ]

filtered_data <- filtered_data[order(filtered_data$mahalanobis_score, decreasing = TRUE), ]
original_observations_found_anomalous <- data_original[rownames(filtered_data),]
```
![Anomalous Observations Table](https://github.com/user-attachments/assets/524fc9a1-d06b-44e2-83f6-8b622f52cc56)

### Observations

These are the observations the anomaly detection model found to be the most abnormal. The exact combination of variables present in these observations is not the anomaly because if you plot the failure rate of:

```r
paytm_subset <- data[(data[,5] %in% c("PAYTM", "PAYTM_V2", "PAYTM_UPI", "notprovided")) & (data[,6] %in% c("UPI_PAY")) & (data[,4] == "UPI"),]
```
You just get white noise.

![White Noise](https://github.com/user-attachments/assets/00bd4cfe-e04f-438f-8764-2d8f8d077352)

---

## Key Findings

However, for all the observations returned by the anomaly detection method, all of them have a PAYTM service as their payment gateway (`pg`). From that, I deduced that the anomaly would be in any of the PAYTM gateways, but not in the subtype "UPI_PAY". After about 10 tries of plotting different combinations of categories that satisfied this criterion, the following was found:

```r
paytm_subset <- data[(data[,5] %in% c("PAYTM", "PAYTM_V2", "PAYTM_UPI", "notprovided")) & (data[,6] %in% c("UPI_COLLECT")) & (data[,4] == "UPI"),]
```
![Failure Percent Isolated Anomaly](https://github.com/user-attachments/assets/fc7f4ee5-5190-47cc-ba4b-4a1578035d1c)

This combination of UPI and PAYTM services produced an 80% failure rate from February 13th, 6 PM to February 14th, 9 AM. The combination being:
- **Payment Method:** UPI
- **Payment Gateway:** PAYTM (or its variants)
- **Sub-Type:** UPI_COLLECT

---

## Merchant Impact Analysis

```r
paytm_subset$f <- paytm_subset[,1] - paytm_subset[,2]
failure_sum_by_merchant <- aggregate(paytm_subset[,10],by=list(paytm_subset$mid),sum)
transaction_sum_by_merchant <- aggregate(paytm_subset[,1],by=list(paytm_subset$mid),sum)
failure_sum_by_merchant$transction_sum <- transaction_sum_by_merchant[,2]
failure_sum_by_merchant$failue_rate_merchant <- failure_sum_by_merchant[,2] / failure_sum_by_merchant[,3]
subset_failures_by_merchant <- failure_sum_by_merchant; data$failures <- data[,1] - data[,2]
```

![Merchant Impact Plot](https://github.com/user-attachments/assets/ff914a7b-960e-474c-b619-75ff7e291fa1)

---

## Further Analysis

### Failure Rate Comparison

```r
paytm_compliment <- data[!(rownames(data) %in% rownames(paytm_subset)), ]

t <- aggregate(paytm_compliment$t, by = list(paytm_compliment$hr), sum)
s <- aggregate(paytm_compliment$s, by = list(paytm_compliment$hr), sum)
f <- t[,2] - s[,2]

proportion_c <- f / t[,2] * 100
```

![Failure Percentage Curve](https://github.com/user-attachments/assets/b8a4740b-e299-4a23-8312-b5bf514d2f74)

---

## Conclusion

There are tons of plots that can be produced here, but only one has meaningful implications.

**Question:** Could this anomaly have been predicted before it occurred?  
- These are two distinct distributions representing two distinct processes. Something happened which caused this mean shift that could not have been predicted from the data before the anomaly.
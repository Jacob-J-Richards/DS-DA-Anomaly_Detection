Given: the following data sheet (abbreviated) containing transaction observations grouped by categorical variables of which those transactions are described by. Each observation (grouping) includes the number of those transactions that occured at each hour, and how many of those transactions were sucessful.

Goal: Merchants started reporting customer complaints about failed transactions on Febury 14th, given data on the previous 48 hours, find out what happened. 

<img width="1259" alt="386206442-2bd0532e-25c7-492a-b3cf-53e1e8297240" src="https://github.com/user-attachments/assets/91a3e897-2ff0-40ea-aab9-697aa93aed8c">


``` r
library('ggplot2')
setwd("/Users/jacobrichards/Desktop/DS assesment/DS_exam_2")
#setwd("C:/Users/jake pc/Desktop/exam_2_restart")
transactions <- read.csv(file="trans.csv", na.strings = c("", "NA"))
transactions[is.na(transactions)] <- "notprovided"
```


``` r
unique_hours <- unique(data$hr)
t <- aggregate(data$t,by=list(data$hr),sum)
s <- aggregate(data$s,by=list(data$hr),sum)

f <- t[,2] - s[,2]
failure_rate <- f/t[,2]
failure_count <- f

unique_hours <- unique(data$hr)
unique_hours <- sort(unique_hours)
```

Initial investigation by plotting failed transaction rate by hour. 

``` r
failed_transactions_rate <- data.frame(hours = unique_hours, failedTransactions = failure_rate, x_index = seq(1, 72, by = 1))
ggplot(data = failed_transactions_rate, aes(x = x_index, y = failedTransactions)) + 
geom_area(fill = "blue", alpha = 0.25) + geom_line(color = "black") +  
scale_x_continuous(breaks = seq(1, 72, by = 6), minor_breaks = 1:72, labels = unique_hours[seq(1, length(unique_hours), by = 6)]) + 
coord_cartesian(ylim = range(failed_transactions_rate$failedTransactions, na.rm = TRUE)) +  
labs(title = "Failed Transactions Percentage by Hour", x = "Hour (72)", y = "Failed Transactions Per Hour") +
theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8), 
axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
plot.background = element_rect(fill = "white", color = NA),panel.background = element_rect(fill = "white", color = NA),
panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), 
legend.position = "none"); ggsave("percent_failed_before.png", plot = last_plot(), width = 10, height = 6, dpi = 300)
```


![percent_failed_prelim](https://github.com/user-attachments/assets/810910d9-7b10-42ac-ae60-197fc0089c2c)

Initial investigation by plotting failed transaction count by hour. 

``` r
failed_transactions <- data.frame(hours = unique_hours, failedTransactions = failure_count, x_index = seq(1, 72, by = 1))
ggplot(data = failed_transactions, aes(x = x_index, y = failedTransactions)) + 
geom_area(fill = "blue", alpha = 0.25) + 
geom_line(color = "black") +  
scale_x_continuous(breaks = seq(1, 72, by = 6), minor_breaks = 1:72, labels = unique_hours[seq(1, length(unique_hours), by = 6)]) + 
coord_cartesian(ylim = range(failed_transactions$failedTransactions, na.rm = TRUE)) +  
labs(title = "Failed Transactions Counts by Hour", x = "Hour (72)", y = "Failed Transactions Per Hour") +
theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8), 
axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
plot.background = element_rect(fill = "white", color = NA),panel.background = element_rect(fill = "white", color = NA),
panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), 
legend.position = "none")
```
![percent_count_prelim](https://github.com/user-attachments/assets/d9a09a24-65ab-4b86-b166-1573b55a585a)

There isn't really anything to go off from these plots so we're going to try an anamoly detection method. 

```{r}
data <- transactions
colnames(data) <- c("t","s","mid","pmt","pg","subtype","hr","bank")
weighted_failure_rate <- numeric(nrow(data))
weighted_failure_rate <- data[,1] - data[,2] / data[,1] * log(1+data[,1])
data$weighted <- weighted_failure_rate; data_original <- data 
```
Appending a weighted failure rate to each observation, which is simply the failure rate of the observation multiplied by log(1+transactions), so that observations with more trasactions have more signifigance than another observation with equal failure rate but fewer transactions. 

``` r
data$pmt <- as.numeric(as.factor(data$pmt)); data$pg <- as.numeric(as.factor(data$pg))
data$bank <- as.numeric(as.factor(data$bank)); data$subtype <- as.numeric(as.factor(data$subtype))

features <- data[, c("weighted", "pmt", "pg", "bank", "subtype")]
center <- colMeans(features); cov_matrix <- cov(features)
mahalanobis_distances <- mahalanobis(features, center, cov_matrix)
data$mahalanobis_score <- mahalanobis_distances
data <- data[order(data$mahalanobis_score,decreasing = TRUE),]
top_quartile <- quantile(data$weighted, 0.999)
filtered_data <- data[data$weighted >= top_quartile, ]

filtered_data <- filtered_data[order(filtered_data$mahalanobis_score, decreasing = TRUE), ]
original_observations_found_anamolous <- data_original[rownames(filtered_data),]
original_observations_found_anamolous; data <- data_original
```
![Anamaly_observations_table](https://github.com/user-attachments/assets/524fc9a1-d06b-44e2-83f6-8b622f52cc56)


These are the observations the anamoly detection model found to be the most abnormal. You can tell that the exact combination of variables present in these observations is not the anamoly because if you plot the failure rate of 

```{r}
paytm_subset <- data[(data[,5] %in% c("PAYTM", "PAYTM_V2", "PAYTM_UPI", "notprovided")) & (data[,6] %in% c("UPI_PAY")) & (data[,4] == "UPI"),]
```

You just get white noise. 

![testettjjf320](https://github.com/user-attachments/assets/00bd4cfe-e04f-438f-8764-2d8f8d077352)

However, for all of the observations returned by the anamoly detection method, all of them have a PAYTM service as it's payment gateway (pg). 

From that I duduced that the anamoly would be in any of the PAYTM gateways, but not in the subtype "UPI_PAY". After about 10 tries of plotting different combinations of categories which satisfied this criteria, the following was found. 


``` r
paytm_subset <- data[(data[,5] %in% c("PAYTM", "PAYTM_V2", "PAYTM_UPI", "notprovided")) & (data[,6] %in% c("UPI_COLLECT")) & (data[,4] == "UPI"),]
unique_hours <- unique(data$hr)
unique_hours <- sort(unique_hours)

t <- aggregate(paytm_subset$t,by=list(paytm_subset$hr),sum)
s <- aggregate(paytm_subset$s,by=list(paytm_subset$hr),sum)
f <- t[,2] - s[,2]

proportion <- f/t[,2] * 100
failed_transactions <- data.frame(hours = unique_hours, failedTransactions = proportion, x_index = seq(1, 72, by = 1))

ggplot(data = failed_transactions, aes(x = x_index, y = failedTransactions)) + geom_area(fill = "blue", alpha = 0.25) + 
  geom_line(color = "black") + scale_x_continuous(breaks = seq(1, 72, by = 6), minor_breaks = 1:72, 
  labels = unique_hours[seq(1, length(unique_hours), by = 6)]) + 
  coord_cartesian(ylim = range(failed_transactions$failedTransactions, na.rm = TRUE)) +  
  labs(title = "Failed Transactions Percentage by Hour", x = "Hour (72)", y = "Failed Transactions Per Hour") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8), axis.title.x = element_text(size = 10),
  axis.title.y = element_text(size = 10), plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA), panel.grid.major.x = element_blank(),  
  panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), legend.position = "none")
```
![failure_percent_isolated_Anamoly](https://github.com/user-attachments/assets/fc7f4ee5-5190-47cc-ba4b-4a1578035d1c)

This combination of UPI and PAYTM services produced a 80% failure rate from Febuary 13th 6PM to Febuary 14th 9AM. 
The combination being: (payment method: UPI, payment gateway: PAYTM or PAYTM_V2 or PAYTM_UPI, sub-type: UPI_COLLECT).


To see how the individual merchants were impacted, the failure rates before and during the anamoly we compared for each merchant. 

``` r
paytm_subset <- data[(data[,5] %in% c("PAYTM", "PAYTM_V2", "PAYTM_UPI", "notprovided")) & (data[,6] %in% c("UPI_COLLECT")) & (data[,4] == "UPI"),]
paytm_subset$f <- paytm_subset[,1] - paytm_subset[,2]
failure_sum_by_merchant <- aggregate(paytm_subset[,10],by=list(paytm_subset$mid),sum)
transaction_sum_by_merchant <- aggregate(paytm_subset[,1],by=list(paytm_subset$mid),sum)
failure_sum_by_merchant$transction_sum <- transaction_sum_by_merchant[,2]
failure_sum_by_merchant$failue_rate_merchant <- failure_sum_by_merchant[,2]/failure_sum_by_merchant[,3]
subset_failures_by_merchant <- failure_sum_by_merchant; data$failures <- data[,1] - data[,2]

rest_of_data_set_failure_count_by_merchant <- aggregate(data[,10],by=list(data$mid),sum)
rest_of_data_transaction_count_by_merchant <- aggregate(data[,1],by=list(data$mid),sum)
rest_of_data_transaction_count_by_merchant$failure_rate <- rest_of_data_set_failure_count_by_merchant[,2]/rest_of_data_transaction_count_by_merchant[,2]
entire_set_failures_by_merchant <- rest_of_data_transaction_count_by_merchant
entire_set_failures_by_merchant$subset_rate <- subset_failures_by_merchant[,4]

entire_set_failures_by_merchant$diff <- entire_set_failures_by_merchant[,4] - entire_set_failures_by_merchant[,3]
colnames(entire_set_failures_by_merchant) <- c("Merchant","Failures","Failure_rate_Pre","Failure_Rate_Anamoly", "Failure_Rate_Difference")
entire_set_failures_by_merchant <- entire_set_failures_by_merchant[c("Merchant","Failure_rate_Pre","Failure_Rate_Anamoly","Failure_Rate_Difference")]

library('reshape2')
long_set_failures_by_merchant <- melt(data = entire_set_failures_by_merchant, id.vars =c("Merchant"),
   measured.vars =c("Failure_rate_Before_Anamoly", "Failure_Rate_During_Anamoly","Difference_between_Failure_Rate"),
   variable.name = "Before_after_difference", value.name = "Rate")

ggplot(data = long_set_failures_by_merchant, 
  aes(x = Before_after_difference, y = Rate, fill = Merchant)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set2") +  
  labs(title = "Failure Rates Before and During Anomaly by Merchant",
  x = "Period", y = "Failure Rate") +theme_minimal()
```
![Merchants_effected_plot](https://github.com/user-attachments/assets/ff914a7b-960e-474c-b619-75ff7e291fa1)

Only one of the merchants was not not effected. 

``` r
library(gt); gt_table <- entire_set_failures_by_merchant %>% gt() %>%
tab_header(title = "Merchant Failure Rates", subtitle = "Comparison of Failure Rates Before and During Anomaly") %>%
cols_label(Merchant = "Merchant",Failure_rate_Pre = "Failure Rate Before Anomaly",
Failure_Rate_Anamoly = "Failure Rate During Anomaly",Failure_Rate_Difference = "Difference in Failure Rate") %>%
fmt_number(columns = c(Failure_rate_Pre, Failure_Rate_Anamoly, Failure_Rate_Difference), decimals = 4) %>%
tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels(everything())) %>%
tab_options(table.font.size = "small", table.width = pct(80), heading.align = "center") %>%
data_color(columns = Failure_Rate_Difference, colors = scales::col_numeric(palette = c("lightblue", "red"), domain = NULL)); gt_table
```

![Merchants_effected_table](https://github.com/user-attachments/assets/e1859ee5-558a-4ba7-9e58-e591e9e6718a)


Clearly, fan fight was effected the worst. 

Plotting the failure rates for the categories of observations in which the anamoly happened and the failure rates for the remainder of the data set where the anamoly is not clearly associated with. 

``` r
paytm_subset <- data[(data[,5] %in% c("PAYTM", "PAYTM_V2", "PAYTM_UPI", "notprovided")) & (data[,6] %in% c("UPI_COLLECT")) & (data[,4] == "UPI"),]
unique_hours <- unique(data$hr); unique_hours <- sort(unique_hours)

t <- aggregate(paytm_subset$t,by=list(paytm_subset$hr),sum)
s <- aggregate(paytm_subset$s,by=list(paytm_subset$hr),sum)
f <- t[,2] - s[,2]

proportion_subset <- f/t[,2] * 100
```


``` r
paytm_compliment <- data[!(rownames(data) %in% rownames(paytm_subset)), ]

t <- aggregate(paytm_compliment$t,by=list(paytm_compliment$hr),sum)
s <- aggregate(paytm_compliment$s,by=list(paytm_compliment$hr),sum)
f <- t[,2] - s[,2]

proportion_c <- f/t[,2] * 100
```


``` r
hours <- seq(1,72,1); wide <- as.data.frame(cbind(hours,proportion_c,proportion_subset))

long <- melt(data = wide, id.vars = c("hours"), measured.vars = c("proportion_c", "proportion_subset"), variable.name = "percentage_failure")

ggplot(data=long,aes(x=hours,y=value,group=percentage_failure,color=percentage_failure)) + geom_smooth() + labs(title="smoothed failure percentage curve") 
```

You can see the failure rate of the anamoly and un-effected data set are relatively close until the shift happens.

![failure_percent_Anamoly_vs_rest_smooth](https://github.com/user-attachments/assets/b8a4740b-e299-4a23-8312-b5bf514d2f74)


``` r
ggplot(data=long,aes(x=hours,y=value,group=percentage_failure,color=percentage_failure)) + geom_line() + labs(title="failure percentage line, Figure 1.0")
```
The variance of the anamoly curve is very high compaired to the un-effected data, but this is just from the anamoly only having a sample size of 500 and and the rest of the data having 19,000 samples. 

![failure_percent_Anamoly_vs_rest_line](https://github.com/user-attachments/assets/3937eec7-c4a9-49c7-b173-be8182863e40)


``` r
ggplot(data=wide,aes(proportion_subset)) + geom_density(fill="blue",alpha=0.20) + theme_minimal() + 
  labs(title = "Anomalous Data Failure Rate Density",xlab="Failure Rate",ylab="Density")
```

Then here is where we get to the bottom of it 

![Anamoly_failure_rate_density](https://github.com/user-attachments/assets/56dc7fba-82a6-44c1-bfc5-1e809f9a9248)


There are tons of plots that can be produced here but only one has a meaningful implication. 

Question: Could have this anomaly been predicted before it occurred? 

* The node on the left is the distribution of failure rates of the anomalous subset of data before and after the anomaly 
* The node on the right is the distribution of failure rates of the anomalous subset of data during the anomaly

Therefore, these are two distinct distributions representing two distinct processes. Something happened which caused this mean shift that could not have been predicted from the data before the anamoly. 









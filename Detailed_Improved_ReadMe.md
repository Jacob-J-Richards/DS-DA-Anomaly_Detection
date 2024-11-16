
# Transaction Anomaly Detection

This project investigates transaction anomalies based on data from the 48 hours leading up to February 14th. Customer complaints about failed transactions prompted an analysis to identify and explain the anomaly.

---

## Goal

Identify the root cause of transaction failures reported on February 14th, focusing on anomalies in transaction success rates and potential predictors.

---

## Data Overview

The dataset consists of transaction observations grouped by categorical variables. Each observation includes:
- Total transactions (`t`)
- Successful transactions (`s`)
- Hourly breakdown (`hr`)
- Associated categorical attributes (e.g., payment method, gateway).

### Initial Findings

Initial investigation involved plotting failed transaction rates and counts by hour. These plots showed no distinct patterns.

#### Failed Transaction Rate by Hour
```r
ggplot(data = failed_transactions_rate, aes(x = x_index, y = failedTransactions)) +
  geom_area(fill = "blue", alpha = 0.25) +
  geom_line(color = "black") +
  labs(title = "Failed Transactions Percentage by Hour", x = "Hour (72)", y = "Failed Transactions Per Hour")
```
![Failed Transactions Percentage](https://github.com/user-attachments/assets/810910d9-7b10-42ac-ae60-197fc0089c2c)

#### Failed Transaction Count by Hour
```r
ggplot(data = failed_transactions, aes(x = x_index, y = failedTransactions)) +
  geom_area(fill = "blue", alpha = 0.25) +
  geom_line(color = "black") +
  labs(title = "Failed Transactions Counts by Hour", x = "Hour (72)", y = "Failed Transactions Per Hour")
```
![Failed Transactions Counts](https://github.com/user-attachments/assets/d9a09a24-65ab-4b86-b166-1573b55a585a)

---

## Methodology

### Weighted Failure Rate
A weighted failure rate was calculated for each observation using:
```r
weighted_failure_rate <- (data$t - data$s) / data$t * log(1 + data$t)
```
This approach assigns higher significance to observations with more transactions.

### Anomaly Detection
Anomalies were identified using Mahalanobis distances. Observations with the highest distances were flagged as anomalous.

```r
mahalanobis_distances <- mahalanobis(features, center, cov_matrix)
```

---

## Results

### Anomalous Observations
The anomaly detection algorithm flagged observations involving:
- **Payment Gateway:** PAYTM (or variants like PAYTM_V2, PAYTM_UPI)
- **Subtype:** UPI_COLLECT
- **Payment Method:** UPI

#### Key Finding
These transactions exhibited an 80% failure rate between **February 13th, 6 PM** and **February 14th, 9 AM**.

```r
paytm_subset <- data[(data$pmt == "PAYTM") & (data$subtype == "UPI_COLLECT"), ]
```
![Anomalous Observations](https://github.com/user-attachments/assets/fc7f4ee5-5190-47cc-ba4b-4a1578035d1c)

### Merchant Impact
Failure rates were compared before and during the anomaly for each merchant.

#### Visualization
```r
ggplot(data = long_set_failures_by_merchant, aes(x = Before_after_difference, y = Rate, fill = Merchant)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Failure Rates Before and During Anomaly by Merchant")
```
![Merchant Impact](https://github.com/user-attachments/assets/ff914a7b-960e-474c-b619-75ff7e291fa1)

#### Observation
One merchant was unaffected, while **Fan Fight** was the most severely impacted.

---

## Insights and Visualizations

1. **Anomaly vs. Non-Anomalous Data**
   Plots highlighted distinct distributions between anomalous and non-anomalous data.

   ```r
   ggplot(data=long,aes(x=hours,y=value,group=percentage_failure,color=percentage_failure)) + geom_smooth()
   ```
   ![Comparison](https://github.com/user-attachments/assets/b8a4740b-e299-4a23-8312-b5bf514d2f74)

2. **Variance and Density**
   Despite higher variance in the anomaly subset, the density plot revealed a clear mean shift.

   ```r
   ggplot(data=wide,aes(proportion_subset)) + geom_density(fill="blue",alpha=0.20)
   ```
   ![Density](https://github.com/user-attachments/assets/56dc7fba-82a6-44c1-bfc5-1e809f9a9248)

---

## Key Question

Could this anomaly have been predicted before it occurred?

### Findings
- The anomaly resulted from a distinct mean shift in failure rate distributions.
- Pre-anomaly data did not indicate this shift, suggesting external factors caused the anomaly.

---

## Conclusion

This project demonstrates the value of anomaly detection in transactional data. While the anomaly's root cause was identified, its occurrence was unpredictable.

---

## How to Run

1. Clone the repository and load the dataset.
2. Install required R packages: `ggplot2`, `reshape2`, `gt`.
3. Execute the R scripts to reproduce the analysis and visualizations.

---

## Contact

For questions or suggestions, reach out to [Jacob Richards](mailto:your_email@example.com).

---

## License

This project is licensed under the MIT License.

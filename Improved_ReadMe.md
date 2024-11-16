
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

**Preliminary Investigation:** Failure rates were plotted by hour, revealing no distinct patterns before anomaly detection methods were applied.

![Initial Investigation](https://github.com/user-attachments/assets/810910d9-7b10-42ac-ae60-197fc0089c2c)

---

## Methodology

1. **Weighted Failure Rate Calculation:** Each observation’s failure rate was weighted by `log(1 + transactions)` to give greater significance to larger transaction volumes.
2. **Anomaly Detection:** Mahalanobis distances were calculated to identify outliers.
3. **Subsetting:** Filtered data with the highest weighted failure rates were analyzed.

---

## Results

### Anomalous Observations
The anomaly detection algorithm identified observations with the following characteristics:
- **Payment Gateway:** PAYTM (or related services like PAYTM_V2, PAYTM_UPI)
- **Subtype:** UPI_COLLECT
- **Payment Method:** UPI

These observations exhibited a failure rate of 80% between **February 13th, 6 PM** and **February 14th, 9 AM**.

![Anomalous Observations](https://github.com/user-attachments/assets/fc7f4ee5-5190-47cc-ba4b-4a1578035d1c)

### Merchant Impact
Failure rates before and during the anomaly were compared for each merchant:

![Merchant Impact](https://github.com/user-attachments/assets/ff914a7b-960e-474c-b619-75ff7e291fa1)

Only one merchant remained unaffected, with the most severe impact observed for **Fan Fight**.

---

## Insights and Visualization

1. **Anomaly vs. Non-Anomalous Data:** Plots compared failure rates in the anomalous subset and the remaining dataset, highlighting distinct failure rate distributions.

   ![Comparison](https://github.com/user-attachments/assets/b8a4740b-e299-4a23-8312-b5bf514d2f74)

2. **Variance and Density:** Despite higher variance in the anomaly subset due to smaller sample size, the failure rate density plot reveals a significant mean shift.

   ![Density](https://github.com/user-attachments/assets/56dc7fba-82a6-44c1-bfc5-1e809f9a9248)

---

## Key Question

Could this anomaly have been predicted before it occurred?

### Findings
- The anomaly was caused by a distinct change in the failure rate distribution.
- Pre-anomaly data did not provide predictive signals of the observed shift, suggesting the anomaly arose from external factors.

---

## Conclusion

This analysis highlights the importance of anomaly detection in transactional data. While the anomaly was detected and its root cause identified, its occurrence was unpredictable from prior data.

---

## How to Run

1. Clone the repository and load the dataset.
2. Install required R packages: `ggplot2`, `reshape2`, `gt`.
3. Run the provided R scripts for data preprocessing, anomaly detection, and visualization.

---

## Contact

For questions or suggestions, please reach out to [Jacob Richards](mailto:your_email@example.com).

---

## License

This project is licensed under the MIT License.

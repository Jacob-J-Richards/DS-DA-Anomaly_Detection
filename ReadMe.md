---
title: "Anamoly Detection"
output:
  html_document: 
    keep_md: true
  pdf_document: default
geometry: margin=0.10in
date: "2024-11-13"
---


``` r
library('ggplot2')
setwd("/Users/jacobrichards/Desktop/DS assesment/DS_exam_2")
#setwd("C:/Users/jake pc/Desktop/exam_2_restart")
transactions <- read.csv(file="trans.csv", na.strings = c("", "NA"))
transactions[is.na(transactions)] <- "notprovided"
data <- transactions
colnames(data) <- c("t","s","mid","pmt","pg","subtype","hr","bank")
weighted_failure_rate <- numeric(nrow(data))
weighted_failure_rate <- data[,1] - data[,2] / data[,1] * log(1+data[,1])
data$weighted <- weighted_failure_rate; data_original <- data 
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

![](Exam_2_restart_Nov_16_636AM_files/figure-html/unnamed-chunk-3-1.png)<!-- -->



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

![](Exam_2_restart_Nov_16_636AM_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

``` r
ggsave("806.png", plot = last_plot(), width = 10, height = 6, dpi = 300)
```



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

```
##         t    s      mid    pmt        pg             subtype            hr
## 3872 3365 2524 fanfight    UPI PAYTM_UPI             UPI_PAY 2020-02-14 15
## 4467 3340 2462 fanfight    UPI PAYTM_UPI             UPI_PAY 2020-02-12 15
## 4170 1391 1086 fanfight    UPI PAYTM_UPI             UPI_PAY 2020-02-12 02
## 4134 1357  918 fanfight    UPI PAYTM_UPI             UPI_PAY 2020-02-14 14
## 4889 1230 1001 fanfight WALLET     PAYTM DIRECT_WALLET_DEBIT 2020-02-14 15
## 4970 1197  952 fanfight WALLET     PAYTM DIRECT_WALLET_DEBIT 2020-02-12 15
## 3944 1258  767 fanfight    UPI PAYTM_UPI             UPI_PAY 2020-02-12 14
## 3838 1156  884 fanfight    UPI PAYTM_UPI             UPI_PAY 2020-02-14 11
## 3866 1078  754 fanfight    UPI PAYTM_UPI             UPI_PAY 2020-02-14 13
## 3910 1025  639 fanfight    UPI PAYTM_UPI             UPI_PAY 2020-02-12 13
## 4544  794  417 fanfight    UPI PAYTM_UPI             UPI_PAY 2020-02-13 14
## 3914  722  470 fanfight    UPI PAYTM_UPI             UPI_PAY 2020-02-12 12
## 4369  658  484 fanfight    UPI PAYTM_UPI             UPI_PAY 2020-02-14 07
## 4247  597  373 fanfight    UPI PAYTM_UPI             UPI_PAY 2020-02-13 13
## 4944  501  395 fanfight WALLET     PAYTM DIRECT_WALLET_DEBIT 2020-02-12 02
## 4862  478  396 fanfight WALLET     PAYTM DIRECT_WALLET_DEBIT 2020-02-14 11
## 4169  550  365 fanfight    UPI PAYTM_UPI             UPI_PAY 2020-02-12 11
## 3908  538  359 fanfight    UPI PAYTM_UPI             UPI_PAY 2020-02-12 07
## 4168  496  379 fanfight    UPI PAYTM_UPI             UPI_PAY 2020-02-12 03
## 3912  480  332 fanfight    UPI PAYTM_UPI             UPI_PAY 2020-02-12 06
##       bank  weighted
## 3872   UPI 3358.9083
## 4467   UPI 3334.0189
## 4170   UPI 1385.3487
## 4134   UPI 1352.1199
## 4889 PAYTM 1224.2092
## 4970 PAYTM 1191.3624
## 3944   UPI 1253.6479
## 3838   UPI 1150.6061
## 3866   UPI 1073.1152
## 3910   UPI 1020.6776
## 4544   UPI  790.4926
## 3914   UPI  717.7144
## 4369   UPI  653.2257
## 4247   UPI  593.0053
## 4944 PAYTM  496.0971
## 4862 PAYTM  472.8870
## 4169   UPI  545.8113
## 3908   UPI  533.8030
## 4168   UPI  491.2559
## 3912   UPI  475.7284
```



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

![](Exam_2_restart_Nov_16_636AM_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

``` r
  ggsave("failure_percent_by_category_combination.png", plot = last_plot(), width = 10, height = 6, dpi = 300)
```



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

![](Exam_2_restart_Nov_16_636AM_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

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

```
## Warning: Since gt v0.9.0, the `colors` argument has been deprecated.
## • Please use the `fn` argument instead.
## This warning is displayed once every 8 hours.
```

```{=html}
<div id="stwhghyepj" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#stwhghyepj table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#stwhghyepj thead, #stwhghyepj tbody, #stwhghyepj tfoot, #stwhghyepj tr, #stwhghyepj td, #stwhghyepj th {
  border-style: none;
}

#stwhghyepj p {
  margin: 0;
  padding: 0;
}

#stwhghyepj .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: small;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: 80%;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#stwhghyepj .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#stwhghyepj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#stwhghyepj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#stwhghyepj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#stwhghyepj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#stwhghyepj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#stwhghyepj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#stwhghyepj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#stwhghyepj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#stwhghyepj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#stwhghyepj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#stwhghyepj .gt_spanner_row {
  border-bottom-style: hidden;
}

#stwhghyepj .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#stwhghyepj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#stwhghyepj .gt_from_md > :first-child {
  margin-top: 0;
}

#stwhghyepj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#stwhghyepj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#stwhghyepj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#stwhghyepj .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#stwhghyepj .gt_row_group_first td {
  border-top-width: 2px;
}

#stwhghyepj .gt_row_group_first th {
  border-top-width: 2px;
}

#stwhghyepj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#stwhghyepj .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#stwhghyepj .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#stwhghyepj .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#stwhghyepj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#stwhghyepj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#stwhghyepj .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#stwhghyepj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#stwhghyepj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#stwhghyepj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#stwhghyepj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#stwhghyepj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#stwhghyepj .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#stwhghyepj .gt_left {
  text-align: left;
}

#stwhghyepj .gt_center {
  text-align: center;
}

#stwhghyepj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#stwhghyepj .gt_font_normal {
  font-weight: normal;
}

#stwhghyepj .gt_font_bold {
  font-weight: bold;
}

#stwhghyepj .gt_font_italic {
  font-style: italic;
}

#stwhghyepj .gt_super {
  font-size: 65%;
}

#stwhghyepj .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#stwhghyepj .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#stwhghyepj .gt_indent_1 {
  text-indent: 5px;
}

#stwhghyepj .gt_indent_2 {
  text-indent: 10px;
}

#stwhghyepj .gt_indent_3 {
  text-indent: 15px;
}

#stwhghyepj .gt_indent_4 {
  text-indent: 20px;
}

#stwhghyepj .gt_indent_5 {
  text-indent: 25px;
}

#stwhghyepj .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#stwhghyepj div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_title gt_font_normal" style>Merchant Failure Rates</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Comparison of Failure Rates Before and During Anomaly</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="Merchant">Merchant</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="Failure_rate_Pre">Failure Rate Before Anomaly</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="Failure_Rate_Anamoly">Failure Rate During Anomaly</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="Failure_Rate_Difference">Difference in Failure Rate</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Merchant" class="gt_row gt_left">countrydelight</td>
<td headers="Failure_rate_Pre" class="gt_row gt_right">0.2527</td>
<td headers="Failure_Rate_Anamoly" class="gt_row gt_right">0.3793</td>
<td headers="Failure_Rate_Difference" class="gt_row gt_right" style="background-color: #F6765A; color: #FFFFFF;">0.1266</td></tr>
    <tr><td headers="Merchant" class="gt_row gt_left">drivezy</td>
<td headers="Failure_rate_Pre" class="gt_row gt_right">0.4115</td>
<td headers="Failure_Rate_Anamoly" class="gt_row gt_right">0.4912</td>
<td headers="Failure_Rate_Difference" class="gt_row gt_right" style="background-color: #ED907A; color: #000000;">0.0797</td></tr>
    <tr><td headers="Merchant" class="gt_row gt_left">fanfight</td>
<td headers="Failure_rate_Pre" class="gt_row gt_right">0.3422</td>
<td headers="Failure_Rate_Anamoly" class="gt_row gt_right">0.5831</td>
<td headers="Failure_Rate_Difference" class="gt_row gt_right" style="background-color: #FF0000; color: #FFFFFF;">0.2409</td></tr>
    <tr><td headers="Merchant" class="gt_row gt_left">medlife_prod</td>
<td headers="Failure_rate_Pre" class="gt_row gt_right">0.3589</td>
<td headers="Failure_Rate_Anamoly" class="gt_row gt_right">0.3968</td>
<td headers="Failure_Rate_Difference" class="gt_row gt_right" style="background-color: #E2A597; color: #000000;">0.0379</td></tr>
    <tr><td headers="Merchant" class="gt_row gt_left">pharmeasytech</td>
<td headers="Failure_rate_Pre" class="gt_row gt_right">0.3290</td>
<td headers="Failure_Rate_Anamoly" class="gt_row gt_right">0.4622</td>
<td headers="Failure_Rate_Difference" class="gt_row gt_right" style="background-color: #F77356; color: #FFFFFF;">0.1332</td></tr>
    <tr><td headers="Merchant" class="gt_row gt_left">purplle.com</td>
<td headers="Failure_rate_Pre" class="gt_row gt_right">0.4135</td>
<td headers="Failure_Rate_Anamoly" class="gt_row gt_right">0.5192</td>
<td headers="Failure_Rate_Difference" class="gt_row gt_right" style="background-color: #F28269; color: #FFFFFF;">0.1057</td></tr>
    <tr><td headers="Merchant" class="gt_row gt_left">urbanclap</td>
<td headers="Failure_rate_Pre" class="gt_row gt_right">0.3156</td>
<td headers="Failure_Rate_Anamoly" class="gt_row gt_right">0.2446</td>
<td headers="Failure_Rate_Difference" class="gt_row gt_right" style="background-color: #ADD8E6; color: #000000;">−0.0710</td></tr>
    <tr><td headers="Merchant" class="gt_row gt_left">zivame</td>
<td headers="Failure_rate_Pre" class="gt_row gt_right">0.3604</td>
<td headers="Failure_Rate_Anamoly" class="gt_row gt_right">0.4149</td>
<td headers="Failure_Rate_Difference" class="gt_row gt_right" style="background-color: #E79D8C; color: #000000;">0.0545</td></tr>
  </tbody>
  
  
</table>
</div>
```

All of the merchants were effected except UrbanClap. 


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

```
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

![](Exam_2_restart_Nov_16_636AM_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

``` r
ggplot(data=long,aes(x=hours,y=value,group=percentage_failure,color=percentage_failure)) + geom_line() + labs(title="failure percentage line, Figure 1.0")
```

![](Exam_2_restart_Nov_16_636AM_files/figure-html/unnamed-chunk-11-2.png)<!-- -->



``` r
ggplot(data=wide,aes(proportion_subset)) + geom_density(fill="blue",alpha=0.20) + theme_minimal() + 
  labs(title = "Anomalous Data Failure Rate Density",xlab="Failure Rate",ylab="Density")
```

![](Exam_2_restart_Nov_16_636AM_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


There are tons of plots that can be produced here but only one has a meaningful implication. 

Question: Could have this anomaly been predicted before it occurred? 

1.) The node on the left is the distribution of failure rates of the anomalous subset of data before and after the anomaly 

2.) The node on the right is the distribution of failure rates of the anomalous subset of data during the anomaly

3.) these are two district distributions representing two distinct processes.

Therefore, real life process of which this data set is a collection of measurements on changed around February 13th 6PM and reverted back to it's original state around February 14th 9AM.


``` r
Note for the future: no one helped you with this, you figured this out on your own. The only clue you had was you saw Figure 1.0 produced by someone else on discord with no indication of where it came from other than it was the result of the combination of categorical variables. 
```







# Univariate Nonparametric SPC (npSPC)

This repository shows the material used in the article "Nonparametric SPC: solving the electrical monitoring of a mining company with low-computational cost". The R script adopted to generate the analysis is presented in <npSPC.R> file, as well as the three datasets adopted in Section 2 <DB1.csv, DB2.csv, DB3.csv> and the real-world dataset <TAB_final.csv> from the Mining Company, placed in Atacama (Chile), related to the monthly amount of interventions on two types of electrical boards (TDF and TDF/SM).

Package available at: https://github.com/CER-UFBA/npSPC or Installing in R-software as:
> remotes::install_github('CER-UFBA/npSPC')

It contains 12 different functions for signs or signed-ranks nonparametric control charts (Shewhart, CuSum, and EWMA); six of them are related to the case of known in-control parameters and another six to the case of unknown parameters. Here called the functions considering the known parameter of the specified tolerant interval, Case K, and
unknown parameter of in-control, Case U.

| Command | Description |
| --- | --- |
| **Case K** | --- |
| `shewhart_sn` | Shewhart based on signs                                    
| `shewhart_sr` | Shewhart based on signed-ranks |
| `cusum_sn` | Cumulative Sum based on signs |
| `cusum_sr` | Cumulative Sum based on signed-ranks |
| `ewma_sn` | Exponentially Weighted Moving Average based on signs |
| `ewma_sr` | Exponentially Weighted Moving Average based on signed-ranks |
| **Case U** | --- |
| `shewhart_prec` | Shewhart based on precedence statistic |
| `shewhart_mw` | Shewhart based on Mann-Whitney statistic |
| `cusum_ex` | Cumulative Sum based on exceedance statistic |
| `cusum_wr` | Cumulative Sum based on Wilcoxon rank-sum statistic |
| `ewma_ex` | Exponentially Weighted Moving Average based on exceedance statistic |
| `ewma_wr` | Exponentially Weighted Moving Average based on Wilcoxon rank-sum statistic |

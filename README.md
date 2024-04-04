# Univariate Nonparametric SPC (npSPC)

This repository shows the material used in the article "Nonparametric SPC: solving the electrical monitoring of a mining company with low-computational cost". The R script adopted to generate the analysis is presented in <npSPC.R> file, as well as the three datasets adopted in Section 2 \[DB1.csv, DB2.csv, DB3.csv\] and the real-world dataset from the Mining Company, placed in Atacama (Chile), related to the monthly amount of interventions on two types of electrical boards (TDF and TDF/SM).

Package Avaible at: \url{https://github.com/CER-UFBA/npSPC} or Installing in R as:
> remotes::install_github('CER-UFBA/npSPC')

It contains 12 different functions for signs or signed-ranks nonparametric control charts (Shewhart, CuSum, and EWMA); six of them are related to the case of known in-control parameters and another six to the case of unknown parameters.

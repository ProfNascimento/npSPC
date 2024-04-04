require(qcc)
require(npSPC)

#### PART 1 - SECTION 2 EXAMPLES 
## DATASET 1
data1 = read.csv("https://raw.githubusercontent.com/ProfNascimento/npSPC/main/DB1.csv")
DB1 = qcc.groups(data=data1$value,data1$sample)

# PARAMETRIC SPC
# X-Bar
XbarChart = qcc(DB1,type = "xbar")
# CuSUM
CuSumChart <- cusum(DB1)
summary(CuSumChart)
# EWMA
EWMAChart <- ewma(DB1, lambda=0.2, nsigmas=3)
summary(EWMAChart)

# NONPARAMETRIC SPC
# CUSUM-SN
cusum_sn(DB1,group_by_col = TRUE)
# CUSUM-SR
cusum_sr(DB1,group_by_col = TRUE, h=8, k=3)


## DATASET 2
data2 = read.csv("https://raw.githubusercontent.com/ProfNascimento/npSPC/main/DB2.csv")
DB2 = qcc.groups(data=data2$value,data2$sample)

# PARAMETRIC SPC
# X-Bar
XbarChart2 = qcc(DB2,type = "xbar")
# CuSUM
CuSumChart2 <- cusum(DB2)
summary(CuSumChart2)
# EWMA
EWMAChart2 <- ewma(DB2, lambda=0.2, nsigmas=3)
summary(EWMAChart2)

# NONPARAMETRIC SPC
# EWMA-SN
ewma_sn(DB2,group_by_col = TRUE, lambda = 0.05, L=2.612)
# EWMA-EX
ewma_ex(DB2[,1:5],DB2[,6:10], group_by_col = TRUE, lambda = 0.05, L=2.612)


## DATASET 3
data3 = read.csv("https://raw.githubusercontent.com/ProfNascimento/npSPC/main/DB3.csv")
DB3 = qcc.groups(data=data3$value,data3$sample)

# PARAMETRIC SPC
# X-Bar
XbarChart3 = qcc(DB3,type = "xbar")
# CuSUM
CuSumChart3 <- cusum(DB3)
summary(CuSumChart3)
# EWMA
EWMAChart3 <- ewma(DB3, lambda=0.2, nsigmas=3)
summary(EWMAChart3)

# NONPARAMETRIC SPC
# SHEWHART-SN
shewhart_sn(DB3,group_by_col = TRUE)
# SHEWHART-SR
shewhart_sr(DB3,group_by_col = TRUE, mu0=0.5, exact = TRUE)


################################################################
#### PART 2 - SECTION 3 
## DATA ELECTRICAL BOARD REMOVED - GROUP "TDF / SM" v/s "TDF" ## 
data_long <- read.csv("https://raw.githubusercontent.com/ProfNascimento/npSPC/main/TAB_final.csv")
names(data_long)

## DESCRIPTIVE TDF/SM
tapply(data_long$Y[data_long$Removed=="1" & data_long$Type=="TDF / SM "], 
  data_long$Month[data_long$Removed=="1"& data_long$Type=="TDF / SM "],summary)

boxplot(
  data_long$Y[data_long$Removed=="1" & data_long$Type=="TDF / SM "] ~ 
  data_long$Month[data_long$Removed=="1"& data_long$Type=="TDF / SM "],
  ylab="Removed Itens", xlab="Time (Month)",main="TDF/SM Board Type"
)

plot(tapply(data_long$Y[data_long$Removed=="1" & data_long$Type=="TDF / SM "], 
            data_long$Time[data_long$Removed=="1"& data_long$Type=="TDF / SM "], median),
     ylab="Median Removed Itens", xlab="Time (Month)",main="TDF/SM Board Type")

## DESCRIPTIVE TDF
tapply(data_long$Y[data_long$Removed=="1" & data_long$Type=="TDF"], 
       data_long$Month[data_long$Removed=="1"& data_long$Type=="TDF"],summary)

boxplot(
  data_long$Y[data_long$Removed=="1" & data_long$Type=="TDF"] ~ 
    data_long$Month[data_long$Removed=="1"& data_long$Type=="TDF"],
  ylab="Removed Itens", xlab="Time (Month)",main="TDF Board Type"
)

plot(tapply(data_long$Y[data_long$Removed=="1" & data_long$Type=="TDF"], 
            data_long$Time[data_long$Removed=="1"& data_long$Type=="TDF"], median),
     ylab="Median Removed Itens", xlab="Time (Month)",main="TDF Board Type")

d.TDF = data_long[data_long$Removed=="1" & data_long$Type=="TDF / SM ",c(2,4,5)]
DB.d1 = qcc.groups(data=d.TDF$Y,d.TDF$Time)

d2.TDF = data_long[data_long$Removed=="1" & data_long$Type=="TDF",c(2,4,5)]
DB.d2 = qcc.groups(data=d2.TDF$Y,d2.TDF$Time)

## MONITORING TOOLS
# PARAMETRIC SPC
qcc(DB.d1,type = "xbar")
qcc(DB.d2,type = "xbar")
# CuSUM
cusum(DB.d1)
cusum(DB.d2)
# EWMA
ewma(DB.d1, lambda=1, nsigmas=3)
ewma(DB.d2, lambda=1, nsigmas=3)

ewma_sn(DB.d1, group_by_col = TRUE)
ewma_sn(DB.d2, group_by_col = TRUE)
ewma_ex(DB.d1,DB.d2, group_by_col = TRUE)

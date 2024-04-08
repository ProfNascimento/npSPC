require(qcc)
require(npSPC)

#### PART 1 - SECTION 2 EXAMPLES 
## DATASET 1
data1 = read.csv("https://raw.githubusercontent.com/ProfNascimento/npSPC/main/DB1.csv")
DB1 = qcc.groups(data=data1$value,data1$sample)

# VISUALIZATION
p11=ggplot(data1, aes(x=value)) + geom_histogram() + coord_flip() + 
  scale_y_reverse()  + theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())

p12=ggplot(data1,aes(x=as.factor(sample),y=value)) + 
  geom_violin(alpha=0.6) + xlab("Sample (unit)") + 
  ylab("Diff(Target Material, Observed Sampled)") +
  stat_summary(fun = "median",col="red")

cowplot::plot_grid(p11,p12,rel_heights = c(2,1),
                   rel_widths = c(1, 3))

qcc.options(bg.margin = "white")
# PARAMETRIC SPC
# Shewhart's (X-Bar and Range)
XbarChart = qcc(DB1,type = "xbar")
RChart = qcc(DB1,type = "S")
# CuSUM
CuSumChart <- cusum(DB1)
summary(CuSumChart)
# EWMA
EWMAChart <- ewma(DB1, lambda=0.2, nsigmas=3)
summary(EWMAChart)

# NONPARAMETRIC SPC
# CUSUM-SN
cusum_sn(DB1,group_by_col = TRUE)
sort(abs(apply(DB1,1,median)[-1]+diff(apply(DB1,1,IQR))))

# CUSUM-SR
cusum_sr(DB1,group_by_col = TRUE, h=8, k=3)


## DATASET 2
data2 = read.csv("https://raw.githubusercontent.com/ProfNascimento/npSPC/main/DB2.csv")
DB2 = qcc.groups(data=data2$value,data2$sample)

# VISUALIZATION
p21=ggplot(data2, aes(x=value)) + geom_histogram() + coord_flip() + 
  scale_y_reverse() + theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())

p22=ggplot(data2,aes(x=as.factor(sample),y=value)) + 
  geom_violin(alpha=0.6) + xlab("Sample (unit)") + 
  ylab("Diff(radiometric temperature, laboratory combustion)") +
  stat_summary(fun = "median",col="red")

cowplot::plot_grid(p21,p22,rel_heights = c(2,1),
                   rel_widths = c(1, 3))

# MEAN REPRESETATION V/S MEDIAN
ggplot(data2,aes(x=sample,y=value)) + 
  geom_jitter(width = 0.1,alpha=0.6) + xlab("Sample (unit)") + 
  ylab("Diff(Radiometric temperature, Laboratory combustion)") +
  stat_summary(fun = "median",col="red", geom="line")+
  stat_summary(fun = "mean",col="black", geom="line")+
  scale_x_continuous(breaks =1:15 )

# PARAMETRIC SPC
# Shewhart's (X-Bar and Range)
XbarChart2 = qcc(DB2,type = "xbar")
RChart2 = qcc(DB2,type = "S")
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

# VISUALIZATION
p31=ggplot(data3, aes(x=value)) + geom_histogram() + coord_flip() + 
  scale_y_reverse() + theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())

p32=ggplot(data3,aes(x=as.factor(sample),y=value)) + 
  geom_violin(alpha=0.6) + xlab("Sample (unit)") + 
  ylab("Life time durability (unit)") +
  stat_summary(fun = "median",col="red")

cowplot::plot_grid(p31,p32,rel_heights = c(2,1),
                   rel_widths = c(1, 3))

# PARAMETRIC SPC
# Shewhart's (X-Bar and Range)
XbarChart3 = qcc(DB3, type = "xbar", center = 0.5)
RChart3 = qcc(DB3, type = "S", center = 0.5)
# CuSUM
CuSumChart3 <- cusum(DB3, center = 0.5)
summary(CuSumChart3)
# EWMA
EWMAChart3 <- ewma(DB3, center = 0.5, lambda=0.2, nsigmas=3)
summary(EWMAChart3)

# NONPARAMETRIC SPC
# SHEWHART-SN
shewhart_sn(DB3, med=0.5, group_by_col = TRUE)
# SHEWHART-SR
shewhart_sr(DB3, group_by_col = TRUE, mu0=0.5, exact = TRUE)


################################################################
#### PART 2 - SECTION 3 
## DATA ELECTRICAL BOARD REMOVED - GROUP "TDF / SM" v/s "TDF" ## 
data_long <- read.csv("https://raw.githubusercontent.com/ProfNascimento/npSPC/main/TAB_final.csv")
names(data_long)

unique(paste(data_long$Type,data_long$ID,sep="."))

summary(as.factor(data_long$Removed[data_long$Type=="TDF"]))
summary(as.factor(data_long$Removed[data_long$Type=="TDF / SM "]))

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
     ylab="Median Removed Itens", xlab="Time (Month)",main="TDF/SM Board Type",type="b")

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
     ylab="Median Removed Itens", xlab="Time (Month)",main="TDF Board Type",type="b")

# stacked area chart
ggplot(aaa1, aes(x=time, y=value, fill=type)) + 
  geom_area(position ="identity")+xlab("Time (month)")+ylab("Median Itens") + 
  scale_x_continuous(breaks=seq(0, 53, 6)) +
  scale_y_continuous(breaks=seq(0, 2, 1)) +
  geom_vline(xintercept=12, linetype="dashed")+
  geom_vline(xintercept=24, linetype="dashed")+
  geom_vline(xintercept=36, linetype="dashed")+
  geom_vline(xintercept=48, linetype="dashed")

d.TDF = data_long[data_long$Removed=="1" & data_long$Type=="TDF / SM ",c(2,4,5)]
DB.d1 = qcc.groups(data=d.TDF$Y,d.TDF$Time)

d2.TDF = data_long[data_long$Removed=="1" & data_long$Type=="TDF",c(2,4,5)]
DB.d2 = qcc.groups(data=d2.TDF$Y,d2.TDF$Time)

## Comparing MEAN and MEDIAN between groups
t.test(d.TDF$Y,d2.TDF$Y)
wilcox.test(d.TDF$Y,d2.TDF$Y)

## MONITORING TOOLS
# PARAMETRIC SPC
qcc(DB.d1,type = "xbar",center=0)
qcc(DB.d2,type = "xbar",center=0)
# Range
qcc(DB.d1,type = "S",center=0)
qcc(DB.d2,type = "S",center=0)
# CuSUM
cusum(DB.d1,center=0)
cusum(DB.d2,center=0)
# EWMA
ewma(DB.d1,center=0)
ewma(DB.d2,center=0)

## Nonparametric SPC (CASE K)
# SHEWHART-SN
shewhart_sn(DB.d1, med=0, group_by_col = TRUE)
shewhart_sn(DB.d2, med=0, group_by_col = TRUE)
# EWMA-SN
ewma_sn(DB.d1, group_by_col = TRUE,med = 0)
ewma_sn(DB.d2, group_by_col = TRUE,med = 0)
# CUSUM-SN
cusum_sn(DB.d1,group_by_col = TRUE, med=0)
cusum_sn(DB.d2,group_by_col = TRUE, med=0)

## Nonparametric SPC (CASE U)
# CUSUM NOT INDICATED GIVEN THE LAST YEAR PATTERN SHIFTING THE MEAN & MEDIAN
# IF THE CUSUM IS NOT INDICATED THEN SHEWHART WILL NOT BE 
# UNKNOWN PARAMETER CASE USES THE NOT-REMOVED ITEM AS REFERENCE
d11.TDF = data_long[data_long$Removed!="1" & data_long$Type=="TDF / SM ",c(2,4,5)]
DB.d11 = qcc.groups(data=d11.TDF$Y,d11.TDF$Time)
ewma_wr(DB.d11,DB.d1, group_by_col = TRUE)

d21.TDF = data_long[data_long$Removed!="1" & data_long$Type=="TDF",c(2,4,5)]
DB.d21 = qcc.groups(data=d21.TDF$Y,d21.TDF$Time)
ewma_wr(DB.d21,DB.d2, group_by_col = TRUE)

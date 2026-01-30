# Load necessary libraries
require(tidyverse)
library(zoo)
library(qcc)
require(npSPC)

################################################################
#### PART 2 - SECTION 3 
## DATA ELECTRICAL BOARD MAINTENANCE - [GROUP "TDF / SM" v/s "TDF"] ## 
data_long <- read.csv("https://raw.githubusercontent.com/ProfNascimento/npSPC/main/TAB_final.csv")
head(data_long)

#####################################
## TOTAL MAINTENANCE - DESCRIPTIVE 
unique(paste(data_long$Type,data_long$ID,sep="."))

tapply(as.factor(data_long$Y[data_long$Type=="TDF"]),data_long$Year[data_long$Type=="TDF"],summary)
tapply(as.factor(data_long$Y[data_long$Type=="TDF / SM "]),data_long$Year[data_long$Type=="TDF / SM "],summary)

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

# boxplot(
#   data_long$Y[data_long$Removed=="1" & data_long$Type=="TDF"] ~ 
#     data_long$Month[data_long$Removed=="1"& data_long$Type=="TDF"],
#   ylab="Removed Itens", xlab="Time (Month)",main="TDF Board Type"
# )

# plot(tapply(data_long$Y[data_long$Removed=="1" & data_long$Type=="TDF"], 
#             data_long$Time[data_long$Removed=="1"& data_long$Type=="TDF"], median),
#      ylab="Median Removed Itens", xlab="Time (Month)",main="TDF Board Type",type="b")

# MEDIAN REMOVED-ITEM (monthly) --stacked area chart--
data_long %>% filter(Removed=="1") %>% group_by(Type, Month, Year) %>% mutate(Total=median(Y)) %>% 
  ggplot(aes(x=Time, y=Total, fill=Type)) + 
  geom_area(position ="identity") + 
  xlab("Time (month)") + ylab("Median Itens")

d.TDF = data_long[data_long$Removed=="1" & data_long$Type=="TDF / SM ",c(2,4,5)]
DB.d1 = qcc.groups(data=d.TDF$Y,d.TDF$Time)

d2.TDF = data_long[data_long$Removed=="1" & data_long$Type=="TDF",c(2,4,5)]
DB.d2 = qcc.groups(data=d2.TDF$Y,d2.TDF$Time)

## Comparing MEAN and MEDIAN between groups
t.test(d.TDF$Y,d2.TDF$Y)
wilcox.test(d.TDF$Y,d2.TDF$Y)

## Maintenance Dynamic PLOT  
# Convert Month/Year to a Date object
# Mapping Spanish months to numbers if your system locale is English
month_map <- c("ENE"=1, "FEB"=2, "MAR"=3, "ABR"=4, "MAY"=5, "JUN"=6, 
               "JUL"=7, "AGO"=8, "SEP"=9, "OCT"=10, "NOV"=11, "DIC"=12)

df_clean <- data_long %>%
  mutate(
    Month_Num = month_map[Month],
    Date = as.Date(as.yearmon(paste(Year, Month_Num, sep="-"))) ) %>%
  group_by(Type, Removed, Date) %>%
  summarise(Total = sum(Y, na.rm = TRUE), .groups = 'drop')

df_clean$Removed = as.factor(df_clean$Removed)

# DYNAMIC MAINTENANCE CUSUM PLOT (TDF vs TDF/SM - Operational-Removed Items)
ggplot(df_clean, aes(x = Date, y = Total, color = Type, fill = Removed, linetype = Removed)) +
  geom_line(size = 1.5, alpha = 0.5) +       
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_minimal() + theme(legend.position = "bottom") +
  labs(
    x = "Time (Monthly)",
    y = "Total Maintenance",
    color = "Equipment Type"
  ) 

## Checking Normality
shapiro.test(d.TDF$Y)
ks.test(d.TDF$Y, "pnorm")
nortest::ad.test(d.TDF$Y)
car::qqPlot(d.TDF$Y)

shapiro.test(d2.TDF$Y)
ks.test(d2.TDF$Y, "pnorm")
nortest::ad.test(d2.TDF$Y)
car::qqPlot(d2.TDF$Y)

##############################################################
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
# CUSUM-SN
cusum_sn(DB.d1,group_by_col = TRUE, med=0)
cusum_sn(DB.d2,group_by_col = TRUE, med=0)
# EWMA-SN
ewma_sn(DB.d1, group_by_col = TRUE,med = 0)
ewma_sn(DB.d2, group_by_col = TRUE,med = 0)

## Nonparametric SPC (CASE U)
# CUSUM NOT INDICATED GIVEN THE LAST YEAR PATTERN SHIFTING THE MEAN & MEDIAN
# IF THE CUSUM IS NOT INDICATED THEN SHEWHART WILL NOT BE 
# UNKNOWN PARAMETER CASE USES THE NOT-REMOVED ITEM AS REFERENCE
source(ewma_wr.R)

d11.TDF = data_long[data_long$Removed!="1" & data_long$Type=="TDF / SM ",c(2,4,5)]
DB.d11 = qcc.groups(data=d11.TDF$Y,d11.TDF$Time)
ewma_wr(DB.d11,DB.d1, group_by_col = TRUE)

d21.TDF = data_long[data_long$Removed!="1" & data_long$Type=="TDF",c(2,4,5)]
DB.d21 = qcc.groups(data=d21.TDF$Y,d21.TDF$Time)
ewma_wr(DB.d21,DB.d2, group_by_col = TRUE)

## DYNAMIC MAINTENANCE CUSUM PLOT + SHIFT DETACTION via EWMA-WR
df_clean %>% filter(Removed==0) %>% 
  ggplot(aes(x = Total, group=Type, fill=Type)) + 
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 12,
                 alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Maintenance Distribution with Density Curve",
    x = "Total Maintenance",
    y = "Density"
  )

## BASE ON THE RESULTS - MINIMUM MAINTENANCE LEVELS PER TYPE
ggplot(df_clean, aes(x = Date, y = Total, color = Type, fill = Removed, linetype = Removed)) +
  geom_line(size = 1.5, alpha = 0.5) +
  geom_hline(yintercept = 15, color = "black", size = 0.8) +
  geom_vline(xintercept = as.Date("2020-04-01"), color = "black", size = 0.8, alpha = 0.2) +
  geom_hline(yintercept = 72, color = "black", size = 0.8) +
  geom_vline(xintercept = as.Date("2021-10-01"), color = "black", size = 0.8, alpha = 0.2) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_minimal() + theme(legend.position = "bottom") +
  labs(
    x = "Time (Monthly)",
    y = "Total Maintenance",
    color = "Equipment Type"
  ) 

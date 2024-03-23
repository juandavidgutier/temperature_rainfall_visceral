library(ltmle)
library(SuperLearner)
library(tidyr)
library(dplyr)
library(forestplot)
library(ggplot2)

#SuperLearner 
sl_libs <- c('SL.biglasso', 'SL.randomForest', 'SL.ranger', 'SL.earth', 'SL.xgboost') 

#Dataset
dataset <- read.csv("D:/data.csv")
str(dataset)

# Calculate the median value of temperature exposure
median_temperature <- median(dataset$Tair_f_inst, na.rm = TRUE)
dataset$Tair_f_inst <- ifelse(dataset$Tair_f_inst > median_temperature, 1, 0)

# Calculate the median value of rainfall exposure
median_rainfall <- median(dataset$Rainf_f_tavg, na.rm = TRUE)
dataset$Rainf_f_tavg <- ifelse(dataset$Rainf_f_tavg > median_rainfall, 1, 0)

# Scale confounders to sd
dataset$SST3 <- scale(dataset$SST3)
dataset$SST4 <- scale(dataset$SST4)
dataset$SST34 <- scale(dataset$SST34)
dataset$SST12 <- scale(dataset$SST12)
dataset$E_SOI <- scale(dataset$E_SOI)
dataset$SOI <- scale(dataset$SOI)
dataset$NATL <- scale(dataset$NATL)
dataset$SATL <- scale(dataset$SATL)
dataset$TROP <- scale(dataset$TROP)
dataset$forest <- scale(dataset$forest)


###############################################################################################################
#####Air temperature lag0
data_temp_lag0 <- select(dataset, 
                     SST3,	SST4, SST34, SST12, E_SOI,	SOI, NATL, SATL, TROP,
                     forest, Year, Month,
                     Tair_f_inst,	lag0_excess, DANE
                     )	

# Drop NAs
data_temp_lag0 = data_temp_lag0 %>% drop_na()

# Grouped by DANE code
id.expanded <- data_temp_lag0$DANE

Lnodes = c("SST3", "SST4", "SST34", "SST12", "E_SOI",	"SOI", "NATL", "SATL", "TROP",
           "forest", "Year", "Month") 


data <- data_temp_lag0 %>% select(-DANE)

#ltmle
temp_lag0 <- ltmle(data, Anodes = "Tair_f_inst", Ynodes = "lag0_excess", abar = 0, id = id.expanded, Lnodes = Lnodes, 
                     SL.cvControl = list(V=3, shuffle = TRUE), SL.library = sl_libs, variance.method="tmle")


result_temp_lag0 <- summary(temp_lag0)
ate_temp_lag0 <- as.numeric(result_temp_lag0$treatment$estimate)
low_temp_lag0 <- result_temp_lag0$treatment$CI[1,1]
up_temp_lag0 <- result_temp_lag0$treatment$CI[1,2]


#####Air temperature lag1
data_temp_lag1 <- select(dataset, 
                         SST3,	SST4, SST34, SST12, E_SOI,	SOI, NATL, SATL, TROP,
                         forest, Year, Month,
                         Tair_f_inst,	lag1_excess, DANE
)	

# Drop NAs
data_temp_lag1 = data_temp_lag1 %>% drop_na()

# Grouped by DANE code
id.expanded <- data_temp_lag1$DANE

Lnodes = c("SST3", "SST4", "SST34", "SST12", "E_SOI",	"SOI", "NATL", "SATL", "TROP",
           "forest", "Year", "Month") 


data <- data_temp_lag1 %>% select(-DANE)

#ltmle
temp_lag1 <- ltmle(data, Anodes = "Tair_f_inst", Ynodes = "lag1_excess", abar = 0, id = id.expanded, Lnodes = Lnodes, 
                   SL.cvControl = list(V=3, shuffle = TRUE), SL.library = sl_libs, variance.method="tmle")


summary(temp_lag1)
result_temp_lag1 <- summary(temp_lag1)
ate_temp_lag1 <- as.numeric(result_temp_lag1$treatment$estimate)
low_temp_lag1 <- result_temp_lag1$treatment$CI[1,1]
up_temp_lag1 <- result_temp_lag1$treatment$CI[1,2]


#####Air temperature lag2
data_temp_lag2 <- select(dataset, 
                         SST3,	SST4, SST34, SST12, E_SOI,	SOI, NATL, SATL, TROP,
                         forest, Year, Month,
                         Tair_f_inst,	lag2_excess, DANE
)	

# Drop NAs
data_temp_lag2 = data_temp_lag2 %>% drop_na()

# Grouped by DANE code
id.expanded <- data_temp_lag2$DANE

Lnodes = c("SST3", "SST4", "SST34", "SST12", "E_SOI",	"SOI", "NATL", "SATL", "TROP",
           "forest", "Year", "Month") 


data <- data_temp_lag2 %>% select(-DANE)

#ltmle
temp_lag2 <- ltmle(data, Anodes = "Tair_f_inst", Ynodes = "lag2_excess", abar = 0, id = id.expanded, Lnodes = Lnodes, 
                   SL.cvControl = list(V=3, shuffle = TRUE), SL.library = sl_libs, variance.method="tmle")


summary(temp_lag2)
result_temp_lag2 <- summary(temp_lag2)
ate_temp_lag2 <- as.numeric(result_temp_lag2$treatment$estimate)
low_temp_lag2 <- result_temp_lag2$treatment$CI[1,1]
up_temp_lag2 <- result_temp_lag2$treatment$CI[1,2]



#####Air temperature lag3
data_temp_lag3 <- select(dataset, 
                         SST3,	SST4, SST34, SST12, E_SOI,	SOI, NATL, SATL, TROP,
                         forest, Year, Month,
                         Tair_f_inst,	lag3_excess, DANE
)	

# Drop NAs
data_temp_lag3 = data_temp_lag3 %>% drop_na()

# Grouped by DANE code
id.expanded <- data_temp_lag3$DANE

Lnodes = c("SST3", "SST4", "SST34", "SST12", "E_SOI",	"SOI", "NATL", "SATL", "TROP",
           "forest", "Year", "Month") 


data <- data_temp_lag3 %>% select(-DANE)

#ltmle
temp_lag3 <- ltmle(data, Anodes = "Tair_f_inst", Ynodes = "lag3_excess", abar = 0, id = id.expanded, Lnodes = Lnodes, 
                   SL.cvControl = list(V=3, shuffle = TRUE), SL.library = sl_libs, variance.method="tmle")


summary(temp_lag3)
result_temp_lag3 <- summary(temp_lag3)
ate_temp_lag3 <- as.numeric(result_temp_lag3$treatment$estimate)
low_temp_lag3 <- result_temp_lag3$treatment$CI[1,1]
up_temp_lag3 <- result_temp_lag3$treatment$CI[1,2]



#####Air temperature lag4
data_temp_lag4 <- select(dataset, 
                         SST3,	SST4, SST34, SST12, E_SOI,	SOI, NATL, SATL, TROP,
                         forest, Year, Month,
                         Tair_f_inst,	lag4_excess, DANE
)	

# Drop NAs
data_temp_lag4 = data_temp_lag4 %>% drop_na()

# Grouped by DANE code
id.expanded <- data_temp_lag4$DANE

Lnodes = c("SST3", "SST4", "SST34", "SST12", "E_SOI",	"SOI", "NATL", "SATL", "TROP",
           "forest", "Year", "Month") 


data <- data_temp_lag4 %>% select(-DANE)

#ltmle
temp_lag4 <- ltmle(data, Anodes = "Tair_f_inst", Ynodes = "lag4_excess", abar = 0, id = id.expanded, Lnodes = Lnodes, 
                   SL.cvControl = list(V=3, shuffle = TRUE), SL.library = sl_libs, variance.method="tmle")


summary(temp_lag4)
result_temp_lag4 <- summary(temp_lag4)
ate_temp_lag4 <- as.numeric(result_temp_lag4$treatment$estimate)
low_temp_lag4 <- result_temp_lag4$treatment$CI[1,1]
up_temp_lag4 <- result_temp_lag4$treatment$CI[1,2]



#####Air temperature lag5
data_temp_lag5 <- select(dataset, 
                         SST3,	SST4, SST34, SST12, E_SOI,	SOI, NATL, SATL, TROP,
                         forest, Year, Month,
                         Tair_f_inst,	lag5_excess, DANE
)	

# Drop NAs
data_temp_lag5 = data_temp_lag5 %>% drop_na()

# Grouped by DANE code
id.expanded <- data_temp_lag5$DANE

Lnodes = c("SST3", "SST4", "SST34", "SST12", "E_SOI",	"SOI", "NATL", "SATL", "TROP",
           "forest", "Year", "Month") 


data <- data_temp_lag5 %>% select(-DANE)

#ltmle
temp_lag5 <- ltmle(data, Anodes = "Tair_f_inst", Ynodes = "lag5_excess", abar = 0, id = id.expanded, Lnodes = Lnodes, 
                   SL.cvControl = list(V=3, shuffle = TRUE), SL.library = sl_libs, variance.method="tmle")


summary(temp_lag5)
result_temp_lag5 <- summary(temp_lag5)
ate_temp_lag5 <- as.numeric(result_temp_lag5$treatment$estimate)
low_temp_lag5 <- result_temp_lag5$treatment$CI[1,1]
up_temp_lag5 <- result_temp_lag5$treatment$CI[1,2]



#####Air temperature lag6
data_temp_lag6 <- select(dataset, 
                         SST3,	SST4, SST34, SST12, E_SOI,	SOI, NATL, SATL, TROP,
                         forest, Year, Month,
                         Tair_f_inst,	lag6_excess, DANE
)	

# Drop NAs
data_temp_lag6 = data_temp_lag6 %>% drop_na()

# Grouped by DANE code
id.expanded <- data_temp_lag6$DANE

Lnodes = c("SST3", "SST4", "SST34", "SST12", "E_SOI",	"SOI", "NATL", "SATL", "TROP",
           "forest", "Year", "Month") 


data <- data_temp_lag6 %>% select(-DANE)

#ltmle
temp_lag6 <- ltmle(data, Anodes = "Tair_f_inst", Ynodes = "lag6_excess", abar = 0, id = id.expanded, Lnodes = Lnodes, 
                   SL.cvControl = list(V=3, shuffle = TRUE), SL.library = sl_libs, variance.method="tmle")


summary(temp_lag6)
result_temp_lag6 <- summary(temp_lag6)
ate_temp_lag6 <- as.numeric(result_temp_lag6$treatment$estimate)
low_temp_lag6 <- result_temp_lag6$treatment$CI[1,1]
up_temp_lag6 <- result_temp_lag6$treatment$CI[1,2]


#Figure 3
# Dataframe of results for temperature
df <- data.frame(
    lag = c("Lag0", "Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Lag6"),
    ATE = c(ate_temp_lag0, ate_temp_lag1, ate_temp_lag2, ate_temp_lag3, ate_temp_lag4, ate_temp_lag5, ate_temp_lag6),
    Lower = c(low_temp_lag0, low_temp_lag1, low_temp_lag2, low_temp_lag3, low_temp_lag4, low_temp_lag5, low_temp_lag6),
    Upper = c(up_temp_lag0, up_temp_lag1, up_temp_lag2, up_temp_lag3, up_temp_lag4, up_temp_lag5, up_temp_lag6)
  )
  
ggplot(data=df, aes(x=lag,y=ATE,ymin=Lower,ymax=Upper)) + 
    expand_limits(y = -0.00005) +
    geom_point(shape = 18, size = 5, col="red") +
    geom_point() + geom_linerange() + 
    geom_hline(yintercept = 0.00000, color = "red", linetype = "dashed", cex = 1, alpha = 0.25) +
    ylab("ATE (95% Confidence Interval)") + 
    theme_minimal() + coord_flip() + 
    theme(axis.title.y = element_blank()) 
  




###############################################################################################################
#####Rainfall lag0
data_rain_lag0 <- select(dataset, 
                         SST3,	SST4, SST34, SST12, E_SOI,	SOI, NATL, SATL, TROP,
                         forest, Year, Month,
                         Rainf_f_tavg,	lag0_excess, DANE
)	

# Drop NAs
data_rain_lag0 = data_rain_lag0 %>% drop_na()

# Grouped by DANE code
id.expanded <- data_rain_lag0$DANE

Lnodes = c("SST3", "SST4", "SST34", "SST12", "E_SOI",	"SOI", "NATL", "SATL", "TROP",
           "forest", "Year", "Month") 


data <- data_rain_lag0 %>% select(-DANE)

#ltmle
rain_lag0 <- ltmle(data, Anodes = "Rainf_f_tavg", Ynodes = "lag0_excess", abar = 0, id = id.expanded, Lnodes = Lnodes, 
                   SL.cvControl = list(V=3, shuffle = TRUE), SL.library = sl_libs, variance.method="tmle")


summary(rain_lag0)
result_rain_lag0 <- summary(rain_lag0)
ate_rain_lag0 <- as.numeric(result_rain_lag0$treatment$estimate)
low_rain_lag0 <- result_rain_lag0$treatment$CI[1,1]
up_rain_lag0 <- result_rain_lag0$treatment$CI[1,2]



#####Rainfall lag1
data_rain_lag1 <- select(dataset, 
                         SST3,	SST4, SST34, SST12, E_SOI,	SOI, NATL, SATL, TROP,
                         forest, Year, Month,
                         Rainf_f_tavg,	lag1_excess, DANE
)	

# Drop NAs
data_rain_lag1 = data_rain_lag1 %>% drop_na()

# Grouped by DANE code
id.expanded <- data_rain_lag1$DANE

Lnodes = c("SST3", "SST4", "SST34", "SST12", "E_SOI",	"SOI", "NATL", "SATL", "TROP",
           "forest", "Year", "Month") 


data <- data_rain_lag1 %>% select(-DANE)

#ltmle
rain_lag1 <- ltmle(data, Anodes = "Rainf_f_tavg", Ynodes = "lag1_excess", abar = 0, id = id.expanded, Lnodes = Lnodes, 
                   SL.cvControl = list(V=3, shuffle = TRUE), SL.library = sl_libs, variance.method="tmle")


summary(rain_lag1)
result_rain_lag1 <- summary(rain_lag1)
ate_rain_lag1 <- as.numeric(result_rain_lag1$treatment$estimate)
low_rain_lag1 <- result_rain_lag1$treatment$CI[1,1]
up_rain_lag1 <- result_rain_lag1$treatment$CI[1,2]


#####Rainfall lag2
data_rain_lag2 <- select(dataset, 
                         SST3,	SST4, SST34, SST12, E_SOI,	SOI, NATL, SATL, TROP,
                         forest, Year, Month,
                         Rainf_f_tavg,	lag2_excess, DANE
)	

# Drop NAs
data_rain_lag2 = data_rain_lag2 %>% drop_na()

# Grouped by DANE code
id.expanded <- data_rain_lag2$DANE

Lnodes = c("SST3", "SST4", "SST34", "SST12", "E_SOI",	"SOI", "NATL", "SATL", "TROP",
           "forest", "Year", "Month") 


data <- data_rain_lag2 %>% select(-DANE)

#ltmle
rain_lag2 <- ltmle(data, Anodes = "Rainf_f_tavg", Ynodes = "lag2_excess", abar = 0, id = id.expanded, Lnodes = Lnodes, 
                   SL.cvControl = list(V=3, shuffle = TRUE), SL.library = sl_libs, variance.method="tmle")


summary(rain_lag2)
result_rain_lag2 <- summary(rain_lag2)
ate_rain_lag2 <- as.numeric(result_rain_lag2$treatment$estimate)
low_rain_lag2 <- result_rain_lag2$treatment$CI[1,1]
up_rain_lag2 <- result_rain_lag2$treatment$CI[1,2]



#####Rainfall lag3
data_rain_lag3 <- select(dataset, 
                         SST3,	SST4, SST34, SST12, E_SOI,	SOI, NATL, SATL, TROP,
                         forest, Year, Month,
                         Rainf_f_tavg,	lag3_excess, DANE
)	

# Drop NAs
data_rain_lag3 = data_rain_lag3 %>% drop_na()

# Grouped by DANE code
id.expanded <- data_rain_lag3$DANE

Lnodes = c("SST3", "SST4", "SST34", "SST12", "E_SOI",	"SOI", "NATL", "SATL", "TROP",
           "forest", "Year", "Month") 


data <- data_rain_lag3 %>% select(-DANE)

#ltmle
rain_lag3 <- ltmle(data, Anodes = "Rainf_f_tavg", Ynodes = "lag3_excess", abar = 0, id = id.expanded, Lnodes = Lnodes, 
                   SL.cvControl = list(V=3, shuffle = TRUE), SL.library = sl_libs, variance.method="tmle")


summary(rain_lag3)
result_rain_lag3 <- summary(rain_lag3)
ate_rain_lag3 <- as.numeric(result_rain_lag3$treatment$estimate)
low_rain_lag3 <- result_rain_lag3$treatment$CI[1,1]
up_rain_lag3 <- result_rain_lag3$treatment$CI[1,2]



#####Rainfall lag4
data_rain_lag4 <- select(dataset, 
                         SST3,	SST4, SST34, SST12, E_SOI,	SOI, NATL, SATL, TROP,
                         forest, Year, Month,
                         Rainf_f_tavg,	lag4_excess, DANE
)	

# Drop NAs
data_rain_lag4 = data_rain_lag4 %>% drop_na()

# Grouped by DANE code
id.expanded <- data_rain_lag4$DANE

Lnodes = c("SST3", "SST4", "SST34", "SST12", "E_SOI",	"SOI", "NATL", "SATL", "TROP",
           "forest", "Year", "Month") 


data <- data_rain_lag4 %>% select(-DANE)

#ltmle
rain_lag4 <- ltmle(data, Anodes = "Rainf_f_tavg", Ynodes = "lag4_excess", abar = 0, id = id.expanded, Lnodes = Lnodes, 
                   SL.cvControl = list(V=3, shuffle = TRUE), SL.library = sl_libs, variance.method="tmle")


summary(rain_lag4)
result_rain_lag4 <- summary(rain_lag4)
ate_rain_lag4 <- as.numeric(result_rain_lag4$treatment$estimate)
low_rain_lag4 <- result_rain_lag4$treatment$CI[1,1]
up_rain_lag4 <- result_rain_lag4$treatment$CI[1,2]



#####Rainfall lag5
data_rain_lag5 <- select(dataset, 
                         SST3,	SST4, SST34, SST12, E_SOI,	SOI, NATL, SATL, TROP,
                         forest, Year, Month,
                         Rainf_f_tavg,	lag5_excess, DANE
)	

# Drop NAs
data_rain_lag5 = data_rain_lag5 %>% drop_na()

# Grouped by DANE code
id.expanded <- data_rain_lag5$DANE

Lnodes = c("SST3", "SST4", "SST34", "SST12", "E_SOI",	"SOI", "NATL", "SATL", "TROP",
           "forest", "Year", "Month") 


data <- data_rain_lag5 %>% select(-DANE)

#ltmle
rain_lag5 <- ltmle(data, Anodes = "Rainf_f_tavg", Ynodes = "lag5_excess", abar = 0, id = id.expanded, Lnodes = Lnodes, 
                   SL.cvControl = list(V=3, shuffle = TRUE), SL.library = sl_libs, variance.method="tmle")


summary(rain_lag5)
result_rain_lag5 <- summary(rain_lag5)
ate_rain_lag5 <- as.numeric(result_rain_lag5$treatment$estimate)
low_rain_lag5 <- result_rain_lag5$treatment$CI[1,1]
up_rain_lag5 <- result_rain_lag5$treatment$CI[1,2]



#####Rainfall lag6
data_rain_lag6 <- select(dataset, 
                         SST3,	SST4, SST34, SST12, E_SOI,	SOI, NATL, SATL, TROP,
                         forest, Year, Month,
                         Rainf_f_tavg,	lag6_excess, DANE
)	

# Drop NAs
data_rain_lag6 = data_rain_lag6 %>% drop_na()

# Grouped by DANE code
id.expanded <- data_rain_lag6$DANE

Lnodes = c("SST3", "SST4", "SST34", "SST12", "E_SOI",	"SOI", "NATL", "SATL", "TROP",
           "forest", "Year", "Month") 


data <- data_rain_lag6 %>% select(-DANE)

#ltmle
rain_lag6 <- ltmle(data, Anodes = "Rainf_f_tavg", Ynodes = "lag6_excess", abar = 0, id = id.expanded, Lnodes = Lnodes, 
                   SL.cvControl = list(V=3, shuffle = TRUE), SL.library = sl_libs, variance.method="tmle")


summary(rain_lag6)
result_rain_lag6 <- summary(rain_lag6)
ate_rain_lag6 <- as.numeric(result_rain_lag6$treatment$estimate)
low_rain_lag6 <- result_rain_lag6$treatment$CI[1,1]
up_rain_lag6 <- result_rain_lag6$treatment$CI[1,2]



#Figure 4
# Dataframe of results for rainfall
df <- data.frame(
  lag = c("Lag0", "Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Lag6"),
  ATE = c(ate_rain_lag0, ate_rain_lag1, ate_rain_lag2, ate_rain_lag3, ate_rain_lag4, ate_rain_lag5, ate_rain_lag6),
  Lower = c(low_rain_lag0, low_rain_lag1, low_rain_lag2, low_rain_lag3, low_rain_lag4, low_rain_lag5, low_rain_lag6),
  Upper = c(up_rain_lag0, up_rain_lag1, up_rain_lag2, up_rain_lag3, up_rain_lag4, up_rain_lag5, up_rain_lag6)
)

ggplot(data=df, aes(x=lag,y=ATE,ymin=Lower,ymax=Upper)) + 
  expand_limits(y = -0.0005) +
  geom_point(shape = 18, size = 5, col="red") +
  geom_point() + geom_linerange() + 
  geom_hline(yintercept = 0.00000, color = "red", linetype = "dashed", cex = 1, alpha = 0.25) +
  ylab("ATE (95% Confidence Interval)") + 
  theme_minimal() + coord_flip() + 
  theme(axis.title.y = element_blank()) 
  
  
  
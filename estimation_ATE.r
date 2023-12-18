library(ltmle)
library(SuperLearner)
library(tidyr)
library(dplyr)
library(forestplot)


#Learners 
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

#Tertiles of confounders
dataset$SST3 <- dplyr::ntile(dataset$SST3, 3)
dataset$SST4 <- dplyr::ntile(dataset$SST4, 3)
dataset$SST34 <- dplyr::ntile(dataset$SST34, 3)
dataset$SST12 <- dplyr::ntile(dataset$SST12, 3)
dataset$E_SOI <- dplyr::ntile(dataset$E_SOI, 3)
dataset$SOI <- dplyr::ntile(dataset$SOI, 3)
dataset$NATL <- dplyr::ntile(dataset$NATL, 3)
dataset$SATL <- dplyr::ntile(dataset$SATL, 3)
dataset$TROP <- dplyr::ntile(dataset$TROP, 3)
dataset$forest <- dplyr::ntile(dataset$forest, 3)


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


summary(temp_lag0)



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


#Figure 3
# Dataframe of results for temperature
df <- data.frame(
    lag = c("Lag0", "Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Lag6"),
    ATE = c(0.00006, 0.00006, 0.00005, 0.00005, 0.00005, 0.00003, 0.00005),
    Lower = c(0.00000, 0.00001, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000),
    Upper = c(0.00012, 0.00011, 0.00011, 0.00010, 0.00010, 0.00006, 0.00011)
  )
  
ggplot(data=df, aes(x=lag,y=ATE,ymin=Lower,ymax=Upper)) + 
    expand_limits(y = -0.00005) +
    geom_point(shape = 18, size = 5, col="black") +
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



#Figure 4
# Dataframe of results for rainfall
df <- data.frame(
  lag = c("Lag0", "Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Lag6"),
  ATE = c(0.00088, 0.00065, 0.00065, 0.00067, 0.00066, 0.00069, 0.00071),
  Lower = c(0.00036, 0.0002, 0.00017, 0.00024, 0.00027, 0.00032, 0.00031),
  Upper = c(0.00140, 0.00109, 0.00112, 0.00109, 0.00105, 0.00106, 0.00112)
)

ggplot(data=df, aes(x=lag,y=ATE,ymin=Lower,ymax=Upper)) + 
  expand_limits(y = -0.0005) +
  geom_point(shape = 18, size = 5, col="black") +
  geom_point() + geom_linerange() + 
  geom_hline(yintercept = 0.00000, color = "red", linetype = "dashed", cex = 1, alpha = 0.25) +
  ylab("ATE (95% Confidence Interval)") + 
  theme_minimal() + coord_flip() + 
  theme(axis.title.y = element_blank()) 
  
  
  
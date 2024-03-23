# Note that the result of the identification is the same for all lags and exposure variables, for this reason here we include
# only the identification for lag = 0 and exposure variable air temperature

library(ggdag)
library(dagitty)
library(lavaan)
library(CondIndTests)
library(dplyr)
library(GGally)
library(tidyr)
library(MKdescr)


#################################################################################
#implied Conditional Independencies
dataset <- read.csv("D:/data.csv")
dataset <- select(dataset, Year, Month, SOI, E_SOI, SST3, SST4, SST34, SST12, NATL, SATL, TROP, forest, vector, Tair_f_inst, lag0_excess)

#sd units
dataset$SST12 <- zscore(dataset$SST12, na.rm = TRUE)  
dataset$SST3 <- zscore(dataset$SST3, na.rm = TRUE) 
dataset$SST34 <- zscore(dataset$SST34, na.rm = TRUE) 
dataset$SST4 <- zscore(dataset$SST4, na.rm = TRUE) 
dataset$SOI <- zscore(dataset$SOI, na.rm = TRUE) 
dataset$E_SOI <- zscore(dataset$E_SOI, na.rm = TRUE) 
dataset$NATL <- zscore(dataset$NATL, na.rm = TRUE) 
dataset$SATL <- zscore(dataset$SATL, na.rm = TRUE) 
dataset$TROP <- zscore(dataset$TROP, na.rm = TRUE)
dataset$forest <- zscore(dataset$forest, na.rm = TRUE) 
dataset$vector <- zscore(dataset$vector, na.rm = TRUE) 
dataset$Tair_f_inst <- zscore(dataset$Tair_f_inst, na.rm = TRUE) 


#complete observations
dataset <- dataset[complete.cases(dataset), ] 
str(dataset)

#descriptive analysis
#ggpairs(dataset)

#DAG 
dag <- dagitty('dag {
lag0_excess [pos="0, 0.5"]
Tair_f_inst  [pos="-1, 0.5"]

E_SOI [pos="-1.6, 1.1"]
SOI [pos="-1.7, 1.2"]
SST3 [pos="-1.8, 1.3"]
SST4 [pos="-1.9, 1.4"]
SST34 [pos="-2, 1.5"]
SST12 [pos="-2.1, 1.6"]
NATL [pos="-2.2, 1.7"]
SATL [pos="-2.3, 1.8"]
TROP [pos="-2.4, 1.9"]
forest [pos="-2.0, -2.1"] 
vector [pos="-0.5, -1.8"] 
Year [pos="-2.6, -2.1"] 
Month [pos="-2.7, -2.3"] 

SST12 -> SST3
SST12 -> SST34
SST12 -> SST4
SST12 -> SOI
SST12 -> E_SOI
SST12 -> NATL
SST12 -> SATL
SST12 -> TROP


SST3 -> SST34
SST3 -> SST4
SST3 -> SOI
SST3 -> E_SOI
SST3 -> NATL
SST3 -> SATL
SST3 -> TROP

SST34 -> SST4
SST34 -> SOI
SST34 -> E_SOI
SST34 -> NATL
SST34 -> SATL
SST34 -> TROP


SST4 -> SOI
SST4 -> E_SOI
SST4 -> NATL
SST4 -> SATL
SST4 -> TROP

SOI -> E_SOI
SOI -> NATL
SOI -> SATL
SOI -> TROP

E_SOI -> NATL
E_SOI -> SATL
E_SOI -> TROP


NATL -> SATL
NATL -> TROP

SATL -> TROP


SST12 -> Tair_f_inst
SST3 -> Tair_f_inst
SST34 -> Tair_f_inst
SST4 -> Tair_f_inst
SOI -> Tair_f_inst
E_SOI -> Tair_f_inst
NATL -> Tair_f_inst
SATL -> Tair_f_inst
TROP -> Tair_f_inst


SST12 -> lag0_excess
SST3 -> lag0_excess
SST34 -> lag0_excess
SST4 -> lag0_excess
SOI -> lag0_excess
E_SOI -> lag0_excess
NATL -> lag0_excess
SATL -> lag0_excess
TROP -> lag0_excess


SST12 -> vector
SST3 -> vector
SST34 -> vector
SST4 -> vector
SOI -> vector
E_SOI -> vector
NATL -> vector
SATL -> vector
TROP -> vector

forest -> vector
Tair_f_inst -> vector
forest -> Tair_f_inst
forest -> lag0_excess

Year -> SST12
Year -> SST3
Year -> SST34
Year -> SST4
Year -> SOI
Year -> E_SOI
Year -> NATL
Year -> SATL
Year -> TROP
Year -> Tair_f_inst
Year -> lag0_excess

Month -> SST12
Month -> SST3
Month -> SST34
Month -> SST4
Month -> SOI
Month -> E_SOI
Month -> NATL
Month -> SATL
Month -> TROP
Month -> Tair_f_inst
Month -> lag0_excess

vector -> lag0_excess

Tair_f_inst -> lag0_excess



}')  


plot(dag)


#check whether any correlations are perfect (i.e., collinearity)
myCov <- cov(dataset)
round(myCov, 2)

myCor <- cov2cor(myCov)
noDiag <- myCor
diag(noDiag) <- 0
any(noDiag == 1)

#if not, check for multicollinearity (i.e., is one variable a linear combination of 2+ variables?)
det(myCov) < 0
##or
any(eigen(myCov)$values < 0)


#independencias condicionales
impliedConditionalIndependencies(dag)
corr <- lavCor(dataset)

#plot con ci convencinales (método analitico)
localTests(dag, sample.cov=corr, sample.nobs=nrow(dataset))
plotLocalTestResults(localTests(dag, sample.cov=corr, sample.nobs=nrow(dataset)), xlim=c(-1,1))





#identification
simple_dag <- dagify(
  lag0_excess ~  Tair_f_inst + SST12 + SST3 + SST34 + SST4 + SOI + E_SOI + NATL + SATL +  TROP + forest + vector + Year + Month,
  Tair_f_inst ~ SST12 + SST3 + SST34 + SST4 + SOI + E_SOI + NATL + SATL +  TROP + forest + Year + Month, 
  SST12 ~ SST3 + SST34 + SST4 + SOI + E_SOI + NATL + SATL +  TROP + Year + Month,
  SST3 ~ SST34 + SST4 + SOI + E_SOI + NATL + SATL +  TROP + Year + Month,
  SST34 ~ SST4 + SOI + E_SOI + NATL + SATL +  TROP + Year + Month,
  SST4 ~ SOI + E_SOI + NATL + SATL +  TROP + Year + Month,
  SOI ~ E_SOI + NATL + SATL +  TROP + Year + Month,
  E_SOI ~ NATL + SATL +  TROP + Year + Month,
  NATL ~ SATL +  TROP + Year + Month,
  SATL ~  TROP + Year + Month,
  TROP ~ Year + Month,
  vector ~ SST12 + SST3 + SST34 + SST4 + SOI + E_SOI + NATL + SATL +  TROP + forest + Tair_f_inst,
  
  
  exposure = "Tair_f_inst",
  outcome = "lag0_excess",
  coords = list(x = c(Tair_f_inst=2, vector=1, lag0_excess=2, SST12=3, SST3=3.1, SST34=3.2, SST4=3.3, SOI=3.4, 
                      E_SOI=3.5, NATL=3.6, SATL=3.7, TROP=3.8, forest=1.5, 
                      Year=3.5, Month=3),
                y = c(Tair_f_inst=2, vector=1.5, lag0_excess=1, SST12=3, SST3=3.1, SST34=3.2, SST4=3.3, SOI=3.4, 
                      E_SOI=3.5, NATL=3.6, SATL=3.7, TROP=3.8, forest=3.0,
                      Year=1.8, Month=1.4))
)


#theme_dag() coloca la trama en un fondo blanco sin etiquetas en los ejes
ggdag(simple_dag) + 
  theme_dag()

ggdag_status(simple_dag) +
  theme_dag()


#adjusting
adjustmentSets(simple_dag,  type = "minimal")


ggdag_adjustment_set(simple_dag, shadow = TRUE) +
  theme_dag()



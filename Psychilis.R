#################################################################
#                                                               #
#                BIOL6007 - Psychilis data analysis             #
#                  Elif KARDAS - elifka@live.be                 #
#                       26 Sept. 2019                           #
#                                                               #
#################################################################

#################################################################
# Getting the data 
#################################################################

## Find the work directory
getwd()
## Path to the file
setwd("C:/Users/Elif/Documents/UPRrp - Courses/BIOL6007/Psychilis")
## Print all the files in this directory
files <- dir()
dir()
psy <- read.csv(file=files[1])
View(psy)
str(psy)


#################################################################
# Organizing the data by vectors (by groups: RS, RE, T)
#################################################################
RS <- c("Poll_rmvl", "Poll", "Num_fruits")
RE <- c("Scars", "Num_Infl", "Inf_Len_cm", "N_Flws", "Num_Bulb", "Buds")
T <- c("PseuBulbDi_mm", "PseuBulbLen_mm", "Leaves_SpeuBulb", "LeavesTotal")
ENV <- c("Percen_cover")

#################################################################
# Doing the linear model of these variables
#################################################################
psylm <- lm(Poll_rmvl~Inf_Len_cm+Num_Infl+Num_fruits+Scars, data=psy, na.action=na.exclude)
## here we need to test a lot of models, or just: y = RS and x = RE, T and ENV
## todavia no sé
summary(psylm)
options(na.action = "na.fail") # to not consider NA

#################################################################
# Testing the hypotheses to run a linear models 
#################################################################

# Explorative plot:
layout(matrix(c(1,2,3,4),2,2))
plot(psylm) 
## Linearity and homoscedasticity: no (not comprised between -2 and 2)
## None of the dots are out of 0.5, there is no points that is too different -> homogeneity OK 


# Testing the hypotheses more quantitatively

## 1. Testing the residuals normality 
resid_psylm <- residuals(psylm)
shapiro.test(resid_psylm)
hist(resid_psylm)
# residuals are normal? NO because p-value is < 0.05 (p-value = 1.819e-06), 
# so RHO which says that the residuals are normally distributed. HERE THEY ARE NOT

## 2. Testing variances homogeneity
plot(psylm) 
## the QQ plot is ok, so the normality is more or less OK (cf. upper)
## the variances are not homogenous because the residuals are not comprised between -2 and 2
# or we can also do the barlett test:
bartlett.test(Poll_rmvl ~ Inf_Len_cm, data = psy) ## need to fix this
# none of the dots are out of 0.5, there is no points that is too different
# 3. OVERFITTING, see plot(psylm)

#################################################################
# we test one by one RE & T together on RS, to see what variables 
# explain the RS
#################################################################
# using advices on https://www.statmethods.net/stats/regression.html
# Stepwise Regressions
library(MASS)

# RE on RS:
psylm1 <- lm(Poll_rmvl ~ Poll+Poll_rmvl+Num_fruits,data=psy, na.action=na.exclude)
step <- stepAIC(psylm1, direction="both")
step$anova # display results

psylm2 <- lm(Poll ~Poll+Poll_rmvl+Num_fruits, data=psy, na.action=na.exclude)
step <- stepAIC(psylm2, direction="both")
step$anova # display results
## weird

anova(psylm1, psylm2)

# then we select the variables explaining RS

# T on RS:

# then we select the variables explaining RS

# we also test the interaction between RE and T on RS and we select the ones 


# NB:
# RS <- c("Poll_rmvl", "Poll", "Num_fruits")
# RE <- c("Scars", "Num_Infl", "Inf_Len_cm", "N_Flws", "Num_Bulb", "Buds")
# T <- c("PseuBulbDi_mm", "PseuBulbLen_mm", "Leaves_SpeuBulb", "LeavesTotal")
# ENV <- c("Percen_cover")


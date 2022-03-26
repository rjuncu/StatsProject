#Project 1 Analysis Script: Missing Data for Titanic Data Set

#Group 6
#Maria Kim r0831824
#Isaac Samuel Racine r0867171
#Soniya Lama r0715145
#Rucsanda Juncu r0872274
#Nikolay Makarenko r0825855
#Niccolò Guglietta r0869647

#libraries
library(pastecs)
library(mice)
library(lattice)
library(VIM)
library(aod)
library(BaM)
library(ggplot2)
library(reshape2)
library(ggh4x)
library(tidyverse)
library(gridExtra)
library(AICcmodavg)
library(ggpubr)
library(ggplotify)
library(pscl)
library(ROCR)

#set wd
setwd("C:/Users/isaac/Education/UVM/R/KU/BioinformaticStats/Project1/StatsProject")

#read in completed data and incompleted
titanicC = read.csv("Titanic-complete.txt", header = T, sep = " ")
titanicI = read.csv("Titanic-incomplete.txt", header = T, sep = " ")

#check to see if data present
head(titanicC)
head(titanicI)

#-----------------Part 1: Analyzing Full Data Set-------------------
#check to make sure the descriptive statistics are as expected
descrip.titanicC <- stat.desc(titanicC[,names(titanicC)],basic=TRUE, desc=TRUE)
descrip.titanicC


#mutate the data to allow for easy graph building
titanicC.mod <- titanicC %>%
  mutate(sex = ifelse(sex == "0", "male", "female")) %>%
  mutate(class = ifelse(class == "0", "not 1st class", "1st class"))  %>%
  mutate(survived = ifelse(survived == "0", "died", "survived"))

head(titanicC.mod)

#make visuals for exploring the shape and distribution of the data
g1 <- ggplot(titanicC.mod, aes(x = age)) +
  geom_histogram() + 
  facet_nested(class + sex ~ survived) +
  ggtitle("Comparison of Age Faceted By \nSurvival and Nested Class and Sex") +
  theme_bw()


g2 <- ggplot(titanicC.mod, aes(x = age, fill = class, alpha = survived)) +
  geom_boxplot() + 
  facet_nested(class + sex ~ survived) +
  ggtitle("Comparison of Age Faceted By \nSurvival and Nested Class and Sex") +
  theme_bw()

g3 <- ggplot(data = titanicC.mod, aes(x = survived)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = class, alpha = sex)) + 
  ylab("proportion") +
  ggtitle("Proportion of Survivors \nGrouped by Class and Sex") + theme_bw()

figure1 <- ggarrange(g3,g1,g2,
                    labels = c("A", "B","c"),
                    ncol = 3, nrow =1)
figure1


#construct the logistic regression model
titanicC.glm <-glm(survived ~ class+sex+age,data=titanicC, family=binomial(link="logit"))
summary(titanicC.glm)

#-----------------Part 2: Where is the missing data?----------------

#aggregation of which data is missing and counts
titanicI.aggr <- aggr(titanicI,numbers=TRUE, prop=FALSE, ylab=c("Histogram of missing data","Pattern"))
titanicI.aggr

#organize remaining plots into one
par(mfrow = c(2, 2))

## Amount of missigness in age for each survived group
m1 <- barMiss(titanicI[,c("survived","age")], only.miss = F, sub = 'A', main = "Missing Data Amount by Survival Status")

## Amount of missigness in age for each sex group
m2 <- barMiss(titanicI[,c("sex","age")], only.miss = F, sub = "B", main = "Missing Data Amount by Sex")

## Amount of missigness in age for each class group
m3 <- barMiss(titanicI[,c("class","age")], only.miss = F, sub = "C", main = "Missing Data Amount by Class")

##histogram of ages
y <- titanicI[, c("age", "survived")]
m4 <- histMiss(y, only.miss = FALSE, sub = "D", main ="Histogram of Passengers Ages with \nBar Plot Representing Observed and \nMissing Observations")


#-----------------Part 3: CC Analysis-------------------------------
#generate the logisitc regression modle for complete case analysis
titanicI.glm <- glm(survived ~ class+sex+age,data=titanicI, family=binomial(link="logit"))
summary(titanicI.glm)

#-----------------Part 4: MI Analysis-------------------------------

#Imputting the missing values

imp_mice_pmm <- mice(titanicI, m = 100) 
imp_mice_pmm #PMM = predictive mean matching 


imp_mice_lassonorm <- mice(titanicI, m = 100, method = "lasso.norm")
imp_mice_lassonorm #Lasso regression for age


imp_mice_norm <- mice(titanicI, m =100, method = "norm")
imp_mice_norm #Bayesian linear regression


imp_mice_wpmm <- mice(titanicI, m =100, method = "midastouch")
imp_mice_wpmm #Weighted predictive mean matching


#PMM
complete(imp_mice_pmm, 1)
com_pmm <- complete(imp_mice_pmm, action ="long", inc=T)
col_pmm <- rep(c("blue","red")[1+as.numeric(is.na(imp_mice_pmm$data$age))],101)
stripplot(age~.imp, data=com_pmm, jit=FALSE, fac=0.8, col=col_pmm, pch=20, cex=1.4,
          xlab= "Imputation number", main = "Distribution of the imputed data")
fit_pmm <- with(data=imp_mice_pmm, exp=glm(survived ~ class + sex + age, family=binomial))
estimate_pmm <- pool(fit_pmm)
summary(estimate_pmm)

### - Wald test to test significance of regression coefficients
complete_pmm <- complete(imp_mice_pmm, 1)
summary(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_pmm))$coefficients
### - Deviance
summary(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_pmm))$deviance #233.7264
### - Pseudo R? -
pR2(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_pmm)) #0.1528244

#PMM analysis ROC curve
names(complete_pmm)
glm_pmm <- glm(survived ~ class + sex + age, family = binomial(link = logit), data = complete_pmm)
pred_m1 <- prediction(fitted(glm_pmm), complete_pmm$survived)
perf_m1 <- performance(pred_m1, measure = "tpr", x.measure = "fpr")
plot(perf_m1, main = "Sensitivity vs False Positive Rate", colorize = TRUE, colorkey.relwidth = 0.5, lwd = 4.5)
performance(pred_m1, measure = "auc")@y.values
#Area under the curve. This model explains 75.9% of all data


#LASSO
complete(imp_mice_lassonorm, 1) 
com_lassonorm <- complete(imp_mice_lassonorm, action ="long", inc=T)
fit_lassonorm <- with(data=imp_mice_lassonorm, exp=glm(survived ~ class + sex + age, family=binomial))
estimate_lassonorm <- pool(fit_lassonorm)
summary(estimate_lassonorm) 

###Wald Test
complete_lassonorm <- complete(imp_mice_lassonorm, 1)
summary(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_lassonorm))$coefficients
### - Deviance -
summary(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_lassonorm))$deviance #234.2229
### - Pseudo R? -
pR2(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_lassonorm)) #0.1552194

#Analysis ROC curve
names(complete_lassonorm)
glm_lassonorm <- glm(survived ~ class + sex + age, family = binomial(link = logit), data = complete_lassonorm)
pred_m2 <- prediction(fitted(glm_lassonorm), complete_lassonorm$survived)
perf_m2 <- performance(pred_m2, measure = "tpr", x.measure = "fpr")
plot(perf_m2, main = "Sensitivity vs False Positive Rate, Lasso", colorize = TRUE, colorkey.relwidth = 0.5, lwd = 4.5)
performance(pred_m2, measure = "auc")@y.values
#Area under the curve. This model explains 75.5% of all data



#BAYESIAN LINEAR REGRESSION
complete(imp_mice_norm, 1) 
com_norm <- complete(imp_mice_norm, action ="long", inc=T)
fit_norm <- with(data=imp_mice_norm, exp=glm(survived ~ class + sex + age, family=binomial))
estimate_norm <- pool(fit_norm)
summary(estimate_norm)

###Wald Test
complete_norm <- complete(imp_mice_norm, 1)
summary(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_norm))$coefficients
### - Deviance
summary(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_norm))$deviance #228.8713
### - Pseudo R
pR2(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_norm)) #0.1745212

#Analysis ROC curve
names(complete_norm)
glm_norm <- glm(survived ~ class + sex + age, family = binomial(link = logit), data = complete_norm)
pred_m3 <- prediction(fitted(glm_norm), complete_norm$survived)
perf_m3 <- performance(pred_m3, measure = "tpr", x.measure = "fpr")
plot(perf_m3, main = "Sensitivity vs False Positive Rate, Bayesian", colorize = TRUE, colorkey.relwidth = 0.5, lwd = 4.5)
performance(pred_m3, measure = "auc")@y.values
#Area under the curve. This model explains 77.3% of all data


#WEIGHTED PMM
complete(imp_mice_wpmm, 1) 
com_wpmm <- complete(imp_mice_wpmm, action ="long", inc=T)
fit_wpmm <- with(data=imp_mice_wpmm, exp=glm(survived ~ class + sex + age, family=binomial))
estimate_wpmm <- pool(fit_wpmm)
summary(estimate_wpmm)

#Wald Test
complete_wpmm <- complete(imp_mice_wpmm, 1)
summary(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_wpmm))$coefficients
### - Deviance
summary(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_wpmm))$deviance #232.2783
### - Pseudo R
pR2(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_wpmm)) #0.1622332

#Analysis ROC curve
names(complete_wpmm)
glm_wpmm <- glm(survived ~ class + sex + age, family = binomial(link = logit), data = complete_wpmm)
pred_m4 <- prediction(fitted(glm_wpmm), complete_wpmm$survived)
perf_m4 <- performance(pred_m4, measure = "tpr", x.measure = "fpr")
plot(perf_m4, main = "Sensitivity vs False Positive Rate, WPMM", colorize = TRUE, colorkey.relwidth = 0.5, lwd = 4.5)
performance(pred_m4, measure = "auc")@y.values
#Area under the curve. This model explains 78.65% of all data


#COMPARE ALL 4 METHODS IN 1 GRAPH
plot(perf_m1, colorize = FALSE, lwd = 3, col = "red", main = "Sensitivity vs False Positive Rate")
legend("bottomright", c("Predictive Mean Matching", "Lasso Linear Regression", "Bayesian linear regression", "Weighted predictive mean matching"), lty=1, 
       col = c("red", "blue", "green", "orange"), bty="n")
plot(perf_m2, add = TRUE, colorize = FALSE, lwd = 3, col="blue")
plot(perf_m3, add = TRUE, colorize = FALSE, lwd = 3, col ="green")
plot(perf_m4, add = TRUE, colorize = FALSE, lwd = 3, col = "orange")
abline(0, 1, lty = 2)


#-----------------Part 5: IPW Analysis------------------------------
## Creating the missing data indicator variable r
titanicI$r <- as.numeric(!is.na(titanicI$age))*as.numeric(!is.na(titanicI$sex))
head(titanicI)

## Fitting the logistic regression model to calculate the probabilities of being complete
titanic.ipw.glm <- glm(r ~ class + survived, data = titanicI, family = binomial)
summary(titanic.ipw.glm)

## Calculating the weights: Inverse Probabilities
titanicI$w <- 1 / fitted(titanic.ipw.glm)
head(titanicI)

## Constructing the logistic regression model
titanic.results.ipw <- glm(survived ~ class + sex + age, data = titanicI, weights = titanicI$w, family = binomial)
summary(titanic.results.ipw)


#-----------------Part 6: Comparing Analyses------------------------

####Generate odds ratio for class and sex for each data set
#CD
exp(cbind(OR =titanicC.glm$coefficients, confint(titanicC.glm)))

#CC
exp(cbind(OR =titanicI.glm$coefficients, confint(titanicI.glm)))

#MI
exp(cbind(OR =glm_norm$coefficients, confint(glm_norm)))

#IPW
exp(cbind(OR =titanic.results.ipw$coefficients, confint(titanic.results.ipw)))

####AIC analysis
# Fitting the models
titanic.list=list()

titanic.list[[1]]=titanicC.glm
#CC omitted because only using 109 observations 
titanic.list[[2]]=glm_norm
titanic.list[[3]]=titanic.results.ipw

titanic.modnames <- c("CD",  "MI", "IPW")

## Akaike weights with AICcmodavg
titanic.aictab=aictab(cand.set = titanic.list, modnames = titanic.modnames)
titanic.aictab

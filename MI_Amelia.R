#MI Statistics


library(mice)
library(VIM)
library(aod)
library(BaM)
library(lattice)

#set wd
setwd("C:/Users/isaac/Education/UVM/R/KU/BioinformaticStats/Project1/StatProject")

#read in completed data 
titanic <- read.csv("Titanic-incomplete.txt", header = T, sep = " ")

titanic

#Checking the data and visualizing it.

names(titanic)

# we can see that we need to have most of the vars as factors
titanic$survived <- as.factor(titanic$survived)
titanic$class <- as.factor(titanic$class)
titanic$sex <- as.factor(titanic$sex)
summary(titanic)




#VIM library

titanic.vim <- titanic

aggr(titanic.vim,numbers=TRUE
     , ylab=c("Histogram of missing data","Pattern"))

aggr(titanic.vim, combined=TRUE, numbers = TRUE, prop = FALSE,
     cex.numbers=0.87, varheight = FALSE)

#We have to check missing data mechanism. Is there bias?

barMiss(titanic.vim[,c("survived", "age")], interactive = TRUE) #Missing obs in age
#Missing data depends on the survivors, less in survived

barMiss(titanic.vim[,c("class","age")], interactive = TRUE) #Missing obs in class
#Missing data depends on the class, less in 1st class

barMiss(titanic.vim[,c("sex","age")], interactive = TRUE) #Missing obs in sex
#Very limited bias in sex

#We identified that our data is not missing at random. The main components of the missing data depends on surviving and class. 
#page 409 for the formula

# The only way to obtain unbiased estimates of the parameters is to model
# missingness. In other words, we would need to write a model that
# accounts for the missing data mechanism and hope this model is
# approximately correct.

#Sensitivity analysis


#The model we are working with will be 
# logit [P(Y = 1|class, sex, age)] = B0 + B1 ? class + B2 ? sex + B3 ? age


#Sensitivity analysis


#The model we are working with will be 
# logit [P(Y = 1|class, sex, age)] = B0 + B1 ? class + B2 ? sex + B3 ? age


#MICE

pattern <- md.pattern(titanic)
pattern

pairs <- md.pairs(titanic)
pairs

#---Imputting the missing values

imp_mice_pmm <- mice(titanic, m = 100) #why 100?
imp_mice_pmm #PMM = predictive mean matching 


imp_mice_lassonorm <- mice(titanic, m = 100, method = "lasso.norm")
imp_mice_lassonorm #Lasso regression for age


imp_mice_norm <- mice(titanic, m =100, method = "norm")
imp_mice_norm #Bayesian linear regression


imp_mice_wpmm <- mice(titanic, m =100, method = "midastouch")
imp_mice_wpmm #Weighted predictive mean matching

#----Check for heteroscedasticity ??
#----Diagnostic check for the plausibility of the data

imp_mice_pmm$imp$age[1:10,1:5]
# sum(is.na(imp_mice_pmm))
complete(imp_mice_pmm, 1) #I find it odd that they are all arounf the same age
com_pmm <- complete(imp_mice_pmm, action ="long", inc=T)
tail(com_pmm)
col_pmm <- rep(c("blue","red")[1+as.numeric(is.na(imp_mice_pmm$data$age))],101)
stripplot(age~.imp, data=com_pmm, jit=FALSE, fac=0.8, col=col_pmm, pch=20, cex=1.4,
          xlab= "Imputation number")  ##Need help here, not too sure what IM doing

fit_pmm <- with(data=imp_mice_pmm, exp=glm(survived ~ class + sex + age, family=binomial))
estimate_pmm <- pool(fit_pmm)
summary(estimate_pmm) #age is not a significant variable




imp_mice_lassonorm$imp$age[1:10,1:5]
# sum(is.na(imp_mice_pmm))
complete(imp_mice_lassonorm, 1) 
com_lassonorm <- complete(imp_mice_lassonorm, action ="long", inc=T)
tail(com_lassonorm)
col_lassonorm <- rep(c("blue","red")[1+as.numeric(is.na(imp_mice_lassonorm$data$age))],101)
stripplot(age~.imp, data=com_lassonorm, jit=FALSE, fac=0.8, col=col_lassonorm, pch=20, cex=1.4,
          xlab= "Imputation number")  ##Need help here, not too sure what IM doing
fit_lassonorm <- with(data=imp_mice_lassonorm, exp=glm(survived ~ class + sex + age, family=binomial))
estimate_lassonorm <- pool(fit_lassonorm)
summary(estimate_lassonorm) #age is not a significant variable



imp_mice_norm$imp$age[1:10,1:5]
# sum(is.na(imp_mice_pmm))
complete(imp_mice_norm, 1) 
com_norm <- complete(imp_mice_norm, action ="long", inc=T)
tail(com_norm)
col_norm <- rep(c("blue","red")[1+as.numeric(is.na(imp_mice_norm$data$age))],101)
stripplot(age~.imp, data=com_norm, jit=FALSE, fac=0.8, col=col_norm, pch=20, cex=1.4,
          xlab= "Imputation number")  ##Need help here, not too sure what IM doing
fit_norm <- with(data=imp_mice_norm, exp=glm(survived ~ class + sex + age, family=binomial))
estimate_norm <- pool(fit_norm)
summary(estimate_norm) #age is not a significant variable



imp_mice_wpmm$imp$age[1:10,1:5]
# sum(is.na(imp_mice_pmm))
complete(imp_mice_wpmm, 1) 
com_wpmm <- complete(imp_mice_wpmm, action ="long", inc=T)
tail(com_norm)
col_wpmm <- rep(c("blue","red")[1+as.numeric(is.na(imp_mice_wpmm$data$age))],101)
stripplot(age~.imp, data=com_wpmm, jit=FALSE, fac=0.8, col=col_wpmm, pch=20, cex=1.4,
          xlab= "Imputation number")  ##Need help here, not too sure what IM doing
fit_wpmm <- with(data=imp_mice_wpmm, exp=glm(survived ~ class + sex + age, family=binomial))
estimate_wpmm <- pool(fit_wpmm)
summary(estimate_wpmm) #age is not a significant variable

##LOG TEST

#PMM
### - Wald test to test significance of regression coefficients -
complete_pmm <- complete(imp_mice_pmm, 1)
summary(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_pmm))$coefficients
### - Deviance -
summary(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_pmm))$deviance
#233.7264
### - Pseudo R? -
pR2(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_pmm))
#We only have a small value of 0.1528244 which means that we can only explain this part of the deviance by the model

#------------------------PMM analysis ROC curve
names(complete_pmm)
glm_pmm <- glm(survived ~ class + sex + age, family = binomial(link = logit), data = complete_pmm)

pred_m1 <- prediction(fitted(glm_pmm), complete_pmm$survived)
perf_m1 <- performance(pred_m1, measure = "tpr", x.measure = "fpr")
plot(perf_m1, main = "Sensitivity vs False Positive Rate", colorize = TRUE, colorkey.relwidth = 0.5, lwd = 4.5)
performance(pred_m1, measure = "auc")@y.values
#Area under the curve. This model explains 75.9% of all data

table(complete_pmm$survived, fitted(glm_pmm)>0.4) #unnecessary
#----------------------


#Lasso Regression
complete_lassonorm <- complete(imp_mice_lassonorm, 1)
summary(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_lassonorm))$coefficients
### - Deviance -
summary(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_lassonorm))$deviance
#234.2229
### - Pseudo R? -
pR2(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_lassonorm))
#We only have a small value of 0.1552194 which means that we can only explain this part of the deviance by the model

#------------------------ analysis ROC curve
names(complete_lassonorm)
glm_lassonorm <- glm(survived ~ class + sex + age, family = binomial(link = logit), data = complete_lassonorm)

pred_m2 <- prediction(fitted(glm_lassonorm), complete_lassonorm$survived)
perf_m2 <- performance(pred_m2, measure = "tpr", x.measure = "fpr")
plot(perf_m2, main = "Sensitivity vs False Positive Rate, Lasso", colorize = TRUE, colorkey.relwidth = 0.5, lwd = 4.5)
performance(pred_m2, measure = "auc")@y.values
#Area under the curve. This model explains 75.5% of all data


#Bayesian linear regression
complete_norm <- complete(imp_mice_norm, 1)
summary(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_norm))$coefficients
### - Deviance -
summary(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_norm))$deviance
#228.8713
### - Pseudo R? -
pR2(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_norm))
#We only have a small value of 0.1745212 which means that we can only explain this part of the deviance by the model

#------------------------ analysis ROC curve
names(complete_norm)
glm_norm <- glm(survived ~ class + sex + age, family = binomial(link = logit), data = complete_norm)

pred_m3 <- prediction(fitted(glm_norm), complete_norm$survived)
perf_m3 <- performance(pred_m3, measure = "tpr", x.measure = "fpr")
plot(perf_m3, main = "Sensitivity vs False Positive Rate, Bayesian", colorize = TRUE, colorkey.relwidth = 0.5, lwd = 4.5)
performance(pred_m3, measure = "auc")@y.values
#Area under the curve. This model explains 77.3% of all data



#Weighted predictive mean matching
complete_wpmm <- complete(imp_mice_wpmm, 1)
summary(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_wpmm))$coefficients
### - Deviance -
summary(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_wpmm))$deviance
#232.2783
### - Pseudo R? -
pR2(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_wpmm))
#We only have a small value of 0.1622332 which means that we can only explain this part of the deviance by the model

#------------------------ analysis ROC curve
names(complete_wpmm)
glm_wpmm <- glm(survived ~ class + sex + age, family = binomial(link = logit), data = complete_wpmm)

pred_m4 <- prediction(fitted(glm_wpmm), complete_wpmm$survived)
perf_m4 <- performance(pred_m4, measure = "tpr", x.measure = "fpr")
plot(perf_m4, main = "Sensitivity vs False Positive Rate, WPMM", colorize = TRUE, colorkey.relwidth = 0.5, lwd = 4.5)
performance(pred_m4, measure = "auc")@y.values
#Area under the curve. This model explains 78.65% of all data


#Compare all 4 methods in one graph
plot(perf_m1, colorize = FALSE, lwd = 3, col = "red", main = "Sensitivity vs False Positive Rate")
legend("bottomright", c("Predictive Mean Matching", "Lasso Linear Regression", "Bayesian linear regression", "Weighted predictive mean matching"), lty=1, 
       col = c("red", "blue", "green", "orange"), bty="n")
plot(perf_m2, add = TRUE, colorize = FALSE, lwd = 3, col="blue")
plot(perf_m3, add = TRUE, colorize = FALSE, lwd = 3, col ="green")
plot(perf_m4, add = TRUE, colorize = FALSE, lwd = 3, col = "orange")
abline(0, 1, lty = 2)
#----------------------

###################################
#could plot summaries from each the mice models
#plot estimate w/ std error and color by approach
##################################
#More tests? Can try these outputs. 
#AMELIA

library(Amelia)
library(tidyverse)

#?amelia()


titanic1 <- read.csv("Titanic-incomplete.txt", header = T, sep = " ")

head(titanic1)
#run amerlia on data
imp_amelia <- amelia(titanic1, m= 5)
#histogram for age
hist(imp_amelia$imputations[[2]]$age)



####melding imputations together
#obtain nested data
all_imputations <- bind_rows(unclass(imp_amelia$imputations), .id = "m") %>%
  group_by(m) %>%
  nest()

all_imputations

#run models
models_imputations <- all_imputations %>%
  mutate(model = data %>% map(~ glm(survived ~ class + sex + age, family = binomial(link = logit),  data = .)),
         tidied = model %>% map(~ tidy(., conf.int = TRUE)),
         glance = model %>% map(~ glance(.)))

models_imputations

#make coefficients accessible
models_imputations %>%
  filter(m == "imp1") %>%
  unnest(tidied)

#melding imputations together
params <- models_imputations %>%
  unnest(tidied) %>%
  select(m, term, estimate, std.error) %>%
  gather(key, value, estimate, std.error) %>%
  spread(term, value) %>% 
  ungroup()
params

models_imputations %>%
  unnest(tidied)
# Extract just the coefficients
just_coefs <- params %>%
  filter(key == "estimate") %>%
  select(-m, -key)
just_coefs

# Extract just the standard errors
just_ses <- params %>%
  filter(key == "std.error") %>%
  select(-m, -key)
just_ses



coefs_melded <- mi.meld(just_coefs, just_ses)
coefs_melded


#time to meld
model_degree_freedom <- models_imputations %>%
  unnest(glance) %>%
  filter(m == "imp1") %>%
  pull(df.residual)

melded_summary <- as.data.frame(cbind(t(coefs_melded$q.mi),
                                      t(coefs_melded$se.mi))) %>%
  magrittr::set_colnames(c("estimate", "std.error")) %>%
  mutate(term = rownames(.)) %>%
  select(term, everything()) %>%
  mutate(statistic = estimate / std.error,
         conf.low = estimate + std.error * qt(0.025, model_degree_freedom),
         conf.high = estimate + std.error * qt(0.975, model_degree_freedom),
         p.value = 2 * pt(abs(statistic), model_degree_freedom, lower.tail = FALSE))

melded_summary

########plotting
plot(imp_amelia)
compare.density(imp_amelia, var = "age")
imp_amelia

disperse(imp_amelia, m = 5, dims = 1, p2s = 0, frontend = FALSE, xlim = NULL, ylim = NULL)

missmap(imp_amelia)

##let's compare mice and amelia
miceC <- summary(glm(formula = survived ~ class + sex + age, family = binomial(link = logit), data = complete_pmm))$coefficients
miceCoef <- c(miceC[1], miceC[2], miceC[3], miceC[4])


#VIM <- CEA

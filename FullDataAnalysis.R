#Project 1: Stat for Bioinformatics

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
library("AICcmodavg")

######### Set up
#set wd
setwd("C:/Users/isaac/Education/UVM/R/KU/BioinformaticStats/Project1/StatsProject")

#read in completed data 
titanicC = read.csv("Titanic-complete.txt", header = T, sep = " ")

#check to see if data present
head(titanicC)

#make sure header and names were formatted correctly
names(titanicC)

######### Descriptive Statistics
descrip.titanicC <- stat.desc(titanicC[,names(titanicC)],basic=TRUE, desc=TRUE)
descrip.titanicC

covar.titanicC <- cor(titanicC)
covar.titanicC
#highest covar is between class and survived
#followed by class and sex

######### Logistic Regression
titanicC.log<-glm(survived ~ class+sex+age,data=titanicC, family=binomial(link="logit"))
summary(titanicC.log)
#can see from output that both class and sex are statistical
#significant in predicting survival 

##### Survival probability

###sex
#first construct logisitc regression to determine
#the survival probability for a woman
titanic.survival.sex <- glm(survived ~ sex,data=titanicC, family=binomial(link="logit"))
summary(titanic.survival.sex)
#intercept = -0.397
#slope = 0.811
survival.female <- (exp(-0.397 + (0.811 * 1))) / (1 + exp(-0.397 + (0.811 * 1)))
survival.female
#can calculate the odds of survival for females
death.female <- 1- survival.female
odds.survival.female <- survival.female / death.female
odds.survival.female

#odds of survival for males
survival.male <- (exp(-0.397 + (0.811 * 0))) / (1 + exp(-0.397 + (0.811 * 0)))
survival.male
death.male <- 1 - survival.male
odds.survival.male <- survival.male / death.male
odds.survival.male

#odds ratio
OR.sex <- odds.survival.female / odds.survival.male
OR.sex

###class
titanic.survival.class <- glm(survived ~ class,data=titanicC, family=binomial(link="logit"))
summary(titanic.survival.class)
#intercept = -0.780
#slope = 1.653
survival.class <- (exp(-0.780 + (1.653 * 1))) / (1 + exp(-0.780 + (1.653 * 1)))
survival.class

death.class = 1- survival.class
odds.survival.class <- survival.class / death.class
odds.survival.class

#Odds for non first class
survival.nonFirst <- (exp(-0.780 + (1.653 * 0))) / (1 + exp(-0.780 + (1.653 * 0)))
survival.nonFirst
death.nonFirst <- 1 - survival.nonFirst
odds.survival.nonFirst <- survival.nonFirst / death.nonFirst
odds.survival.nonFirst

#odds ratio
OR.class <- odds.survival.class / odds.survival.nonFirst
OR.class



###sex and class
titanic.survival.class.sex <- glm(survived ~ sex + class, data = titanicC, family = binomial(link = "logit"))
summary(titanic.survival.class.sex)
#intercept = -1.358
#slope sex = 1.042
#slope class = 1.793
survival.class.sex <- (exp(-1.358 + (1.042 * 1) + (1.793*1))) / (1 + exp(-1.358 + (1.042 * 1) + (1.793*1)))
survival.class.sex

death.class.sex = 1 - survival.class.sex
odds.survival.class.sex <- survival.class.sex / death.class.sex
odds.survival.class.sex

#Odds for non first class and non female
survival.nonFirst.male <- (exp(-1.358 + (1.042 * 0) + (1.793*0))) / (1 + exp(-1.358 + (1.042 * 0) + (1.793*0)))
survival.nonFirst.male
death.nonFirst.male <- 1 - survival.nonFirst.male
odds.survival.nonFirst.male <- survival.nonFirst.male / death.nonFirst.male
odds.survival.nonFirst.male

#odds ratio
OR.class.sex <- odds.survival.class.sex / odds.survival.nonFirst.male
OR.class.sex


#construct table
odds.df <- data.frame(Variable = c("sex", "class", "class and sex"), 
                      Odds = c(odds.survival.female, odds.survival.class, odds.survival.class.sex),
                      Odd.Ratio = c(OR.sex, OR.class, OR.class.sex))
odds.df
plot.new()
grid.table(odds.df)

#######SLIDE 124 -> he uses this command
#OR represents the OR of just that variable
#notice how OR column multiplied equals Odds col of odds.df
#and how the OR value for the variable = Odds.ratio of df
exp(cbind(OR =titanic.survival.sex$coefficients, confint(titanic.survival.sex)))
exp(cbind(OR =titanic.survival.class$coefficients, confint(titanic.survival.class)))
exp(cbind(OR =titanic.survival.class.sex$coefficients, confint(titanic.survival.class.sex)))

########## Visualization
titanicC
#make bar charts to visually confirm probabilities
#calculated above

##Sex
titanicC.mod <- titanicC %>%
  mutate(sex = ifelse(sex == "0", "male", "female")) %>%
  mutate(class = ifelse(class == "0", "not 1st class", "1st class"))  %>%
  mutate(survived = ifelse(survived == "0", "died", "survived"))

titanicC.mod

#sex and survivor
ggplot(data = titanicC.mod, aes(x = survived)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = sex)) + 
  ylab("proportion") +
  ggtitle("Proportion of Survivors \nGrouped by Sex")

#class and survivor
ggplot(data = titanicC.mod, aes(x = survived)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = class)) + 
  ylab("proportion") +
  ggtitle("Proportion of Survivors \nGrouped by Class")

#class, sex and survivor
ggplot(data = titanicC.mod, aes(x = survived)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = class, alpha = sex)) + 
  ylab("proportion") +
  ggtitle("Proportion of Survivors \nGrouped by Class and Sex")



##Since we will be predicting age for missing values
##Let's see if there is anything w/ that
ggplot(titanicC, aes(y= age, group = sex, fill = factor(sex))) +
  geom_boxplot(aes())


ggplot(titanicC.mod, aes(x = age)) +
  geom_histogram() + 
  facet_nested(class + sex ~ survived) +
  ggtitle("Comparison of Age Faceted By \nSurvival and Nested Class and Sex")



############TOOO Work ON

#####More models!

#How can i generate a model here that could be useful?

#this one includes an interaction between sex and class
#variable
log.titanic.class.sex <- glm(survived ~ sex*class, data = titanicC, family = binomial(link = "logit"))
summary(log.titanic.class.sex)

lm.titanic.class.sex <- lm(age ~ sex*class*survived, data = titanicC)
summary(lm.titanic.class.sex)





######Plotting Logit Curve
#useful functions
logit<-function(x)log(x/(1-x))
ilogit<-function(x,a,b)exp(a+b*x)/(1+exp(a+b*x))

##plotting age and survival grouped by sex
#generate logistic model for age, survival and sex
titanic.age.sex <- glm(survived ~ sex + class,data=titanicC, family=binomial(link="logit"))
cl.sex <- coef(titanic.age.sex)
titanicCModS <- titanicC$sex +1
titanicCModS
cl.sex
names(titanicC)
plot(titanicC$age,jitter(titanicC$survived,.2),col=titanicCModS,pch=20, cex=1.2,xlab="Sex",ylab="Survived (jittered)")
curve(ilogit(cl.sex[1]+cl.sex[2]*x+cl.sex[3]*0,0,1),add=T)
curve(ilogit(cl.sex[1]+cl.sex[2]*x+cl.sex[3]*1,0,1),add=T,col="red")
legend("topright",pch=20,lty="solid",col=c("red","black"),c("women","men"))

##Good plot?? Not really because age does not seem to matter
ggplot(titanicC, aes(x=age, y=survived, color = factor(sex))) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, fullrange=TRUE, 
              method.args = list(family=binomial)) +
  facet_grid(~class)




#class vs survived, grouped by sex
ggplot(titanicC, aes(x=class, y=survived, color = factor(sex))) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, fullrange=TRUE, 
              method.args = list(family=binomial))

#sex vs survived, grouped by class
ggplot(titanicC, aes(x=sex, y=survived, color = factor(class))) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, fullrange=TRUE, 
              method.args = list(family=binomial))

####PROBABLY NOT NEEDED
###APPLY AIC TO ALL MODELS
titanic.list <- list()

titanic.list[[1]] <- glm(survived ~ age, data = titanicC, family = binomial(link = "logit")) 
titanic.list[[2]] <- glm(survived ~ sex, data = titanicC, family = binomial(link = "logit")) 
titanic.list[[3]] <- glm(survived ~ class, data = titanicC, family = binomial(link = "logit")) 
titanic.list[[4]] <- glm(survived ~ age + sex, data = titanicC, family = binomial(link = "logit")) 
titanic.list[[5]] <- glm(survived ~ age + sex + class, data = titanicC, family = binomial(link = "logit")) 
titanic.list[[6]] <- glm(survived ~ sex + class, data = titanicC, family = binomial(link = "logit")) 
titanic.list[[7]] <- glm(survived ~ age + class, data = titanicC, family = binomial(link = "logit")) 
titanic.list[[8]] <- glm(survived ~ age * sex, data = titanicC, family = binomial(link = "logit")) 
titanic.list[[9]] <- glm(survived ~ age * class, data = titanicC, family = binomial(link = "logit")) 
titanic.list[[10]] <- glm(survived ~ class * sex, data = titanicC, family = binomial(link = "logit")) 
titanic.list[[11]] <- glm(survived ~ age * sex * class, data = titanicC, family = binomial(link = "logit")) 


titanic.aic = aictab(cand.set = titanic.list)
titanic.aic

titanic.list1 <- list()

titanic.list1[[1]] <- lm(age ~ survived, data = titanicC)
titanic.list1[[2]] <- lm(age ~ sex, data = titanicC) 
titanic.list1[[3]] <- lm(age ~ class, data = titanicC)
titanic.list1[[4]] <- lm(age ~ survived + sex, data = titanicC)
titanic.list1[[5]] <- lm(age ~ survived+ sex + class, data = titanicC)
titanic.list1[[6]] <- lm(age ~ sex + class, data = titanicC) 
titanic.list1[[7]] <- lm(age ~ survived + class, data = titanicC) 
titanic.list1[[8]] <- lm(age ~ survived * sex, data = titanicC) 
titanic.list1[[9]] <- lm(age ~ survived * class, data = titanicC) 
titanic.list1[[10]] <- lm(age ~ class * sex, data = titanicC) 
titanic.list1[[11]] <- lm(age ~ survived * sex * class, data = titanicC) 

titanic.aic1 = aictab(cand.set = titanic.list1)
titanic.aic1

#####OK we will be predicting age missing values
#####should build a model to predict them 
####sequential building of the model
ageT.lm <- lm(age~ sex + class + survived, data = titanicC)
ageT.anova <- anova(ageT.lm)
ageT.anova
ageT.lm



######Sphagetti Plot???
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


#-----------------Part 4: MI Analysis-------------------------------


#-----------------Part 5: IPW Analysis------------------------------



#-----------------Part 6: Comparing Analyses------------------------

####Generate odds ratio for class and sex for each data set
#CD
exp(cbind(OR =titanicC.glm$coefficients, confint(titanicC.glm)))

#CC

#MI

#IPW


#Plot comparing Age distribution
#put all age columns of datasets into one dataframe
all.age <- data.frame("CD" = titanicC$age, "CC" = titanicI$age)
all.ready <- melt(all.age)

#plot the distributions of ages by dataset
plt <- ggplot(data = all.ready, aes(x = variable, y = value, fill = variable)) + 
  geom_boxplot() + labs(x = "Dataset", y = "Age") +
  ggtitle("Spread of Ages By Dataset")

plt


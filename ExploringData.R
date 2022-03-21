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

######### Set up
#set wd
setwd("C:/Users/isaac/Education/UVM/R/KU/BioinformaticStats/Project1")

#read in completed data 
titanicC = read.csv("Titanic-complete.txt", header = T, sep = " ")

#read incomplete data
titanicI <- read.csv("Titanic-incomplete.txt", header = T, sep = " ")

#check to see if data present
head(titanicC)
head(titanicI)


########Exploring the missing data
#aggregation of which data is missing and counts
titanicI.aggr=aggr(titanicI,numbers=TRUE, prop=FALSE, ylab=c("Histogram of missing data","Pattern"))
titanicI.aggr

aggr(titanicI, combined=TRUE, numbers = TRUE, prop = TRUE, cex.numbers=0.87, varheight = FALSE)

## Amount of missigness in age for each survived group
barMiss(titanicI[,c("survived","age")], only.miss = F)

## Amount of missigness in age for each sex group
barMiss(titanicI[,c("sex","age")], only.miss = F)

## Amount of missigness in age for each class group
barMiss(titanicI[,c("class","age")], only.miss = F)

##histogram of ages
y <- titanicI[, c("age", "survived")]
histMiss(y, only.miss = FALSE, main ="Histogram of Passengers Ages with \nBar Plot Representing Observed and \nMissing Observations")


########Wald Test?


########Generating missing data using MI



###plot histograms of the calculated betas
#put a line for the "true" beta values


########Generating missing data using IPW



########Descriptive stats for all models
##make a vector of the names of the stats we care
#about seeing the most across the datasets
values <- c( "nbr.val","median","mean","CI.mean.0.95" ,"std.dev","min","max")
##CD
descrip.cd <- stat.desc(titanicC[,names(titanicC)],basic=TRUE, desc=TRUE)
descrip.cd[values,]

##CC
descrip.cc <- stat.desc(titanicI[,names(titanicC)],basic=TRUE, desc=TRUE)
descrip.cc[values,]

##MI


##IPW



########Calculating Odds Ratio for survival by age for 
########each of the datasets
##Odds ratio calculation



##visualizing the percentages for sex and class
####bar chart groupping the frequencies of survival by factors
ggplot(data = titanicC.mod, aes(x = survived)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = class, alpha = sex)) + 
  ylab("proportion") +
  ggtitle("Proportion of Survivors \nGrouped by Class and Sex")




########Generating logistic models for predicting survival
##CD
cd.log<-glm(survived ~ class+sex+age,data=titanicC, family=binomial(link="logit"))
summary(cd.log)

##CC
cc.log<-glm(survived ~ class+sex+age,data=titanicI, family=binomial(link="logit"))
summary(cc.log)

##MI


##IPW


#######Visualizing the different datasets
####histograms for age
hist(titanicC$age, main = "Histogram of Ages for \nNon-missing Value Dataset", 
     xlab = "age")                                   # Draw histogram
abline(v = mean(titanicC$age),                       # Add line for mean
       col = "red",
       lwd = 3)

hist(titanicI$age, main = "Histogram of Ages for \nComplete Cases Dataset", 
     xlab = "age")                                   # Draw histogram
abline(v = mean(titanicI$age, na.rm = T),                       # Add line for mean
       col = "red",
       lwd = 3)


####histograms showing age by other factors
#first need to change data such that it is easier
#for labeling plots
titanicC.mod <- titanicC %>%
  mutate(sex = ifelse(sex == "0", "male", "female")) %>%
  mutate(class = ifelse(class == "0", "not 1st class", "1st class"))  %>%
  mutate(survived = ifelse(survived == "0", "died", "survived"))
titanicI.mod <- titanicI %>%
  mutate(sex = ifelse(sex == "0", "male", "female")) %>%
  mutate(class = ifelse(class == "0", "not 1st class", "1st class"))  %>%
  mutate(survived = ifelse(survived == "0", "died", "survived"))


#original
ggplot(titanicC.mod, aes(x = age)) +
  geom_histogram() + 
  facet_nested(class + sex ~ survived) +
  ggtitle("Comparison of Age Faceted By \nSurvival and Nested Class and Sex")


#CC
ggplot(titanicI.mod, aes(x = age)) +
  geom_histogram() + 
  facet_nested(class + sex ~ survived) +
  ggtitle("Comparison of Age Faceted By \nSurvival and Nested Class and Sex")

####box plots
#put all age columns of datasets into one dataframe
all.age <- data.frame("original" = titanicC$age, "CC" = titanicI$age)
all.ready <- melt(all.age)

plt <- ggplot(data = all.ready, aes(x = variable, y = value, fill = variable)) + 
  geom_boxplot() + labs(x = "Dataset", y = "Age") +
  ggtitle("Spread of Ages By Dataset")

plt

####creating logisitic regressino plots faceted by 
####sex and groupped by class

#Good plot?? Not really because age does not seem to matter

#original
ggplot(titanicC, aes(x=age, y=survived, color = factor(sex))) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, fullrange=TRUE, 
              method.args = list(family=binomial)) +
  facet_grid(~class)

ggplot(titanicI, aes(x=age, y=survived, color = factor(sex))) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, fullrange=TRUE, 
              method.args = list(family=binomial)) +
  facet_grid(~class)


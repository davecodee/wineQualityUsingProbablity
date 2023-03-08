library(dplyr)
library(plyr)
library(ggplot2)
library(tidyverse)
library(gginference)
library(corrplot)
library(RColorBrewer)
library(psych)

#importing dataset
wine<-read.csv("winequalityN.csv")

#Summary
summary(wine)
str(wine)
temp<-headtail(wine)
View(temp)
is.na.data.frame(wine)

#checking outlier and data cleaning
boxplot(wine$pH,main="Outlier Detection", ylab="pH") ##Not much outlier found
wine<-sapply(wine, unclass)

df<-na.omit(wine)
str(df)

summary(df)
describe(df)
#Subsetting
subsetdata <- subset(df ,select=c(type,free.sulfur.dioxide, total.sulfur.dioxide, density,alcohol, 
                          residual.sugar,quality))
subsetdata <-as.data.frame(subsetdata)



#Visualization

ggplot(subsetdata,aes(y=residual.sugar, x=total.sulfur.dioxide))+
  geom_point(position="identity", stat="identity")+ggtitle("How sulfur dioxide affects residual sugar")+
  xlab("Total sulfur Dioxide")+
  ylab("Residual sugar")

ggplot(subsetdata,aes(y=alcohol, x=density))+
  geom_point(position="identity", stat="identity")+ggtitle("How density affects alcohol")+
  xlab("Density")+
  ylab("Alcohol")

ggplot(subsetdata,aes(y=residual.sugar, x=density))+
  geom_point(position="identity", stat="identity")+ggtitle("How density affects residual sugar")+
  xlab("density")+
  ylab("Residual sugar")

ggplot(subsetdata,aes(y=quality, x=alcohol))+
  geom_bar(position="dodge", stat="identity")+ggtitle("How quality affects alcohol")+
  xlab("Alcohol")+
  ylab("Quality")

#Hypothesis testing
#H0:There are no effects on quality with respect to alcohol.
attach(subsetdata)
boxplot(alcohol~quality)
t.test(alcohol[quality <= 6],alcohol[quality> 6])

#H0: There is no difference in quality in type of wine.
attach(as.data.frame(df))
boxplot(quality~type)
t.test(quality~type)

#H0: Students score greater or equal to 50
#HA: Students score less than 50
boxplot(final_test,xlab="Final Test")
t.test(final_test)


#Linear Model and correlation
cor1<-cor(as.data.frame(subsetdata))

corrplot(cor1)

lm_model<- lm(residual.sugar~total.sulfur.dioxide , data = subsetdata )
lm_model1<-lm(alcohol~density, data = subsetdata)
lm_model2<-lm(residual.sugar~density)
subsetdata<-as.data.frame(subsetdata)

ggplot(subsetdata,aes(y=residual.sugar, x=total.sulfur.dioxide))+
  geom_point(position="identity", stat="identity")+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=F) +
  ggtitle("How sulfur dioxide affects residual sugar")+
  xlab("Total sulfur Dioxide")+
  ylab("Residual sugar")

ggplot(subsetdata,aes(y=alcohol, x=density))+
  geom_point(position="identity", stat="identity")+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=F) +
  ggtitle("How density affects alcohol")+
  xlab("Density")+
  ylab("Alcohol")

ggplot(subsetdata,aes(y=residual.sugar, x=density))+
  geom_point(position="identity", stat="identity")+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=F) +
  ggtitle("How density affects residual sugar")+
  xlab("density")+
  ylab("Residual sugar")

#summary of Linear regression
summary(lm_model)
summary(lm_model1)
summary(lm_model2)

library(dsEssex)
library(AER)
library(tidyverse)
library(na.tools)
library(dplyr)
library(MASS)
library(wesanderson)
library(psych)
library(gridExtra)
library(grid)
library(patchwork)
library(vtable)
library(GGally)
library(tableone)
library(xtable)

data("Affairs")
summary(Affairs)
str(Affairs)

numaffairs <- Affairs %>%
  select_if(negate(is.factor))


#UNIVARIATE ANALYSIS#

hist(Affairs$affairs, # histogram
     col="grey", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Number of affairs",
     main = "Histogram plot and density function of number of affairs")
lines(density(Affairs$affairs), # density plot
      lwd = 2, # thickness of line
      col = "red")

Affairs$affairs <- as.factor(Affairs$affairs)
Affairs$religiousness <- as.factor(Affairs$religiousness)
Affairs$occupation <- as.factor(Affairs$occupation)
Affairs$rating <- as.factor(Affairs$rating)
Affairs$age <- as.factor(Affairs$age)
Affairs$education <- as.factor(Affairs$education)
Affairs$yearsmarried <- as.factor(Affairs$yearsmarried)

#GenderBox
B1 <- ggplot(Affairs, aes(fill=affairs, x=gender)) +
  geom_bar(position = "stack") +
  xlab("Gender") + ylab("Number") + guides(fill=guide_legend(title="Number of affairs")) +
  theme(legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=7)) + #change legend text font size
  scale_fill_brewer(palette="Set2")
#AgeBox
B2 <- ggplot(Affairs, aes(fill=affairs, x=age)) +
  geom_bar(position = "stack") +
  xlab("Age") + ylab("Number") + guides(fill=guide_legend(title="Number of affairs")) +
  theme(legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=7)) + #change legend text font size
  scale_fill_brewer(palette="Set2")
#yearsMarried
B3 <- ggplot(Affairs, aes(fill=affairs, x=yearsmarried)) +
  geom_bar(position = "stack") +
  xlab("Years married") + ylab("Number") + guides(fill=guide_legend(title="Number of affairs")) +
  theme(legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=7)) + #change legend text font size
  scale_fill_brewer(palette="Set2")
#ChildrenBox
B4 <- ggplot(Affairs, aes(fill=affairs, x=children)) +
  geom_bar(position = "stack") +
  xlab("Children") + ylab("Number") + guides(fill=guide_legend(title="Number of affairs")) +
  theme(legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=7)) + #change legend text font size
  scale_fill_brewer(palette="Set2")
#ReligiousnessBox
B5 <- ggplot(Affairs, aes(fill=affairs, x=religiousness)) +
  geom_bar(position = "stack") +
  xlab("Religiousness") + ylab("Number") + guides(fill=guide_legend(title="Number of affairs")) +
  theme(legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=7)) + #change legend text font size
  scale_fill_brewer(palette="Set2")
#EducationBox
B6 <- ggplot(Affairs, aes(fill=affairs, x=education)) +
  geom_bar(position = "stack") +
  xlab("Education") + ylab("Number") + guides(fill=guide_legend(title="Number of affairs")) +
  theme(legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=7)) + #change legend text font size
  scale_fill_brewer(palette="Set2")
#OccupationBox
B7 <- ggplot(Affairs, aes(fill=affairs, x=occupation)) +
  geom_bar(position = "stack") +
  xlab("Occupation") + ylab("Number") + guides(fill=guide_legend(title="Number of affairs")) +
  theme(legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=7)) + #change legend text font size
  scale_fill_brewer(palette="Set2")
#MarriageRatingBox
B8 <- ggplot(Affairs, aes(fill=affairs, x=rating)) +
  geom_bar(position = "stack") +
  xlab("Self-rating of Marriage") + ylab("Number") + guides(fill=guide_legend(title="Number of affairs")) +
  theme(legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=7)) + #change legend text font size
  scale_fill_brewer(palette="Set2")

#Combining Uni-variate plots
grid.arrange(B1,B2,B3,B4,B5,B6,B7,B8, nrow=4, ncol=2)

#EDA
table(Affairs$affairs)
table(Affairs$age)
table(Affairs$yearsmarried)
table(Affairs$religiousness)
table(Affairs$education)
table(Affairs$occupation)
table(Affairs$rating)

#CORRELATION TABLE
corraffairs <- round(cor(numaffairs),5)
upper<- corraffairs
upper[upper.tri(corraffairs)]<-""
upper<-as.data.frame(upper)
print(xtable(upper), type="html", file = "graph.html")

#LOGISTIC REGRESSION
Affairs$ynaffair[Affairs$affairs >  0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair,levels=c(0,1), labels=c("No","Yes"))
table(Affairs$ynaffair)

fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children + religiousness + education + occupation +rating, data=Affairs, family=binomial())
summary(fit.full)

fit.reduced <- glm(ynaffair ~ age + yearsmarried + religiousness + rating, data=Affairs, family=binomial())
summary(fit.reduced)

anova(fit.reduced, fit.full, test="Chisq") 

coef(fit.reduced)
exp(coef(fit.reduced))

#Impact of self-rating 
testdata <- data.frame(rating=c(1, 2, 3, 4, 5), age=mean(Affairs$age),
             yearsmarried=mean(Affairs$yearsmarried),
             religiousness=mean(Affairs$religiousness))

testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
#Impact of Age
testdata <- data.frame(rating=mean(Affairs$rating), age=seq(17, 57, 10), 
                       yearsmarried=mean(Affairs$yearsmarried),
                       religiousness=mean(Affairs$religiousness))
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")

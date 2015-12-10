##Setup
rm(list = ls())
setwd("/home/robert/all_Robert_asus/prog_asus/R/econ510_final/")

#libraries needed
library(readr)
library(sampleSelection)
library(stargazer)
library(MASS)

creditdf <- read_csv("AmEx1.csv")
str(creditdf)
# ## change qualitative vars if I have time to try
# dfNum <- creditdf
# 
# creditdf$
### end setup

## selection of vars
## using a linear model to select variables with significance
m1 <- lm(CARDHLDR ~.,data = creditdf)
summary(m1)

m2 <- lm(DEFAULT ~.,data = creditdf)
summary(m2)
#made no difference
#creditdf$PROF <- factor(creditdf$PROF)
ml1 <- selection(DEFAULT~.-CARDHLDR,CARDHLDR~.-DEFAULT,data = creditdf)
summary(ml1)

## could spend some time eliminating some of these that are highly correlated
cor <- cor(creditdf) 
cor[cor > .5]
############# NOT RIGHT #######################



# ## change qualitative vars if I have time to try
# dfNum <- creditdf
# 
# creditdf$
## Variables significant for default

### just going through the motions to get something -- I think this is probit or logit

glm1 <- glm(DEFAULT~.-CARDHLDR,data = creditdf,family = binomial(link="logit"))
summary(glm1)

## with the cardholder var left in (don't really think this is it, but I need to move on)

glm2 <- glm(DEFAULT~.-CARDHLDR,data = creditdf,family = binomial(link="probit"))
summary(glm1)

### I don't think this is it but I'm going with it 
glm3 <- glm(DEFAULT~.-CARDHLDR,data = subset(creditdf,creditdf$CARDHLDR == 1),family = binomial(link="logit"))
summary(glm3)

## only not accepted (NO significance)
glm4 <- glm(DEFAULT~.-CARDHLDR,data = subset(creditdf,creditdf$CARDHLDR == 0),family = binomial(link="logit"))
summary(glm4)


## still have no idea how to do the joint table but here is a non joint table
#CARDHLDR
glm5 <- glm(CARDHLDR~.-DEFAULT,data = creditdf,family = binomial(link = "logit"))
summary(glm5)

##with default
glm5 <- glm(CARDHLDR~.,data = creditdf,family = binomial(link = "logit"))
summary(glm5)

### printing the tables
##table 1
stargazer(glm5,type = "text",title = "Cardholder Equation")
##table 2
stargazer(glm1,glm3,type = "text",style = "qje",title = "Conditional and Unconditional default rates")

##discriminant analysis
#linear
mda1 <- lda(DEFAULT ~.-CARDHLDR,data = creditdf)
mda1
#quadratic
mda2 <- qda(DEFAULT ~.-CARDHLDR,data = creditdf)
mda2

## dividing data into training set and test set for models


# import libraries
library(dplyr)
library(GGally)
library(corrplot)

# import data
data.ori = read.csv("customer-personality.csv")

# summary of data
str(data.ori)

# check for NA
sum(is.na(data.ori))

# check which columns contain NA
apply(is.na(data.ori), 2, which)

# remove NA rows
data.clean = data.ori[which(!is.na(data.ori$Income)),]
str(data.clean)
sum(is.na(data.clean))

# remove outliers from income 
data.clean <- data.clean[!(data.clean$Income >=200000),]

# filter numerical columns
numerical.data = data.clean[, c(2,5:7,9:ncol(data.ori))]
str(numerical.data)

# plot correlation matrix
corrplot(cor(numerical.data), tl.col = "black", diag = FALSE, 
         method = 'number', type = 'upper')

# # inefficient to use this, too much data
# ggpairs(numerical.data)#, aes(color = class, alpha = 0.5))


# converting 'Education' and 'Marital status' variables to categorical variables
## Education
data.clean$Education[data.clean$Education == "Basic"] <- "1"
data.clean$Education[data.clean$Education == "Graduation"] <- "2"
data.clean$Education[data.clean$Education == "2n Cycle"] <- "3"
data.clean$Education[data.clean$Education == "Master"] <- "3"
data.clean$Education[data.clean$Education == "PhD"] <- "4"
data.clean$Education <- as.numeric(data.clean$Education)

## Marital Status - Merging YOLO, Alone, and Absurd to add to single
data.clean$Marital_Status[data.clean$Marital_Status == "Absurd"] <- "1"
data.clean$Marital_Status[data.clean$Marital_Status == "Alone"] <- "1"
data.clean$Marital_Status[data.clean$Marital_Status == "YOLO"] <- "1"
data.clean$Marital_Status[data.clean$Marital_Status == "Single"] <- "1"
data.clean$Marital_Status[data.clean$Marital_Status == "Together"] <- "2"
data.clean$Marital_Status[data.clean$Marital_Status == "Married"] <- "3"
data.clean$Marital_Status[data.clean$Marital_Status == "Divorced"] <- "4"
data.clean$Marital_Status[data.clean$Marital_Status == "Widow"] <- "5"
data.clean$Marital_Status <- as.numeric(data.clean$Marital_Status)

# Adding Age variable (EXPLAIN)
data.clean$Age <- (2014 - data.clean$Year_Birth)

# Figuring out 'Dt_Customer' dataset
data.clean$Dt_Customer <- as.Date.numeric(data.clean$Dt_Customer)


######## Factor Analysis ########

### determine the number of factors of FA
# We want to identify the component of the correlation structure

data.clean$Dt_Customer <- NULL


fa.cor = cor(data.clean)


# looking at fa.cor
# let's look at eigenvector of the correlation to see how many factors we use

fa.eigen = eigen(fa.cor)
fa.eigen$values
# in decreasing values
# higher = more important
# rule of thuumb to see the number of variables we should use? Do the sum

sum(fa.eigen$values)


cumsum(fa.eigen$values)/19
# seeing this, using the first 4, we can explain 79% of the data
# draw screen plot for better visualisation


# use the scree plot
plot(fa.eigen$values, type = "b", ylab = "Eigenvalues", xlab = "Factor") # we choose 4
plot(cumsum(fa.eigen$values)/17, type = "b", ylab = "Eigenvalues", xlab = "Factor") # we choose 4

# maybe use 7?
# Why we use 4
# Because after 4, the decrease in the eigenvalue is much smaller, therefore we choose 4

##############################################
### factor analysis
fa.res = factanal(x = data.clean, factors = 7, rotation = "none") # factor = 4 because of our eigenvalues easlier
fa.res

#uniqueness = epsilon values
# loadings
# We have proportion variance explained by these loadings
# hypothesis test to see whether 4 factors are sufficient
# here, our p value is high therefore we don't reject that 4 factors are sufficient


##############################################
### factor rotation
fa.res = factanal(x = data.clean, factors = 4, rotation = "promax")
# promax belongs to the oblique rotation?

print(fa.res, cut = 0.2)
# we only show the factors with a loading of higher than 0.2
# therefore only selecting the variables with large loading
# you can see that paragraph, sentence and wordm is with factor 1
##############################################
### factor scores
fa.res = factanal(x = data.clean, factors = 7, rotation = "promax", scores = "Bartlett")
head(fa.res$scores)
summary(lm(Factor2 ~ Factor1, data = as.data.frame(fa.res$scores)))


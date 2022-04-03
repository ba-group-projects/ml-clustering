# import libraries
library(dplyr)
library(GGally)
library(corrplot)
library(lubridate)
library(forcats)
library(readr)
library(reshape)

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

# remove outliers from income and birth year
## income
data.clean <- data.clean[!(data.clean$Income >=200000),]

## birth year
data.clean <- data.clean[!(data.clean$Year_Birth <=1901),]


# converting 'Education', 'Marital status', and 'Dt_Customer' variables to categorical variables
## Education - Creating 3 categories
data.clean$Education[data.clean$Education == "Basic"] <- "1"
data.clean$Education[data.clean$Education == "Graduation"] <- "2"
data.clean$Education[data.clean$Education == "2n Cycle"] <- "3"
data.clean$Education[data.clean$Education == "Master"] <- "3"
data.clean$Education[data.clean$Education == "PhD"] <- "3"
data.clean$Education <- as.numeric(data.clean$Education)

## Marital Status - Creating 2 categories of Single and together
data.clean$Marital_Status[data.clean$Marital_Status == "Absurd"] <- "1"
data.clean$Marital_Status[data.clean$Marital_Status == "Alone"] <- "1"
data.clean$Marital_Status[data.clean$Marital_Status == "YOLO"] <- "1"
data.clean$Marital_Status[data.clean$Marital_Status == "Single"] <- "1"
data.clean$Marital_Status[data.clean$Marital_Status == "Together"] <- "2"
data.clean$Marital_Status[data.clean$Marital_Status == "Married"] <- "2"
data.clean$Marital_Status[data.clean$Marital_Status == "Divorced"] <- "1"
data.clean$Marital_Status[data.clean$Marital_Status == "Widow"] <- "1"
data.clean$Marital_Status <- as.numeric(data.clean$Marital_Status)

## Dt_customer
data.clean$Dt_Customer <- dmy(data.clean$Dt_Customer)
data.clean$Dt_Customer <- year(data.clean$Dt_Customer)
data.clean$Dt_Customer <- as.factor(data.clean$Dt_Customer)

data.clean$Dt_Customer <- fct_collapse(data.clean$Dt_Customer,
                                      "3" = "2014",
                                      "1" = "2012",
                                      "2" = "2013")

data.clean$Dt_Customer <- as.numeric(levels(data.clean$Dt_Customer))[data.clean$Dt_Customer]

# Adding Age variable 
data.clean$Age <- (2015 - data.clean$Year_Birth)

# filter numerical columns
numerical.data = data.clean[, c(3:21)]#:ncol(data.ori))]
str(numerical.data)

# plot correlation matrix
# corrplot(cor(numerical.data), tl.col = "black", diag = FALSE, 
         # method = 'number', type = 'upper')

# cor_1 <- round(cor(numerical.data, use = "pairwise.complete.obs"),2)

# corrplot.mixed(cor_1, lower = "number", upper = "ellipse", lower.col = "black", tl.cex=0.5)

corr_simple <- function(data=numerical.data,sig=0.5){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=T, tl.col="black", method = 'number', na.label=" ", number.cex=0.8 ,tl.cex=0.8)
}
corr_simple()

# # inefficient to use this, too much data
# ggpairs(numerical.data)#, aes(color = class, alpha = 0.5))

######## Factor Analysis ########

### determine the number of factors of FA
# We want to identify the component of the correlation structure

# Remove Dt_customer for now 
# data.clean$Dt_Customer <- NULL


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
# plot(cumsum(fa.eigen$values)/17, type = "b", ylab = "Eigenvalues", xlab = "Factor") # we choose 4


##############################################
### factor analysis
# Choosing 6 as it explains 71.4% of the data
fa.res = factanal(x = data.clean, factors = 7, rotation = "none") # factor = 4 because of our eigenvalues easlier
fa.res

#uniqueness = epsilon values
# loadings
# We have proportion variance explained by these loadings
# hypothesis test to see whether 4 factors are sufficient
# here, our p value is high therefore we don't reject that 4 factors are sufficient


##############################################
### factor rotation
fa.res = factanal(x = data.clean, factors = 6, rotation = "promax")
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



####### PCA ########


# # briefly examine the data
# apply(data.clean, 2, mean)
# apply(data.clean, 2, var)
# apply PCA

# Apply PCR to data
pr.out = prcomp(data.clean, scale = TRUE)

# # trying out PCA with first 7 columns
# pr.out = prcomp(data.freq, scale = TRUE)
names(pr.out)
# have a look at the output
pr.out$center
pr.out$scale
pr.out$rotation
# get the PC vector
dim(pr.out$x)
# plot the PCs
biplot(pr.out, scale = 0,cex=0.5)
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale =0,cex=0.5)
# check the variance explained by each PC
pr.out$sdev
pr.var = pr.out$sdev ^2
pr.var
# proportion of variance explained
pve = pr.var/sum(pr.var)
pve
cumsum(pve)
plot(pve, xlab = " Principal Component", ylab = "Proportion of
Variance Explained", ylim = c(0,1), type = "b")

plot(cumsum(pve), xlab = "Principal Component", ylab ="
Cumulative Proportion of Variance Explained", ylim = c(0,1),
     type = "b")

# Plotting PC1 and PC2
PC1 <- pr.out$rotation[,1]

pca_1_2 <- data.frame(pr.out$x[, 1:2])

plot(pca_1_2[,1], pca_1_2[,2])

"
PCA - 
from TDS:
This plot clearly shows how instead of the 8 columns given to us in the dataset,
only two were enough to understand we had three different types of pizzas,
thus making PCA a successful analytical tool to reduce high-dimensional 
data into a lower one for modelling and analytical purposes.

our_results
PCA may not be a suitable task, as it is unable for us to clearly understand the 
number and identity of segments in our customer dataset. 


"

# Plotting importance of each variable
PC1 <- pr.out$rotation[,1]
PC1_scores <- abs(PC1)
PC1_scores_ordered <- sort(PC1_scores, decreasing = TRUE)
names(PC1_scores_ordered)

# Plotting 3d
# install.packages("scatterplot3d")
# 
# library(scatterplot3d)


pca_1to3 <- data.frame(pr.out$x[, 1:3])
scatterplot3d(pca_1to3,
              main="3D Scatter Plot",
              xlab = "PCA1",
              ylab = "PCA2",
              zlab = "PCA3", angle=60)

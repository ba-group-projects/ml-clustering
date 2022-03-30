
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

# filter numerical columns
numerical.data = data.clean[, c(2,5:7,9:ncol(data.ori))]
str(numerical.data)

# plot correlation matrix
corrplot(cor(numerical.data), tl.col = "black", diag = FALSE, 
         method = 'number', type = 'upper')

# # inefficient to use this, too much data
# ggpairs(numerical.data)#, aes(color = class, alpha = 0.5))

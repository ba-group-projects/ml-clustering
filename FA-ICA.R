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

################ ICA #############################
library(fastICA)
set.seed(20)
latent.5 = fastICA(data.clean, 5, fun = "logcosh", alpha = 1,
            row.norm = FALSE, maxit = 200,
            tol = 0.0001, verbose = TRUE)
lantent.5.S = latent$S
################# Kmeans #############################

kmean.model.4 = kmeans(lantent.5.S, 4, nstart = 10)
kmean.model.4.cluster = kmean.model.4$cluster
# clusplot(lantent.5.S,model$cluster, main="4 Cluster")
plot(lantent.5.S[,1], lantent.5.S[,2], col=kmean.model.4$cluster)



################ RMF #################################

rfm.R = data.clean$Recency
rfm.F = rowSums(data.clean[,15:19])
rfm.M = rowSums(data.clean[,9:14])
RFM = data.frame(rfm.R,rfm.F,rfm.M)

# scale the rfm
RFM.scaled = (RFM - min(RFM))/(max(RFM) - min(RFM))

# plot the rfm in different clusters


scatterplot3d(RFM.scaled, kmean.model.4$cluster, main="4 Cluster")
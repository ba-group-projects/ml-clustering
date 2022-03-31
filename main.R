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

# TODO 
# Remove Dt_customer for now
data.clean$Dt_Customer <- NULL 

######## Factor Analysis ########
######## ######## ######## ########

### determine the number of factors of FA
fa.cor = cor(data.clean)
fa.eigen = eigen(fa.cor)
fa.eigen$values
sum(fa.eigen$values)

# Cumulative sum of eigenvalues
cumsum(fa.eigen$values)/19 
# use the scree plot 
plot(fa.eigen$values, type = "b", ylab = "Eigenvalues", xlab = "Factor") # we choose 4


### Factor Analysis
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

# Apply PCR to data
pr.out = prcomp(data.clean, scale = TRUE)

# PCA Rotation Output
pr.out$rotation

# plot the PCs
biplot(pr.out, scale = 0,cex=0.5)
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale =0,cex=0.5)
# check the variance explained by each PC
pr.var = pr.out$sdev ^2
# proportion of variance explained
pve = pr.var/sum(pr.var)
cumsum(pve)

plot(pve, xlab = " Principal Component", ylab = "Proportion of
Variance Explained", ylim = c(0,1), type = "b")


## Test 1 - Taking only two components
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
 - need to dispute on normal distribution


"

# Applying kmeans to PCA
library(cluster)

# Sample
model=kmeans(pca_1_2, 2, nstart = 10)
clusplot(pca_1_2,model$cluster, main="3 Cluster")
sil <- silhouette(model$cluster, dist(pca_1_2))
fviz_silhouette(sil)
mean(sil[,3])


# Test different silhouette scores
loop_data <- pca_1_2
score = c()
n_cluster = c()

for (i in 2:5) {
  model=kmeans(loop_data, i, nstart = 10)
  sil <- silhouette(model$cluster, dist(loop_data))
  mean_sil <- mean(sil[,3])
  score <- c(score, mean_sil)
  n_cluster = c(n_cluster, i)
}

# Table to show different silhouettte score
sil_table = cbind(n_cluster, score)

# Other clusters



# fmodel=kmeans(pca_1_2,3, nstart = 10)
# library(cluster)
# clusplot(pca_1_2,model$cluster, main="3 Cluster")
# 
# model=kmeans(pca_1_2,4)
# library(cluster)
# clusplot(pca_1_2,model$cluster)
# 
# model=kmeans(pca_1_2,5)
# library(cluster)
# clusplot(pca_1_2,model$cluster)


# IF settle for 2 clusters

model=kmeans(pca_1_2, 3, nstart = 10)
clusplot(pca_1_2,model$cluster, main="2 Cluster")

plot(pca_1_2[,1], pca_1_2[,2], col=model$cluster)


# IF PCA component = 3

# Plotting 3d

library(scatterplot3d)

pca1to3 <- data.frame(pr.out$x[, 1:3])

model_3cluster=kmeans(pca1to3, 3, nstart = 10)

scatterplot3d(pca1to3,
              main="3D Scatter Plot",
              xlab = "PCA1",
              ylab = "PCA2",
              zlab = "PCA3", angle=40,
              color = model_3cluster$cluster)

model_3cluster$cluster


# test

biplot(pr.out, scale = 0,cex=0.5)
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale =0,cex=0.5, col=model$cluster)





"

So, the interpretation of the silhouette width is the following:

Si > 0 means that the observation is well clustered. The closest it is to 1, the best it is clustered.
Si < 0 means that the observation was placed in the wrong cluster.
Si = 0 means that the observation is between two clusters.
"


# # Testing to see the optimal number of groups of k means
# 
# wssplot <- function(data, nc=15, seed=123){
#   wss <- (nrow(data)-1)*sum(apply(data,2,var))
#   for (i in 2:nc){
#     set.seed(seed)
#     wss[i] <- sum(kmeans(data, centers=i)$withinss)}
#   plot(1:nc, wss, type="b", xlab="Number of groups",
#        ylab="Sum of squares within a group")}
# 
# wssplot(pca_1_2, nc = 20)
# 
# 
# # Plotting importance of each variable
# PC1 <- pr.out$rotation[,1]
# PC1_scores <- abs(PC1)
# PC1_scores_ordered <- sort(PC1_scores, decreasing = TRUE)
# names(PC1_scores_ordered)
# 
# # Plotting 3d
# 
# library(scatterplot3d)
# 
# 
# pca_1to3 <- data.frame(pr.out$x[, 1:3])
# scatterplot3d(pca_1to3,
#               main="3D Scatter Plot",
#               xlab = "PCA1",
#               ylab = "PCA2",
#               zlab = "PCA3", angle=40)



## Using TENSORFLOW




# #### ICA
# 
# ############################################################
# ############ Independent component analysis ###############
# 
# ##### apply ICA
# library(fastICA)
# set.seed(20)
# latent = fastICA(data.clean, 5, fun = "logcosh", alpha = 1,
#                  row.norm = TRUE, maxit = 200,
#                  tol = 0.0001, verbose = TRUE)
# ##### the estimated source signals are in S
# par(mfrow = c(1, 2))
# plot(1:2215, latent$S[,1], type = "l", xlab = "Esource 1",ylab="",cex.lab=1.5,cex.axis=1.5,lwd=2)
# plot(1:2215, latent$S[,2], type = "l", xlab = "Esource 1",ylab="",cex.lab=1.5,cex.axis=1.5,lwd=2)
# 
# 
# 
# #### Isometric feature mapping
# library(vegan)
# dis = dist(data.clean)
# swissIsomap = isomap(dis, k=10)
# plot(swissIsomap,col=labels)

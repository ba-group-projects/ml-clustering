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

# TODO remove ID and Year_Birth
data.clean$Year_Birth <- NULL 
data.clean$ID <- NULL



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


"
We can choose 6 components as it captures 70% of the variance
"


# Define the pca data with the optimal number of clusters
pca_data <- data.frame(pr.out$x[, 1:6])


# Applying kmeans to PCA
library(cluster)

# Test different silhouette scores


"
Silhouette scores are a method to see the optimal number of clusters

the interpretation of the silhouette width is the following:
  
Si > 0 means that the observation is well clustered. The closest it is to 1, the best it is clustered.
Si < 0 means that the observation was placed in the wrong cluster.
Si = 0 means that the observation is between two clusters.

"

loop_data <- pca_data
score = c()
n_cluster = c()


# Loop to see silhouette scores with different number of clusters
for (i in 2:9) {
  model=kmeans(loop_data, i, nstart = 10)
  sil <- silhouette(model$cluster, dist(loop_data))
  mean_sil <- mean(sil[,3])
  score <- c(score, mean_sil)
  n_cluster = c(n_cluster, i)
}

# Table to show different silhouettte score
sil_table = cbind(n_cluster, score)
sil_table

"
Table here shows the optimal number of clusters (scores should be ~0.5)

From my findings, our data does not have that lmao

"


# Elbow plot

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 10
data <- pca_data
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

"
We can also use the elbow plot to see which number of clusters are optimal
"


# Do kmeans with the optimal number of cluster
# In this example, I used 5, but feel free to change
model=kmeans(pca_data, 5, nstart = 10)

model$cluster

# Assign cluster label to new dataset
data.cluster <- data.clean
data.cluster$cluster <- model$cluster



# Plot the chart

"
Compare the distribution of variables between clusters

"
boxplot(Income ~ cluster, data = data.cluster, xlab = "Cluster",
        ylab = "Income", main = "Income Level by Cluster")

boxplot(Education ~ cluster, data = data.cluster, xlab = "Cluster",
        ylab = "Education", main = "Income Level by Cluster")

boxplot(NumWebVisitsMonth ~ cluster, data = data.cluster, xlab = "Cluster",
        ylab = "Number of Web Visits (Month)", main = "Income Level by Cluster")

boxplot(MntGoldProds ~ cluster, data = data.cluster, xlab = "Cluster",
        ylab = "MntGoldProds", main = "MntGoldProds and Cluster")

boxplot(Age ~ cluster, data = data.cluster, xlab = "Cluster",
        ylab = "Age", main = "Age and Cluster")


### Misc 

# PCA Loadings

# Plotting importance of each variable
PC1 <- pr.out$rotation[,1]
PC1_scores <- abs(PC1)
PC1_scores_ordered <- sort(PC1_scores, decreasing = TRUE)
names(PC1_scores_ordered)

# Plotting importance of each variable
PC2 <- pr.out$rotation[,2]
PC2_scores <- abs(PC2)
PC2_scores_ordered <- sort(PC2_scores, decreasing = TRUE)
names(PC2_scores_ordered)

PC3 <- pr.out$rotation[,3]
PC3_scores <- abs(PC3)
PC3_scores_ordered <- sort(PC3_scores, decreasing = TRUE)
names(PC3_scores_ordered)


# import libraries
library(ggplot2)
library(reshape)
library(dplyr)
library(GGally)
library(corrplot)
library(lubridate)
library(forcats)
library(readr)
library(reshape)
library(polycor)
library(EFA.dimensions)
library(fastICA)
library(tidyr)
library(stringr)
library(grid)
library(gridExtra)
library(ica)

##################
### PREPROCESS ###
##################

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
data.clean$Marital_Status[data.clean$Marital_Status == "Absurd"] <- 0
data.clean$Marital_Status[data.clean$Marital_Status == "Alone"] <- 0
data.clean$Marital_Status[data.clean$Marital_Status == "YOLO"] <- 0
data.clean$Marital_Status[data.clean$Marital_Status == "Single"] <- 0
data.clean$Marital_Status[data.clean$Marital_Status == "Together"] <- 1
data.clean$Marital_Status[data.clean$Marital_Status == "Married"] <- 1
data.clean$Marital_Status[data.clean$Marital_Status == "Divorced"] <- 0
data.clean$Marital_Status[data.clean$Marital_Status == "Widow"] <- 0
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

###########
### EDA ###
###########

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

# TODO: add histogram plots for appendix

features.order = c("Education", "Marital_Status", "Kidhome", "Teenhome", "Age", 
                   "Dt_Customer", "Income", "Recency", "MntWines", "MntFruits", 
                   "MntMeatProducts", "MntFishProducts", "MntSweetProducts",
                   "MntGoldProds", "NumDealsPurchases", "NumWebPurchases", 
                   "NumCatalogPurchases", "NumStorePurchases", "NumWebVisitsMonth")

###########################
### DIMENSION REDUCTION ###
###########################

####### FACTOR ANALYSIS #######

### determine the number of factors of FA
# We want to identify the component of the correlation structure
fa.cor = POLYCHORIC_R(numerical.data)

# looking at fa.cor
# let's look at eigenvector of the correlation to see how many factors we use
fa.eigen = eigen(fa.cor)
fa.eigen$values

# in decreasing values
# higher = more important
# rule of thumb to see the number of variables we should use? Do the sum
sum(fa.eigen$values)

# to select number of factors
cumsum(fa.eigen$values) / ncol(numerical.data) # Which is 19 here

# seeing this, using the first 4, we can explain 79% of the data
# draw screen plot for better visualisation

# use the scree plot
plot(fa.eigen$values, type = "b", ylab = "Eigenvalues", xlab = "Factor") # we choose 4
# plot(cumsum(fa.eigen$values)/17, type = "b", ylab = "Eigenvalues", xlab = "Factor") # we choose 4

### factor analysis with rotation
fa.res = factanal(x = numerical.data, factors = 4, rotation = "promax")
# promax belongs to the oblique rotation?

print(fa.res, cut = 0.2)

# we only show the factors with a loading of higher than 0.2
# therefore only selecting the variables with large loading
# you can see that paragraph, sentence and wordm is with factor 1

### factor scores
# TODO: play around the rotation method
fa.res.rot = factanal(x = numerical.data, factors = 4, rotation = "promax", scores = "Bartlett")
head(fa.res.rot$scores)
summary(lm(Factor2 ~ Factor1, data = as.data.frame(fa.res.rot$scores)))
fa.res.rot.loading  = data.frame(fa.res.rot$loadings[1:19,1:4])
fa.res.rot.loading$features = rownames(fa.res.rot.loading)
colnames(fa.res.rot.loading) = c('F1','F2','F3','F4','features')

# factors
fa.res.rot.loading

# prepare data for heat map
fa.res.df = melt(fa.res.rot.loading)
fa.res.df$features = factor(fa.res.df$features, levels = c("Education", "Marital_Status", "Kidhome", "Teenhome", "Age", 
                                                           "Dt_Customer", "Income", "Recency", "MntWines", "MntFruits", 
                                                           "MntMeatProducts", "MntFishProducts", "MntSweetProducts",
                                                           "MntGoldProds", "NumDealsPurchases", "NumWebPurchases", 
                                                           "NumCatalogPurchases", "NumStorePurchases", "NumWebVisitsMonth"))

###### PRINCIPAL COMPONENT ANALYSIS #######

# Apply PCA to data
pc.res = prcomp(numerical.data, scale = TRUE)

# we pick 9 components to explain 80% of variance
cumsum(pc.res$sdev^2/sum(pc.res$sdev^2))
pc.loading = data.frame(pc.res$rotation[1:19,1:9])

# principal components
pc.loading

# prepare data for heat map
pc.loading$features = rownames(pc.loading)
pr.loading.df = melt(pc.loading)
pr.loading.df$features = factor(pr.loading.df$features, levels = c("Education", "Marital_Status", "Kidhome", "Teenhome", "Age", 
                                                           "Dt_Customer", "Income", "Recency", "MntWines", "MntFruits", 
                                                           "MntMeatProducts", "MntFishProducts", "MntSweetProducts",
                                                           "MntGoldProds", "NumDealsPurchases", "NumWebPurchases", 
                                                           "NumCatalogPurchases", "NumStorePurchases", "NumWebVisitsMonth"))

###### INDEPENDENT COMPONENT ANALYSIS ######
# TODO ask about classification variable
# change the number of dimension
set.seed(20)
ica = fastICA(numerical.data, 6, fun = "logcosh", alpha = 1,
                   row.norm = T, maxit = 200,
                   tol = 0.0001, verbose = FALSE)

# # xia code
# ica.2 = icafast(numerical.data, 4, center = FALSE, maxit = 200, tol = 0.0001,
#                   Rmat = diag(4), alg = c("par", "def"),
#                   fun = c("logcosh"), alpha = 1)
# ica.2

ica.loading = data.frame(t(ica$A)) %>% 
  rename_with(~ str_glue("IC{seq(.)}")) %>%
  mutate(variable = names(numerical.data)) %>%
  pivot_longer(cols = starts_with("IC"), names_to = "components", values_to = "loading")

# prepare data for heat map
colnames(ica.loading) = c("features","variable","value")
ica.loading$features = factor(ica.loading$features, levels = c("Education", "Marital_Status", "Kidhome", "Teenhome", "Age", 
                                                              "Dt_Customer", "Income", "Recency", "MntWines", "MntFruits", 
                                                              "MntMeatProducts", "MntFishProducts", "MntSweetProducts",
                                                              "MntGoldProds", "NumDealsPurchases", "NumWebPurchases", 
                                                              "NumCatalogPurchases", "NumStorePurchases", "NumWebVisitsMonth"))

# TODO: ask Rui how to pick # of components for ICA
# TODO: extract variance explained for ICA

###### COMPARE LOADINGS ######

fa.plot = ggplot(fa.res.df, aes(x = variable, y = factor(features, level = features.order), fill = value)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1, show.legend = FALSE) +
  scale_fill_gradient2(low='blue',high='red') +
  ylab("Features") + xlab("Factors") + ggtitle('FA') +
  coord_fixed() + theme(plot.title = element_text(hjust = 0.5))

pr.plot = ggplot(pr.loading.df, aes(x = variable, y = factor(features, level = features.order), fill = value)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1, show.legend = FALSE) +
  scale_fill_gradient2(low='blue',high='red') + xlab("Components") + ggtitle('PCA') +
  coord_fixed() + ylab(NULL) + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), plot.title = element_text(hjust = 0.5))

ica.plot = ggplot(ica.loading, aes(x = variable, y = factor(features, level = features.order), fill = value)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) + 
  scale_fill_gradient2(low='blue',high='red',limits=c(-1,1)) + xlab("Components") + ggtitle('ICA') +
  coord_fixed() + ylab(NULL) + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), plot.title = element_text(hjust = 0.5))

# heat map comparison plot
grid.arrange(fa.plot, pr.plot, ica.plot, nrow = 1, 
             top=textGrob("Compare loadings among FA, PCA and ICA", gp=gpar(fontsize=15, font = 2)))

##################
### CLUSTERING ###
##################

# TODO: use ICA components for clustering

###### HEIRARCHICAL CLUSTERING ######



###### KMEANS CLUSTERING ######
# load required packages
library(factoextra)
library(NbClust)

# Elbow method
ica.latent = ica$S
fviz_nbclust(ica.latent, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle

kmean.model.4 = kmeans(ica.latent, 4, nstart = 10)
kmean.model.4.cluster = kmean.model.4$cluster

rfm.R = data.clean$Recency
rfm.F = rowSums(data.clean[,16:20])
rfm.M = rowSums(data.clean[,10:15])
rfm.income = data.clean$Income
RFM = data.frame(rfm.R,rfm.F,rfm.M,rfm.income)

min.max.scale <- function(x){(x-min(x))/(max(x)-min(x))}

RFM$rfm.R.scaled = min.max.scale(RFM$rfm.R)
RFM$rfm.F.scaled = min.max.scale(RFM$rfm.F)
RFM$rfm.M.scaled = min.max.scale(RFM$rfm.M)
RFM$rfm.income.scaled = min.max.scale(RFM$rfm.income)

RFM$cluster = kmean.model.4.cluster

# Label the cluster
# plot the rfm in different clusters
RFM%>%
  select(cluster, rfm.R.scaled, rfm.F.scaled, rfm.M.scaled,rfm.income.scaled)%>%
  melt(id='cluster')%>%
  ggplot(aes(as_factor(cluster), value))+
  geom_boxplot()+
  facet_wrap(~variable, ncol = 4)

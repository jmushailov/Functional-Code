 library(readr)
 library(readxl)
 rm(list=ls())

setwd("C:/users/joe/desktop/")
 
 
 file_list <- list.files()
 dat <- data.frame()
 
 # Read and merge excel files
 for(i in file_list[21:28]){
   assign("d", read_excel(i))
   dat <- rbind(dat, d)
 }
 
 # Read and merge csv files
 setwd("C:/users/joe/desktop/")
 file_list <- list.files()
 dat <- data.frame()
 
 for(i in file_list[1:length(file_list)]){
   assign("d", read_csv(i))
   dat <- rbind(dat, d)
 }
 
# ~~~~~~~~~~~~~~~~~~ PCA ON UNSCALED
pca <- princomp(na.omit(dat[,2:ncol(dat)]))
pca$loadings
screeplot(pca)

# ~~~~~~~~~~~~~~~~~~~ PCA ON SCALED
#Scale it
# Merge in simple churn results first
dat_scaled <- as.data.frame(cbind(dat$ID, scale(dat[,2:ncol(dat)], scale = FALSE), dat$simple_churn))
# Convert to numeric because PCA cannot handle non numerics and filter out simple churn from scaling
dat_scaled[,2:ncol(dat_scaled)-1] <- lapply(dat_scaled[,2:ncol(dat)-1], FUN = function(x) as.numeric(x))

str(dat_scaled) #Ensure all numeric

# Rename some columns just in case
colnames(dat_scaled)[1] <- "ID"
colnames(dat_scaled)[20] <- "simple_churn"

# Check if there are any non finites and NAs
sapply(dat_scaled,FUN = function(x) all(is.finite(x)))
sapply(dat_scaled, FUN = function(x) all(!is.na(x)))

pca_scaled <- princomp(na.omit(dat_scaled[,2:ncol(dat)-1]))
pca_scaled$loadings
screeplot(pca_scaled)

# ~~~~~~~~~~~~~~~~~~~ Logistic Regression / churn
ind <- sample(1:nrow(dat), 0.8*nrow(dat))
train <- dat[ind,]
test <- dat[-ind,]


logit <- glm(simple_churn ~ x1 + x2 + x3 +  x4 , 
             data = train, family = "binomial", maxit = 100) 


print(summary(logit))
anova(logit, "Chisq")


test_log <- predict(logit, newdata = test, type = "response")
test_log <- ifelse(test_log>0.5,1,0)
misClasificError <- mean(test_log != test$simple_churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))
print("Confusion Matrix for Logistic Regression"); table(test$simple_churn, test_log > 0.5)
# Odds Ratio --> Odds of an event happening
exp(cbind(OR=coef(logit), confint(logit)))
# Add back to df
test$pred <- test_log


# ~~~~~~~~~~~~~~~~~~~ Clustering
library(cluster)    # clustering algorithms
library(factoextra) #  algos and viz
library(tidyverse) # data manipulation
library(klaR) # Kmodes analysis
library(ggplot2)

# Take results of PCA analysis and enter into this
km_scaled <- kmeans(dat_scaled[,c("x1","x2","x3")], centers = 4)
km_scaled$cluster
head(km_scaled)

km_centers <- data.frame(cluster = factor(1:4), km_scaled$centers)
km_centers

#Visualize the clusters
fviz_cluster(km_scaled, data = dat_scaled[,c("x1","x2","x3")])



# Calculating ideal # of clusters using elbow method
wss <- function(k){
   kmeans(dat_scaled[,2:ncol(dat_scaled)-1], k, nstart = 10)$tot.withinss
}

k.values <- 3:9
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
     type = "b", pch = 19, frame =FALSE,
     xlab = "Num Clusters of K",
     ylab = "Total WSS")


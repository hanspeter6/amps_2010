# libraries
library(stringr)
library(tidyverse)
library(corrplot)
library(rpart)
library(rpart.plot)
library(scatterplot3d)
library(rgl)
library(kohonen)
library(caret)
library(randomForest)
library(MASS)
library(CCA)
library(nFactors)
library(FactoMineR)
library(factoextra)
library(gridExtra)

#  read in datasets
set10 <- readRDS("set10.rds")
set10_simple <- readRDS("set10_simple.rds")

# consider some correlations
png('corTypePlot2010.png')
corrplot(cor(set10[,c("newspapers","magazines","radio", "tv", "internet")]),
         method = "pie",
         order = "hclust",
         hclust.method = "complete",
         tl.col = 'black',
         mar = c(1,1,1,1),
         addCoefasPercent = TRUE,
         tl.pos = TRUE)
dev.off()

# # consider some clustering
# # construct distance matrix for newspapers, magazines, radio, tv and internet engagement:
# 
# dist10 <- dist(set10[,c("newspapers","magazines","radio", "tv", "internet")])
# clust10 <- hclust(dist10, method = "complete")
# plot(clust10) # messy, unhelpful

## consider kmeans
wss <- vector()
for(k in c(1,2,3,4,5,6)) {
        temp <- kmeans(set10[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                       centers = k,
                       nstart = 10,
                       iter.max = 20)
        wss <- append(wss,temp$tot.withinss)
}

png('kmeansTypePlot2010.png')
plot(c(1,2,3,4,5,6), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(56)
kmeans10 <- kmeans(set10[,c("newspapers","magazines","radio", "tv", "internet","all")],
                   centers = 4,
                   nstart = 20)
set.seed(56)
kmeans10_simple <- kmeans(set10_simple[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                          centers = 4,
                          nstart = 20,
                          iter.max = 20)

table(kmeans10$cluster) #

# Comparing 2012 with 2010... will change colours to reflect meanin based on 2012:

# green and stays green: 2 stays 2
# pink becomes red: 4 becomes 1
#  blue becomes pink\: 3 becomes 4
#  red becomes blue: 1 becomes 3

kmeans10$cluster <- ifelse(kmeans10$cluster == 4, 6, kmeans10$cluster)
kmeans10$cluster <- ifelse(kmeans10$cluster == 3, 9, kmeans10$cluster)
kmeans10$cluster <- ifelse(kmeans10$cluster == 1, 8, kmeans10$cluster)
kmeans10$cluster <- ifelse(kmeans10$cluster == 2, 7, kmeans10$cluster)
kmeans10$cluster <- kmeans10$cluster - 5
# add cluster labels to the dataset
set10 <- set10 %>%
        mutate(cluster = factor(kmeans10$cluster))




set10_simple <- set10_simple %>%
        mutate(cluster = factor(kmeans10_simple$cluster))

saveRDS(set10, "set10.rds")
saveRDS(set10_simple, "set10_simple.rds")

set10 <- readRDS("set10.rds")
set10_simple <- readRDS("set10_simple.rds")

# consider multidimensional scaling and self organising maps on the clusters :

# 1st create a subset to ensure easier running
set.seed(56)
sub10 <- set10[sample(nrow(set10), size = 1000),]

# distance matrix and MDS
sub10_dist <- dist(sub10[,c("newspapers","magazines","radio", "tv", "internet", "all")])
mds10 <- cmdscale(sub10_dist)
# plot(mds10, col = as.numeric(sub10$cluster) + 1, pch = 19, ylab = "", xlab = "")

# # 3D scaling
# mds3 <- cmdscale(dist(sub10[,c("newspapers", "magazines", "radio", "tv", "internet")]), k = 3)
# mds3 <- as.data.frame(mds3)

# 2D Scatterplots of 4 centers
# # setting colours
cols <- as.numeric(sub10$cluster) + 1
cols <- ifelse(cols == 5, 6, cols)

jpeg('kmeans2DPlot2010.jpeg')
plot(mds10, col = cols, ylab = "", xlab = "", pch = 19)
dev.off()

# consider for some predictions:
# create training and test sets:

set.seed(56)
ind_train <- createDataPartition(set10$cluster, p = 0.7, list = FALSE)
training <- set10[ind_train,]
testing <- set10[-ind_train,]

# # using random forest:
forest10_type <- randomForest(cluster ~ newspapers
                              + tv
                              + radio
                              + magazines
                              + internet,
                              data = training )

pred_forest10_type <- predict(forest10_type, newdata = testing)

confusionMatrix(pred_forest10_type, testing$cluster) 

# with lda. Although given accuracy of forest,  no real need.
set.seed(56)
lda10 <- lda(cluster ~ newspapers
             + tv
             + radio
             + magazines
             + internet,
             data = training)
summary(lda10)

pred_lda10 <- predict(lda10, newdata = testing)
confusionMatrix(pred_lda10$class, testing$cluster) # 

# using only demographic information
forest10_demogr <- randomForest(cluster ~ age
                                + sex
                                + edu
                                + hh_inc
                                + race
                                + lang
                                + lifestages
                                + mar_status
                                + lsm
                                + lifestyle
                                + attitudes,
                                data = training)

pred_forest10_demogr <- predict(forest10_demogr, newdata = testing)

confusionMatrix(pred_forest10_demogr, testing$cluster)

# with lda
set.seed(56)
lda10_demogr <- lda(cluster ~ age
                    + sex
                    + edu
                    + hh_inc
                    + race
                    + lang
                    + lifestages
                    + mar_status
                    + lsm
                    + lifestyle
                    + attitudes,
                    data = training)

pred_lda10_demogr <- predict(lda10_demogr, newdata = testing)
confusionMatrix(pred_lda10_demogr$class, testing$cluster)

##  some qualitative consideration of the four types:

# consider a single tree partitioning to try to add meaning to the six clusters
control <- rpart.control(maxdepth = 4, cp = 0.001)
tree10 <- rpart(cluster ~ newspapers + tv + radio + magazines + internet, 
                data = set10,
                control = control) # weights = pwgt
par(mfrow = c(1,1))
plot(tree10, uniform = TRUE, margin = 0.2)
text(tree10, pretty = 0, cex = 0.8)

# for more detail
rpart.plot(tree10, type = 4, extra = 1, cex = 0.5)

percentile <- ecdf(set10$internet)
percentile(1.4)

# some plots
jpeg('typeBoxPlots_10.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,3))
plot(set10$radio ~ set10$cluster, col = c(2,3,4,6), main = "radio", xlab = "cluster", ylab = '')
plot(set10$tv ~ set10$cluster, col = c(2,3,4,6), main = "tv", xlab = "cluster", ylab = '')
plot(set10$newspapers ~ set10$cluster, col = c(2,3,4,6), main = "newspapers", xlab = "cluster", ylab = '')
plot(set10$magazines ~ set10$cluster, col = c(2,3,4,6), main = "magazines", xlab = "cluster", ylab = '')
plot(set10$internet ~ set10$cluster, col = c(2,3,4,6), main = "internet", xlab = "cluster", ylab = '')
plot(set10$all ~ set10$cluster, col = c(2,3,4,6), main = "all", xlab = "cluster", ylab = '')
dev.off()

# try to make sense of demographics
jpeg('typeDemogPlots1_10.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,2))
plot(set10$cluster ~ factor(set10$race,labels = c("black", "coloured", "indian", "white")), col = c(2,3,4,6), main = "race", xlab = "", ylab = "")
plot(set10$cluster ~ factor(set10$edu, labels = c("<matric", "matric",">matric" )), col = c(2,3,4,6), main = "education", xlab = "", ylab = "")
plot(set10$cluster ~ factor(set10$age, labels = c("15-24","25-44", "45-54","55+")), col = c(2,3,4,6), main = "age", xlab = "", ylab = "")
plot(set10$cluster ~ factor(set10$lsm, labels = c("1-2", "3-4", "5-6", "7-8", "9-10")), col = c(2,3,4,6), main = "LSM", xlab = "", ylab = "")
dev.off()

jpeg('typeDemogPlots2_10.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,2))
plot(set10$cluster ~ factor(set10$sex, labels = c("male", "female")), col = c(2,3,4,6), main = "sex", xlab = "", ylab = "")
plot(set10$cluster ~ factor(set10$hh_inc, labels = c("<2500","2500-6999","7000-11999",">=10000")), col = c(2,3,4,6), main = "hh_inc", xlab = "", ylab = "")
# plot(set10$cluster ~ set10$lifestages, col = c(2,3,4,6), main = "lifestages", xlab = "", ylab = "") # appears to be not available, unless I extract summaries myself from lifestages set...
plot(set10$cluster ~ set10$lifestyle, col = c(2,3,4,6), main = "lifestyle", xlab = "", ylab = "")
dev.off()



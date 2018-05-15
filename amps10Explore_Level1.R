# # loading packages
library(tidyverse)
library(corrplot)
library(rpart)
library(rpart.plot)
# library(scatterplot3d)
# library(rgl)
library(caret)
library(randomForest)
library(MASS)
library(gridExtra)
library(ggplot2)

#  read in datasets
set10 <- readRDS("set10.rds")

# consider some correlations
jpeg('corTypePlot2010.jpeg')
corrplot(cor(set10[,c("newspapers","magazines","radio", "tv", "internet")]),
         method = "pie",
         order = "hclust",
         hclust.method = "complete",
         tl.col = 'black',
         mar = c(1,1,1,1),
         addCoefasPercent = TRUE,
         tl.pos = TRUE)
dev.off()

## consider kmeans
wss <- vector()
set.seed(123)
for(k in c(1,2,3,4,5,6)) {
        temp <- kmeans(set10[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                       centers = k,
                       nstart = 5,
                       iter.max = 30)
        wss <- append(wss,temp$tot.withinss)
}

jpeg('kmeansTypePlot2010.jpeg')
plot(c(1,2,3,4,5,6), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(123)
kmeans10 <- kmeans(set10[,c("newspapers","magazines","radio", "tv", "internet","all")],
                   centers = 4,
                   nstart = 5,
                   iter.max = 100)

table(kmeans10$cluster)

# align with interpretation of 2012 basic (ie, not simple)....
# green to green:  2 to 2
# lilac to blue: 4 to 3
# blue to red: 3 to 1
# red to lilac: 1 to 4
kmeans10$cluster <- ifelse(kmeans10$cluster == 1, 9, kmeans10$cluster)
kmeans10$cluster <- ifelse(kmeans10$cluster == 2, 7, kmeans10$cluster)
kmeans10$cluster <- ifelse(kmeans10$cluster == 3, 6, kmeans10$cluster)
kmeans10$cluster <- ifelse(kmeans10$cluster == 4, 8, kmeans10$cluster)
kmeans10$cluster <- kmeans10$cluster - 5



# add cluster labels to the dataset
set10c <- set10 %>%
        mutate(cluster = factor(kmeans10$cluster)) %>%
        dplyr::select(qn, pwgt, cluster, everything())

# save them
saveRDS(set10c, "set10c.rds")
# read back
set10c <- readRDS("set10c.rds")

## some plots for simple version to use in longitudinal stuff later...
# boxplots of clusters and media types

boxplot <- function(set,type) {
        ggplot(set, aes_string("cluster", type, fill = "cluster")) +
                geom_boxplot() +
                guides(fill = FALSE) +
                labs(title = type)
}

jpeg('typeBoxPlots_10.jpeg', quality = 100, type = "cairo")
grid.arrange(boxplot(set10c, type = "all"),
             boxplot(set10c, type = "newspapers"),
             boxplot(set10c, type = "magazines"),
             boxplot(set10c, type = "radio"),
             boxplot(set10c, type = "tv"),
             boxplot(set10c, type = "internet"),
             ncol=3, nrow = 2)
dev.off()

# try to make sense of demographics

# size of each cluster
ggplot(data = set10c, aes(x = cluster, fill = cluster)) +
        geom_bar(stat = "count") +
        guides(fill = FALSE)

# demographics by cluster

bars_by_cluster <- function(set, category) { # category:one of race, edu, age, lsm, sex, hh_inc
        if(category == "race") {
                level = c("black", "coloured", "indian", "white")
                title = "Population Group 2010"
        }
        if(category == "edu") {
                level = c(c("<matric", "matric",">matric"))
                title = "Education Level 2010"
        }
        if(category == "age") {
                level = c(c("15-24","25-44", "45-54","55+"))
                title = "Age Group 2010"
        }
        if(category == "lsm") {
                level = c("1-2", "3-4", "5-6", "7-8", "9-10")
                title = "LSM 2010"
        }
        if(category == "sex") {
                level = c("male", "female")
                title = "Gender 2010"
        }
        if(category == "hh_inc") {
                level = c("<5000","5000-10999","11000-19999",">=20000")
                title = "Household Income 2010"
        }
        
        ggplot(data = set10c, aes_string(x = "cluster", fill = category)) +
                geom_bar(stat = "count", position = position_dodge()) +
                scale_fill_discrete(labels=level) +
                labs(title = title) +
                guides(fill=guide_legend(title=NULL)) 
}

jpeg('typeDemogPlots_10.jpeg', quality = 100, type = "cairo")
grid.arrange(bars_by_cluster(set10c, "sex"),
             bars_by_cluster(set10c, "age"),
             bars_by_cluster(set10c, "race"),
             bars_by_cluster(set10c, "edu"),
             bars_by_cluster(set10c, "hh_inc"),
             bars_by_cluster(set10c, "lsm"),
             ncol=2, nrow = 3)
dev.off()

# consider multidimensional scaling and self organising maps on the clusters :

# 1st create a subset to ensure easier running
set.seed(56)
sub10 <- set10c[sample(nrow(set10c), size = 1000),]

# distance matrix and MDS
sub10_dist <- dist(sub10[,c("newspapers","magazines","radio", "tv", "internet", "all")])
mds10 <- cmdscale(sub10_dist)
plot(mds10, col = as.numeric(sub10$cluster) + 1, pch = 19, ylab = "", xlab = "")

# 3D scaling
mds3 <- cmdscale(dist(sub10[,c("newspapers", "magazines", "radio", "tv", "internet", "all")]), k = 3)
mds3 <- as.data.frame(mds3)

# 2D Scatterplots of 4 cente

# setting colours
cols <- as.numeric(sub10$cluster) + 1
cols <- ifelse(cols == 5, 6, cols)

jpeg('kmeans2DPlot2010.jpeg')
plot(mds10, col = cols, ylab = "", xlab = "", pch = 19)
dev.off()
# 

# consider for some predictions:
# create training and test sets:

set.seed(56)
ind_train <- createDataPartition(set10c$cluster, p = 0.7, list = FALSE)
training <- set10c[ind_train,]
testing <- set10c[-ind_train,]

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
confusionMatrix(pred_lda10$class, testing$cluster) # collinearity meant took out 

# using only demographic information
forest10_demogr <- randomForest(cluster ~ age
                                + sex
                                + edu
                                + hh_inc
                                + race
                                + lsm,
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
                    + lsm,
                    data = training)

pred_lda10_demogr <- predict(lda10_demogr, newdata = testing)
confusionMatrix(pred_lda10_demogr$class, testing$cluster)

##  some qualitative consideration of the four types:

# consider a single tree partitioning to try to add meaning to the four clusters
control <- rpart.control(maxdepth = 3, cp = 0.001)
tree10 <- rpart(cluster ~ newspapers + tv + radio + magazines + internet, 
                data = set10c,
                control = control) # weights = pwgt
par(mfrow = c(1,1))
plot(tree10, uniform = TRUE, margin = 0.2)
text(tree10, pretty = 0, cex = 0.8)

# for more detail
rpart.plot(tree10, type = 4, extra = 1, cex = 0.5)





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
library(ggplot2)

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

set.seed(65)
kmeans10 <- kmeans(set10[,c("newspapers","magazines","radio", "tv", "internet","all")],
                   centers = 4,
                   nstart = 20,
                   iter.max = 20)
set.seed(56)
kmeans10_simple <- kmeans(set10_simple[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                          centers = 4,
                          nstart = 20,
                          iter.max = 20)

table(kmeans10$cluster) #

# Comparing 2012 with 2010... will change colours to reflect meanin based on 2012:

# green becomes red: 2 becomes 1
# lilac becomes blue: 4 becomes 3
# blue becomes green: 3 becomes 2
#  red becomes lilac: 1 becomes 4

kmeans10$cluster <- ifelse(kmeans10$cluster == 1, 9, kmeans10$cluster)
kmeans10$cluster <- ifelse(kmeans10$cluster == 2, 6, kmeans10$cluster)
kmeans10$cluster <- ifelse(kmeans10$cluster == 3, 7, kmeans10$cluster)
kmeans10$cluster <- ifelse(kmeans10$cluster == 4, 8, kmeans10$cluster)
kmeans10$cluster <- kmeans10$cluster - 5

# add cluster labels to the dataset
set10c <- set10 %>%
        mutate(cluster = factor(kmeans10$cluster))

set10c_simple <- set10_simple %>%
        mutate(cluster = factor(kmeans10_simple$cluster))

saveRDS(set10c, "set10c.rds")
saveRDS(set10c_simple, "set10c_simple.rds")

set10c <- readRDS("set10c.rds")
set10c_simple <- readRDS("set10c_simple.rds")

# some plots
# boxplots of clusters and media types
p1 <- ggplot(set10c, aes(cluster, all, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "all")
p2 <- ggplot(set10c, aes(cluster, newspapers, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "newspapers")
p3 <- ggplot(set10c, aes(cluster, magazines, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "magazines")
p4 <- ggplot(set10c, aes(cluster, radio, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "radio")
p5 <- ggplot(set10c, aes(cluster, tv, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "tv")
p6 <- ggplot(set10c, aes(cluster, internet, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "internet")

jpeg('typeBoxPlots_10.jpeg', quality = 100, type = "cairo")
grid.arrange(p1, p2, p3, p4, p5,p6,  ncol=3, nrow = 2)
dev.off()

# try to make sense of demographics
d1 <- ggplot(set10c, aes(race, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "race", y = "", x = "") +
        scale_x_discrete(labels=c("black", "coloured", "indian", "white"))
d2 <- ggplot(set10c, aes(edu, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "education", y = "", x = "") +
        scale_x_discrete(labels=c("<matric", "matric",">matric"))
d3 <- ggplot(set10c, aes(age, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "age", y = "", x = "") +
        scale_x_discrete(labels=c("15-24","25-44", "45-54","55+"))
d4 <- ggplot(set10c, aes(lsm, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "lsm", y = "", x = "") +
        scale_x_discrete(labels=c("1-2", "3-4", "5-6", "7-8", "9-10"))

jpeg('typeDemogPlots1_10.jpeg', quality = 100, type = "cairo")
grid.arrange(d1, d2, d3, d4, ncol=2, nrow = 2)
dev.off()

d5 <- ggplot(set10c, aes(sex, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "gender", y = "", x = "") +
        scale_x_discrete(labels=c("male", "female"))
d6 <- ggplot(set10c, aes(hh_inc, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "household income", y = "", x = "") +
        scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
d7 <- ggplot(set10c, aes(lifestages, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "lifestages", y = "", x = "")# +
# scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
d8 <- ggplot(set10c, aes(lifestyle, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "lifestyle", y = "", x = "")# +
# scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
jpeg('typeDemogPlots2_10.jpeg', quality = 100, type = "cairo")
grid.arrange(d5, d6, d7, d8, ncol=2, nrow = 2)
dev.off()




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

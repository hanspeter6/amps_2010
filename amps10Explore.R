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
# 

# read datafiles
magazines_engagement_10 <- readRDS("magazines_engagement_10.rds")
newspapers_engagement_10 <- readRDS("newspapers_engagement_10.rds")
radio_engagement_10 <- readRDS("radio_engagement_10.rds")
tv_engagement_10 <- readRDS("tv_engagement_10.rds")
internet_engagement_10 <- readRDS("internet_engagement_10.rds")

media_type_10 <- readRDS("media_type_10.rds")
media_vehicles_10 <- readRDS("media_vehicles_10.rds")

demographics_10 <- readRDS("demographics_10.rds")

#reducing levels of categorical variables and setting factor types for demographics:

# age:
demographics_10$age <- ifelse(demographics_10$age %in% c(1,2), 1, demographics_10$age)
demographics_10$age <- ifelse(demographics_10$age %in% c(3,4), 2, demographics_10$age)
demographics_10$age <- ifelse(demographics_10$age %in% c(5,6), 3, demographics_10$age)
demographics_10$age <- ifelse(demographics_10$age %in% c(7,8), 4, demographics_10$age)
demographics_10$age <- factor(demographics_10$age, ordered = TRUE)

# sex:
demographics_10$sex <- factor(demographics_10$sex, ordered = FALSE)

#edu:
demographics_10$edu <- ifelse(demographics_10$edu %in% c(1,2,3,4), 1, demographics_10$edu)
demographics_10$edu <- ifelse(demographics_10$edu %in% c(5), 2, demographics_10$edu)
demographics_10$edu <- ifelse(demographics_10$edu %in% c(6,7,8), 3, demographics_10$edu)
demographics_10$edu <- factor(demographics_10$edu, ordered = TRUE)

#hh_inc
demographics_10$hh_inc <- ifelse(demographics_10$hh_inc %in% c(1,2,3,4), 1, demographics_10$hh_inc)
demographics_10$hh_inc <- ifelse(demographics_10$hh_inc %in% c(5,6), 2, demographics_10$hh_inc)
demographics_10$hh_inc <- ifelse(demographics_10$hh_inc %in% c(7), 3, demographics_10$hh_inc)
demographics_10$hh_inc <- ifelse(demographics_10$hh_inc %in% c(8), 4, demographics_10$hh_inc)
demographics_10$hh_inc <- factor(demographics_10$hh_inc, ordered = TRUE)

demographics_10$race <- factor(demographics_10$race, ordered = FALSE)
demographics_10$province <- factor(demographics_10$province, ordered = FALSE)
demographics_10$metro <- factor(demographics_10$metro, ordered = FALSE)
demographics_10$lang <- factor(demographics_10$lang, ordered = FALSE)
demographics_10$lifestages <- factor(demographics_10$lifestages, ordered = FALSE)
demographics_10$mar_status <- factor(demographics_10$mar_status, ordered = FALSE)
# demographics_10$pers_inc <- factor(demographics_10$pers_inc, ordered = TRUE)

# lsm
demographics_10$lsm <- ifelse(demographics_10$lsm %in% c(1,2), 1, demographics_10$lsm)
demographics_10$lsm <- ifelse(demographics_10$lsm %in% c(3,4), 2, demographics_10$lsm)
demographics_10$lsm <- ifelse(demographics_10$lsm %in% c(5,6), 3, demographics_10$lsm)
demographics_10$lsm <- ifelse(demographics_10$lsm %in% c(7,8), 4, demographics_10$lsm)
demographics_10$lsm <- ifelse(demographics_10$lsm %in% c(9,10), 5, demographics_10$lsm)
demographics_10$lsm <- factor(demographics_10$lsm, ordered = TRUE)

demographics_10$lifestyle <- factor(demographics_10$lifestyle, ordered = FALSE)
demographics_10$attitudes <- factor(demographics_10$attitudes, ordered = FALSE)

# #create single dataset minus non metropolitans
set10 <- demographics_10 %>%
        left_join(media_type_10) %>%
        left_join(media_vehicles_10) %>%
        filter(metro != 0)

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
for(k in c(3,4,5,6,7,8,9,10,11,10)) {
        temp <- kmeans(set10[,c("newspapers","magazines","radio", "tv", "internet")],
                       centers = k,
                       nstart = 3,
                       iter.max = 20)
        wss <- append(wss,temp$tot.withinss)
}

png('kmeansTypePlot2010.png')
plot(c(3,4,5,6,7,8,9,10,11,10), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(56)
kmeans10 <- kmeans(set10[,c("newspapers","magazines","radio", "tv", "internet")],
                   centers = 5,
                   nstart = 20)
table(kmeans10$cluster) #

# add cluster labels to the dataset
set10 <- set10 %>%
        mutate(cluster = factor(kmeans10$cluster))

saveRDS(set10, "set10.rds")

set10 <- readRDS("set10.rds")

# consider multidimensional scaling and self organising maps on the clusters :

# 1st create a subset to ensure easier running
set.seed(56)
sub10 <- set10[sample(nrow(set10), size = 1000),]

# distance matrix and MDS
sub10_dist <- dist(sub10[,c("newspapers","magazines","radio", "tv", "internet")])
mds10 <- cmdscale(sub10_dist)
plot(mds10, col = as.numeric(sub10$cluster) + 1, pch = 19, ylab = "", xlab = "")

# 3D scaling
mds3 <- cmdscale(dist(sub10[,c("newspapers", "magazines", "radio", "tv", "internet")]), k = 3)
mds3 <- as.data.frame(mds3)

# 2D & 3D Scatterplots of 5 centers
jpeg('kmeans2DPlot2010.jpeg')
plot(mds10, col = as.numeric(sub10$cluster) + 1, ylab = "", xlab = "", pch = 19)
dev.off()

jpeg('kmeans3DPlot2010.jpeg')
scatterplot3d(mds3, color = as.numeric(sub10$cluster) + 1, xlab = '', ylab = '', zlab = '')
dev.off()

# Spinning 3D for 5 classes
jpeg('kmeansSpinningPlot2010.png')
plot3d(jitter(mds3$V1), jitter(mds3$V2), jitter(mds3$V3), col= as.numeric(sub10$cluster) + 1, size=5, xlab = '', ylab = '', zlab = '', pch = 19)
dev.off()

# try some Self Organising Maps.... try to explain the differences....

# set up somgrid
grid <- somgrid(xdim = 10, ydim = 10, topo = "hexagonal")

# run som
# set up as data matrix
mat_sub <- as.matrix(sub10[,c('newspapers', 'magazines', 'radio', 'tv','internet')])
som_sub <- som(mat_sub, grid = grid, rlen = 10000) 

par(mfrow = c(1,1))
plot(som_sub, type = "codes")
plot(som_sub, type = "changes")
plot(som_sub, type = "counts")
plot(som_sub, type = "dist.neighbours")
plot(som_sub, type = "quality")

par(mfrow = c(3,2))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,1], main = names(sub10['newspapers']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,2], main = names(sub10['magazines']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,3], main = names(sub10['radio']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,4], main = names(sub10['tv']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,5], main = names(sub10['internet']))

par(mfrow = c(1,1))
plot(som_sub, type = "mapping", bgcol = sub10$cluster ) # not very good organising??

# Try pca to get sense of relative use of media type... not very helpful since in most cases require many components to reflect variation in the data.

mags_pca <- princomp(scale(magazines_engagement_10))
screeplot(mags_pca, type = "lines")
newsp_pca <- princomp(scale(newspapers_engagement_10))
screeplot(newsp_pca, type = "lines")
tv_pca <- princomp(scale(tv_engagement_10))
screeplot(tv_pca, type = "lines")
rad_pca <- princomp(scale(radio_engagement_10[,-60])) # cant divide by zero
screeplot(rad_pca, type = "lines")
int_pca <- princomp(scale(internet_engagement_10))
screeplot(int_pca, type = "lines")

all_pca <- princomp(set10[,c('newspapers','magazines', 'tv', 'radio', 'internet')])
screeplot(all_pca, type = "lines")
summary(all_pca) # first component could be useful (@~40% of variation) to give relative multimedia scores

# try kmeans on the first pca and compare with cluster values...
test <- kmeans(all_pca$scores[,1], centers = 6)
test$cluster
set10$cluster
cor(test$cluster, as.numeric(set10$cluster))

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
plot(set10$radio ~ set10$cluster, col = c(2,3,4,5,6), main = "radio", xlab = "cluster", ylab = '')
plot(set10$tv ~ set10$cluster, col = c(2,3,4,5,6), main = "tv", xlab = "cluster", ylab = '')
plot(set10$newspapers ~ set10$cluster, col = c(2,3,4,5,6), main = "newspapers", xlab = "cluster", ylab = '')
plot(set10$magazines ~ set10$cluster, col = c(2,3,4,5,6), main = "magazines", xlab = "cluster", ylab = '')
plot(set10$internet ~ set10$cluster, col = c(2,3,4,5,6), main = "internet", xlab = "cluster", ylab = '')
dev.off()

# try to make sense of demographics
jpeg('typeDemogPlots1_10.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,2))
plot(set10$cluster ~ factor(set10$race,labels = c("black", "coloured", "indian", "white")), col = c(2,3,4,5,6), main = "race", xlab = "", ylab = "")
plot(set10$cluster ~ factor(set10$edu, labels = c("<matric", "matric",">matric" )), col = c(2,3,4,5,6), main = "education", xlab = "", ylab = "")
plot(set10$cluster ~ factor(set10$age, labels = c("15-24","25-44", "45-54","55+")), col = c(2,3,4,5,6), main = "age", xlab = "", ylab = "")
plot(set10$cluster ~ factor(set10$lsm, labels = c("1-2", "3-4", "5-6", "7-8", "9-10")), col = c(2,3,4,5,6), main = "LSM", xlab = "", ylab = "")
dev.off()

jpeg('typeDemogPlots2_10.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,2))
plot(set10$cluster ~ factor(set10$sex, labels = c("male", "female")), col = c(2,3,4,5,6), main = "sex", xlab = "", ylab = "")
plot(set10$cluster ~ factor(set10$hh_inc, labels = c("<2500","2500-6999","7000-11999",">=10000")), col = c(2,3,4,5,6), main = "hh_inc", xlab = "", ylab = "")
plot(set10$cluster ~ set10$lifestages, col = c(2,3,4,5,6), main = "lifestages", xlab = "", ylab = "")
plot(set10$cluster ~ set10$lifestyle, col = c(2,3,4,5,6), main = "lifestyle", xlab = "", ylab = "")
dev.off()

# libraries
library(nFactors)
library(psych)
library(FactoMineR)

# load datafiles 
set10_min_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set10_min_simple.rds")

# LEVEL 2

# Subsetting only on the variable I intend to use in this section:
set10_min_simple <- set10_min_simple[,-c(1:2,8:12,14:21)]

# ## Determine Number of Factors to Extract
# ev <- eigen(cor(set10_min[,7:ncol(set10_min)]))
# ap <- parallel(subject=nrow(set10_min[,7:ncol(set10_min)]),var=ncol(set10_min[,7:ncol(set10_min)]),
#                rep=100,cent=.02)
# nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
# jpeg("nScree_10_min")
# plotnScree(nS, main = "National") # optimal = 6
# dev.off()
# 
# # will set them at six for both Jhb and CT for now
# npc <- 6
# 
# # creating objects with supplementary variables (qualitative and quantitative) and active one defined:
# set.seed(56)
# pca_10_min <- PCA(set10_min,
#                   quanti.sup = c(1,3,4,6),
#                   quali.sup = c(2,5),
#                   ncp = npc,
#                   graph = FALSE)
# saveRDS(pca_10_min, "pca_10_min.rds")

# pa method of factor analysis with oblimin rotation allowed....to try and get better estimation
set.seed(123)
fact_10_simple <- fa(set10_min_simple[7:ncol(set10_min_simple)], nfactors = 6, fm = "pa") # default rotation oblimin, so does allow correlation between factors
fact_10_loadings_simple <- fact_10_simple$loadings
fact_10_scores_simple <- fact_10_simple$scores

# save model
saveRDS(fact_10_simple, "fact_10_simple.rds")

# save loadings:
saveRDS(fact_10_loadings_simple, "fact_10_loadings_simple.rds")

# save scores:
saveRDS(fact_10_scores_simple, "fact_10_scores_simple.rds")


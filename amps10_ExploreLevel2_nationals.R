# libraries
library(nFactors)
library(psych)
# library(FactoMineR)
library(grid)

# load datafiles 
set10_nat <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set10_nat.rds")

# LEVEL 2

# focussing only on the variable I intend to use in this section:
set10_nat <- set10_nat[,-c(1:2,8:12,14:21)]

## Determine Number of Factors to Extract
ev <- eigen(cor(set10_nat[,7:ncol(set10_nat)]))
ap <- parallel(subject=nrow(set10_nat[,7:ncol(set10_nat)]),var=ncol(set10_nat[,7:ncol(set10_nat)]),
               rep=100,cent=.10)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
jpeg("nScree_10_nat")
plotnScree(nS, main = "National 2010") # optimal = 8
dev.off()

# # will set them at six for now
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
fact_10_nat <- fa(set10_nat[7:ncol(set10_nat)], nfactors = 8, fm = "pa") # default rotation oblimin, so does allow correlation between factors
fact_10_nat_loadings <- fact_10_nat$loadings

# save model
saveRDS(fact_10_nat, "fact_10_nat.rds")

# save loadings:
saveRDS(fact_10_nat_loadings, "fact_10_nat_loadings.rds")

write.csv(round(loadings(fact_10_nat, sort = TRUE), 2), file = "loadings_nat_10.csv")

loadings(fact_10_nat, sort = TRUE)

# 
# tab_teset <- tableGrob(round(fact_10_nat_loadings, 2), theme = ttheme_minimal(base_size = 6)) # table
# 
# grid.newpage()
# h_ct_2 <- grobHeight(tab_teset)
# w_ct_2 <- grobWidth(tab_teset)
# title_ct_2 <- textGrob("Dimension 2", y=unit(0.5,"npc") + 0.5*h_ct_2, 
#                        vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
# gt_ct_2 <- gTree(children = gList(tab_teset, title_ct_2)) #,footnote
# grid.draw(gt_ct_2) # check
# 
# # arrange two Dimensions on one plot and print to interactive graphic with latex
# ml_ct_1n2 <- marrangeGrob(list(gt_ct_1,gt_ct_2), nrow=1, ncol=2, top = '\n\n\n\nCape Town')
# 
# # print to graphic
# jpeg("dims10_ct_1n2.jpeg")
# ml_ct_1n2
# dev.off()



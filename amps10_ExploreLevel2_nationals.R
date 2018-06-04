# libraries
library(nFactors)
library(psych)
# library(FactoMineR)
library(grid)
library(dplyr)
library(ggplot2)
library(gridExtra)

# load datafiles 
set10_nat <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set10_nat.rds")

# LEVEL 2

# # focussing only on the variable I intend to use in this section:
# set10_nat <- set10_nat[,-c(1:2,8:12,14:21)]

## Determine Number of Factors to Extract

# id vehicle columns:
strt <- which(names(set10_nat) == "Business.Day")
lst <- ncol(set10_nat)

ev <- eigen(cor(set10_nat[,strt:lst]))
ap <- parallel(subject=nrow(set10_nat[,strt:lst]),var=ncol(set10_nat[,strt:lst]),
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
fact_10_nat <- fa(set10_nat[strt:lst], nfactors = 8, fm = "ml") # default rotation oblimin, so does allow correlation between factors
fact_10_nat_loadings <- fact_10_nat$loadings

# save model
saveRDS(fact_10_nat, "fact_10_nat.rds")

# save loadings:
saveRDS(fact_10_nat_loadings, "fact_10_nat_loadings.rds")

write.csv(round(loadings(fact_10_nat, sort = TRUE), 2), file = "loadings_nat_10.csv")

loadings(fact_10_nat, sort = TRUE)

# consider linear regression model to examine relationship between demographics and the eight factors:

# Factor 1-8:
set10_nat_factors <- cbind.data.frame(set10_nat, fact_10_nat$scores)

# Do this for each factor
lm_f1 <- lm(ML1 ~ 
                    age_actual +
                    sex +
                    edu +
                    hh_inc +
                    race +
                    factor(lsm, ordered = FALSE),
            data = set10_nat_factors)
summary(lm_f1) # discuss along with visualisation developed below

# Try to visualise this?
expl_demogs_factors <- function(set, category, factor_name) {
        
        require(dplyr, ggplot2)
        
        if(category == "race") {
                level = c("black", "coloured", "indian", "white")
                title = "Population Group"
        }
        if(category == "edu") {
                level = c(c("<matric", "matric",">matric"))
                title = "Education Level"
        }
        if(category == "age") {
                level = c(c("15-24","25-44", "45-54","55+"))
                title = "Age Group"
        }
        if(category == "lsm") {
                level = c("1-2", "3-4", "5-6", "7-8", "9-10")
                title = "LSM"
        }
        if(category == "sex") {
                level = c("male", "female")
                title = "Gender"
        }
        if(category == "hh_inc") {
                level = c("<5000","5000-10999","11000-19999",">=20000")
                title = "Household Income"
        }
        
        # mean factor engagement level by category
        temp_set <- set %>%
                group_by_(category) %>%
                summarise(mean = mean(get(factor_name)))
        
        ggplot(data = temp_set) +
                aes_string(x = category, y = "mean") +
                aes(fill = mean ) +
                geom_col() +
                scale_x_discrete(labels = level) +
                # labs(title = paste(factor_name, "Mean Score and", title)) +
                # guides(fill=guide_legend(title=NULL)) +
                xlab(label = title) +
                guides(fill=FALSE) +
                ylab(label = "") +
                theme(plot.title = element_text(size = 10),
                      axis.text.y = element_text(size = 8),
                      axis.text.x = element_text(size = 6)) 
}

expl_demogs_factors(set10_nat_factors,"hh_inc", "ML2")

# function to print as grid
grid_print_demogs_factors <- function(set,factor,file,plot = TRUE) {
        
        # packages and functions
        get("expl_demogs_factors")
        require(ggplot2)
        require(gridExtra)
        
        # individual plots using own function
        p1 <- expl_demogs_factors(set,"sex", factor)
        p2 <- expl_demogs_factors(set,"age", factor)
        p3 <- expl_demogs_factors(set,"race", factor)
        p4 <- expl_demogs_factors(set,"edu", factor)
        p5 <- expl_demogs_factors(set,"hh_inc", factor)
        p6 <- expl_demogs_factors(set,"lsm", factor)
        
        # plot to see (if asked for)
        if(plot) {
                grid.arrange(p1,p2,p3,p4,p5,p6, nrow = 2, top = paste(factor,"and Mean Engagement Levels"))   
        }
        
        # print to graphic file
        jpeg(paste0(file,".jpeg"), quality = 100)
        grid.arrange(p1,p2,p3,p4,p5,p6, nrow = 2, top = paste(factor,"and Mean Engagement Levels"))
        dev.off()
} 

grid_print_demogs_factors(set = set10_nat_factors,
                          factor = "ML1",
                          file ="ML1_demogs",
                          plot = TRUE)


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



# libraries
library(stringr)
library(tidyverse)

print_10 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/csv/amps-2010-newspaper-magazine-readership-v1.1.csv")
electr_10 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/csv/amps-2010-electronic-media-v1.1.csv")
internet_10 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/csv/amps-2010-cellphone-and-internet-v1.1.csv")
demogrs_10 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/csv/amps-2010-demographics-v1.1.csv")
personal_10 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/csv/amps-2010-personal-v1.1.csv")
lsm_10 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/csv/amps-2010-lsm-and-saarf-segmentations-v1.1.csv")
lifestage_10 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/csv/amps-2010-lifestages-v1.1.csv")
attitudes_10 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/csv/amps-2010-attitudes-v1.1.csv")
# 
save(print_10, electr_10, internet_10, demogrs_10, personal_10, lsm_10, lifestage_10, attitudes_10, file = "input_10.RData")

load("input_10.RData")
# 
print_10_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/csv/metadata/variable_labels/amps-2010-newspaper-magazine-readership-v1.1_variable_labels.txt")
electr_10_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/csv/metadata/variable_labels/amps-2010-electronic-media-v1.1_variable_labels.txt")
internet_10_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/csv/metadata/variable_labels/amps-2010-cellphone-and-internet-v1.1_variable_labels.txt")
demogrs_10_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/csv/metadata/variable_labels/amps-2010-demographics-v1.1_variable_labels.txt")
personal_10_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/csv/metadata/variable_labels/amps-2010-personal-v1.1_variable_labels.txt")
lsm_10_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/csv/metadata/variable_labels/amps-2010-lsm-and-saarf-segmentations-v1.1_variable_labels.txt")
lifestage_10_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/csv/metadata/variable_labels/amps-2010-lifestages-v1.1_variable_labels.txt")
attitudes_10_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/csv/metadata/variable_labels/amps-2010-attitudes-v1.1_variable_labels.txt")


# 
save(print_10_labels, electr_10_labels, internet_10_labels, demogrs_10_labels, personal_10_labels, lsm_10_labels, lifestage_10_labels, attitudes_10_labels, file = "labels_10.RData")

load("labels_10.RData")

## 1st Print (newspapers and magazines) Media Set

names_print_10 <- str_subset(print_10_labels, 'Number of different issues usually read or page through') %>%
        str_replace('.+\\s-', '') %>%
        str_trim()

# check_12 <- readRDS("names_print_12_copy.rds")
# 
# fix(names_print_10)
# saveRDS(names_print_10, "names_print_10.rds")

names_print_10 <- readRDS("names_print_10.rds")

# names_dailies_10 <- names_print_10[1:22]
# names_biweeklies_10 <- names_print_10[23]
# names_weeklies_10 <- names_print_10[24:52]

# # NBNB: Not community papers in 2010...
# names_community_cape_town <- names_print[40:51]
# names_community_restCape <- names_print[52:61]
# names_community_FreeState <- names_print[62:66]
# names_community_NWest <- names_print[67:68]
# names_community_Jhb <- names_print[69]
# names_community_ERand <- names_print[70:71]
# names_community_KZn <- names_print[72:74]
# 
# names_mags_weekly_10 <- names_print_10[53:65]
# names_fortnightly_mags_10 <- names_print_10[66:67]
# names_monthly_news_10 <- names_print_10[68:69]
# names_monthly_mags_10 <- names_print_10[70:147]
# 
# names_alt_monthly_10 <- names_print_10[150:161]
# names_quarterly_mags_10 <- names_print_10[164:168]
# 
# names_monthly_store_mags_10 <- names_print_10[148:149]
# names_alt_month_store_mags_10 <- names_print_10[162:163]
# names_quarterly_store_mags_10 <- names_print_10[169:172]

# create print dataset:
issues_10 <- print_10[,str_detect(names(print_10), 'ca[345678]co\\d{2}')]
names(issues_10) <- names_print_10


saveRDS(issues_10, "issues_10.rds")

thorough_10 <- print_10[,str_detect(names(print_10), 'ca((34)|(35)|(36)|(37)|(38)|(39))co\\d{2}')]
thorough_10 <- thorough_10[,-which(names(thorough_10) == 'ca38co50'| names(thorough_10) == 'ca38co51')] # get rid of six-week mags (zigzag and saltwater girl) not in issues

names(thorough_10) <- names_print_10

# # need to reverse numbering to serve as weights (see value_lables text file):
thorough_10 <- 7 - thorough_10

saveRDS(thorough_10, "thorough_10.rds")
# create single print dataset:

print_engagement_10 <- issues_10 * thorough_10

# replace nas with zero's:
print_engagement_10[is.na(print_engagement_10)] <- 0

saveRDS(print_engagement_10, "print_engagement_10.rds")

print_engagement_10 <- readRDS("print_engagement_10.rds")

newspapers_engagement_10 <- print_engagement_10[,1:50]
magazines_engagement_10 <- print_engagement_10[,51:167]

saveRDS(newspapers_engagement_10, "newspapers_engagement_10.rds")
saveRDS(magazines_engagement_10, "magazines_engagement_10.rds")

magazines_engagement_10 <- readRDS("magazines_engagement_10.rds")
newspapers_engagement_10 <- readRDS("newspapers_engagement_10.rds")

## 2nd Electronic Media Set
# RADIO

names_radio_10_4w <- electr_10_labels %>%
        str_subset('Listened to this radio station in the past 4 weeks') %>%
        str_replace('.+-','') %>%
        str_trim()

names_radio_10_4w <- names_radio_10_4w[1:94] # get rid of "unsure" and "none" and summaries


# most radio stations in 4 weeks, so use that to create names list
names_radio_10 <- names_radio_10_4w

# check_radio <- readRDS("names_radio_12_copy.rds")
# fix(names_radio_10)
# 
# saveRDS(names_radio_10, "names_radio_10.rds")
names_radio_10 <- readRDS('names_radio_10.rds')

names_radio_10_7 <- electr_10_labels %>%
        str_subset('Listened to this radio station in the past 7 days') %>%
        str_replace('.+-','') %>%
        str_trim()

names_radio_10_7 <- names_radio_10_7[1:83] # get rid of "unsure" and "none" and summaries


names_radio_10_y <- electr_10_labels %>%
        str_subset('Listened to this radio station yesterday') %>%
        str_replace('.+-','') %>%
        str_trim()

names_radio_10_y <- names_radio_10_y[2:67] # get rid of "unsure" and "none" and summaries

# get data...
# 4 weeks:
colnames_4weeks <- electr_10_labels %>%
        str_subset('Listened to this radio station in the past 4 weeks') %>%
        str_replace('Listened.+','') %>%
        str_trim()
colnames_4weeks <- colnames_4weeks[1:94]
radio4weeks_10 <- electr_10[,names(electr_10) %in% colnames_4weeks]

# 7 days
colnames_7days <- electr_10_labels %>%
        str_subset('Listened to this radio station in the past 7 days') %>%
        str_replace('Listened.+','') %>%
        str_trim()
colnames_7days <- colnames_7days[1:83]
radio7days_10 <- electr_10[,names(electr_10) %in% colnames_7days]

# yesterday
colnames_yesterday <- electr_10_labels %>%
        str_subset('Listened to this radio station yesterday') %>%
        str_replace('Listened.+','') %>%
        str_trim()
colnames_yesterday <- colnames_yesterday[2:67]
radioYesterday_10 <- electr_10[,names(electr_10) %in% colnames_yesterday]

# want to create two sparser sets for 7 days & yesterday to add up with 4week set

length(names_radio_10_4w[which(!names_radio_10_4w %in% names_radio_10_7)])
length(names_radio_10_4w[which(!names_radio_10_4w %in% names_radio_10_y)])

ind_7 <- c(38, 42, 47, 66, 70, 74, 75, 76, 88, 93, 94)
ind_y <- c(19, 35, 37, 38, 42, 45, 47, 58, 62, 66, 67, 70, 71, 72, 73, 74, 75, 76, 82, 83, 86, 87, 88, 89, 90, 91, 93, 94)

mat_7 <- data.frame(matrix(0, nrow = 25160, ncol = 94))
mat_y <- data.frame(matrix(0, nrow = 25160, ncol = 94))

mat_7[,-ind_7] <- radio7days_10
mat_y[,-ind_y] <- radioYesterday_10
        
radio_engagement_10 <- radio4weeks_10 + mat_7 + mat_y
names(radio_engagement_10) <- names_radio_10

saveRDS(radio_engagement_10, "radio_engagement_10.rds")
radio_engagement_10 <- readRDS("radio_engagement_10.rds")


## TV

# check with 2012 names:
check_tv_names_12 <- readRDS("names_tv_12.rds")
# names_tv_10 <- electr_10_labels %>%
#         str_subset('Watched.+4\\sweeks') %>%
#         str_replace('.+Watched\\s','') %>%
#         str_replace('in\\sthe\\sPAST\\s4\\sWEEKS','') %>%
#         str_trim()
# 
# saveRDS(names_tv_10, "names_tv_10.rds")
# names_tv_10 <- readRDS("names_tv_10.rds")

# want to isolate only past 4 weeks (no DSTV yet)
tv4weeks_10 <- electr_10[,c('ca50co11_9', # "e TV"
                            'ca50co13_7', # "MNet Main" 
                            'ca50co13_9', # "MNet Community"
                            'ca50co15_7', # "SABC 1"
                            'ca50co15_8', # "SABC 2"
                            'ca50co15_9', # "SABC 3"
                            'ca50co24_2', # "Cape Town TV" 
                            'ca50co16_2' # "Soweto TV"
                            )] 

# want to isolate only past 7 days...(no DSTV yet)
tv7days_10 <- electr_10[,c('ca50co31_9', # "e TV"
                           'ca50co33_7', # "MNet Main" 
                           'ca50co33_9', # "MNet Community"
                           'ca50co35_7', # "SABC 1"
                           'ca50co35_8', # "SABC 2"
                           'ca50co35_9', # "SABC 3"
                           'ca50co44_2', # "Cape Town TV" 
                           'ca50co36_2' # "Soweto TV"
                           )] 

# want to isolate only yesterday...(no DSTV yet)
tvYesterday_10 <- electr_10[,c('ca50co51_9', # "e TV"
                               'ca50co53_7', # "MNet Main" 
                               'ca50co53_9', # "MNet Community"
                               'ca50co55_7', # "SABC 1"
                               'ca50co55_8', # "SABC 2"
                               'ca50co55_9', # "SABC 3"
                               'ca50co64_2', # "Cape Town TV" 
                               'ca50co56_2' # "Soweto TV"
                               )]


# dealing with complicated DSTV issue:
# will identify list of variables for each time period that will serve as
# proxy or indication of dstv viewing for each period.

# 4 weeks:
dstv_10_4w <- electr_10[,c('ca50co09_1', # Africa Magic
                'ca50co10_6', # Cartoon Network
                'ca50co11_0', # Channel O
                'ca50co11_6', # Discovery Channel
                'ca50co12_3', # ESPN
                'ca50co12_6', # Fashion TV
                'ca50co12_7', # History Channel
                'ca50co13_3', # KykNET
                'ca50co14_0', # Mnet Movies 1
                'ca50co14_1', # Mnet Movies 2
                'ca50co14_4', # MTV
                'ca50co14_7', #National Geographic Channel
                'ca50co14_9', # News24
                'ca50co16_0', # Sony Entertainment Television
                'ca50co16_5', # Supersports1
                'ca50co16_6', # Supersports2
                'ca50co16_7', # Supersports3
                'ca50co16_8', # Supersports4
                'ca50co17_5' # Travel Channel
                )]
# create single vector:

dstv_10_4w_vec <- rowSums(dstv_10_4w)
dstv_10_4w_vec <- ifelse(dstv_10_4w_vec != 0, 1, 0)

# 7 days:
dstv_10_7d <- electr_10[,c('ca50co29_1', # Africa Magic
                'ca50co30_6', # Cartoon Network
                'ca50co31_0', # Channel O
                'ca50co31_6', # Discovery Channel
                'ca50co32_3', # ESPN
                'ca50co32_6', # Fashion TV
                'ca50co32_7', # History Channel
                'ca50co33_3', # KykNET
                'ca50co34_0', # Mnet Movies 1
                'ca50co34_1', # Mnet Movies 2
                'ca50co34_4', # MTV
                'ca50co34_7', # National Geographic Channel
                'ca50co34_9', # News24
                'ca50co36_0', # Sony Entertainment Television
                'ca50co36_5', # Supersports1
                'ca50co36_6', # Supersports2
                'ca50co36_7', # Supersports3
                'ca50co36_8', # Supersports4
                'ca50co37_5' # Travel Channel
                )]
dstv_10_7d_vec <- rowSums(dstv_10_7d)
dstv_10_7d_vec <- ifelse(dstv_10_7d_vec != 0, 1, 0)

# yesterday:
dstv_10_y <- electr_10[,c('ca50co49_1', # Africa Magic
                'ca50co50_6', # Cartoon Network
                'ca50co51_0', # Channel O
                'ca50co51_6', # Discovery Channel
                'ca50co52_3', # ESPN
                'ca50co52_6', # Fashion TV
                'ca50co52_7', # History Channel
                'ca50co53_3', # KykNET
                'ca50co54_0', # Mnet Movies 1
                'ca50co54_1', # Mnet Movies 2
                'ca50co54_4', # MTV
                'ca50co54_7', # National Geographic Channel
                'ca50co54_9', # News24
                'ca50co56_0', # Sony Entertainment Television
                'ca50co56_5', # Supersports1
                'ca50co56_6', # Supersports2
                'ca50co56_7', # Supersports3
                'ca50co56_8', # Supersports4
                'ca50co57_5' # Travel Channel
                )]
dstv_10_y_vec <- rowSums(dstv_10_y)
dstv_10_y_vec <- ifelse(dstv_10_y_vec != 0, 1, 0)

dstv_10 <- dstv_10_4w_vec + dstv_10_7d_vec + dstv_10_y_vec

# combining into a tv engagement dataset first not including dstv:
tv_engagement_10 <- tv4weeks_10 + tv7days_10 + tvYesterday_10

tv_engagement_10 <- tv_engagement_10 %>%
        mutate(DSTV = dstv_10)

names(tv_engagement_10) <- c("e TV",
                             "MNet Main",
                             "MNet Community",
                             "SABC 1",
                             "SABC 2",
                             "SABC 3",
                             "Cape Town TV",
                             "Soweto TV",
                             "DSTV")

saveRDS(tv_engagement_10, "tv_engagement_10.rds")

tv_engagement_10 <- readRDS("tv_engagement_10.rds")

## 3rd Internet Media Set

## accessed: sum of 12 months, 4weeks, 7days and yesterday
internet_level1 <- internet_10[,str_detect(names(internet_10), 'ca47co(41)|(42)|(43)|(44)')]

#change all 2 = "No" and NA's' to 0
internet_level1 <- data.frame(ifelse(is.na(internet_level1) | internet_level1 == 2, 0, 1))

internet_level1 <- rowSums(internet_level1)

# what internet was accessed for...
##  (maybe could use similar to vehicles?? as well as add up and multiply with first eng):

internet_level2 <- internet_10[,str_detect(names(internet_10), 'ca47co(50_1)|(50_6)|(51_6)|(51_7)|(51_8)|(51_9)')]

names(internet_level2) <- c('int_search',
                          'int_social',
                          'int_radio',
                          'int_tv',
                          'int_print',
                          'int_news')

## create single dataframe for internet multiplying internet_level1 with sum of internet_level2:
internet_engagement_10 <- internet_level2  * internet_level1

saveRDS(internet_engagement_10, "internet_engagement_10.rds")

internet_engagement_10 <- readRDS("internet_engagement_10.rds")

## create single dataframe for media10, including total_engagement columns (consider using media groupings .. follow up on this!)

# Level 1: Type
media_type_10 <- data.frame(cbind(qn = print_10$qn,
                                  scale(rowSums(newspapers_engagement_10)),
                                  scale(rowSums(magazines_engagement_10)),
                                  scale(rowSums(radio_engagement_10)),
                                  scale(rowSums(tv_engagement_10)),
                                  scale(rowSums(internet_engagement_10))))
names(media_type_10) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")
# Level 2: Vehicles
media_vehicles_10 <- data.frame(cbind(qn = print_10$qn,
                                      newspapers_engagement_10,
                                      magazines_engagement_10,
                                      radio_engagement_10,
                                      tv_engagement_10,
                                      internet_engagement_10))

saveRDS(media_type_10, 'media_type_10.rds')
saveRDS(media_vehicles_10, 'media_vehicles_10.rds')

media_type_10 <- readRDS("media_type_10.rds")
media_vehicles_10 <- readRDS("media_vehicles_10.rds")

## 4th Demographics Set (see notes for descriptions)



age <- personal_10[,'ca56co34'] -1 # for some reason 2-9 vs 1-8 elsewhere (double check this)
sex <- demogrs_10[,'ca91co51a']
edu <- demogrs_10[,'ca91co48']
for(i in 1: length(edu)) {
        if(edu[i] %in% c(6,7)) {
                edu[i] <- edu[i] + 1
        }
        else if(edu[i] == 8) {
                edu[i] <- 6
        }
}
hh_inc <- demogrs_10[,'ca91co50']
race <- demogrs_10[,'ca91co51b']
province <- demogrs_10[,'ca91co56']


metro1 <- demogrs_10[,'ca91co57'] # NB... reconsider all to greater jhb..ie including soweto..
metro2 <- demogrs_10[,'ca91co58'] 
metro <- rowSums(cbind(metro1, # NB...use the combination to determine non metropolitan respondents
                       metro2), na.rm = TRUE)

# collect and code into single metro set:
#0 = no metro
#1 Cape Town
#2 Cape Town Fringe Area
#3 Port Elizabeth/Uitenhage
#4 East London
#5 Durban
#6 Bloemfontein
#7 Greater Johannesburg
#8 Reef
#9 Pretoria
#10 Kimberley
##11 Pietermaritzburg
##12 Vaal
##13 Welkom

metro <- ifelse(metro == 7 | metro == 19, 7, metro) # Soweto back into greater jhb
metro <- ifelse(metro %in% c(8,23,24), 8,metro)
metro <- ifelse(metro %in% c(13), 12,metro)
metro <- ifelse(metro %in% c(14), 13,metro)

lang <- demogrs_10[,'ca91co75'] + 1 # change 0 to 1, so add one to all
lifestages <- demogrs_10[,'ca91co77']
mar_status <- personal_10[,'ca56co09']
# pers_inc1 <- personal_10[,'ca57co61']
# pers_inc2 <- personal_10[,'ca57co62'] + 10
# pers_inc3 <- personal_10[,'ca57co63'] + 20
# pers_inc4 <- personal_10[,'ca57co64'] + 30
# for(i in 1: length(pers_inc4)) {
#         if(!is.na(pers_inc4[i])) {
#                 if(pers_inc4[i] == 31) {
#                         pers_inc4[i] <- 0
#                 }
#                 if(pers_inc4[i] == 32) {
#                         pers_inc4[i] <- 60
#                 }
#         }
# }
# pers_inc <- rowSums(cbind(pers_inc1,
#                           pers_inc2,
#                           pers_inc3,
#                           pers_inc4), na.rm = TRUE)
lsm <- lsm_10[,'ca91co64']
lsm <- ifelse(lsm == 0,10,lsm)

lifestyle <- lsm_10[,'ca58co39'] + 1 # to get rid of zero

attitudesA <- lsm_10[,'ca76co10'] + 1 # to get rid of zeros
attitudesB <- lsm_10[,'ca76co10_lsm']


attitudesA <- ifelse(is.na(attitudesA), 0, attitudesA)
attitudesB <- ifelse(is.na(attitudesB), 0, attitudesB)
attitudes <- attitudesA + attitudesB
attitudes <- ifelse(attitudes == 12, 4, attitudes) # distants rooted
attitudes <- ifelse(attitudes == 5 | attitudes == 6, attitudes + 1, attitudes)
attitudes <- ifelse(attitudes == 13, 5, attitudes)
table(attitudes) # check


demographics_10 <- data.frame(qn = print_10$qn,
                              pwgt = print_10$pwgt,
                              age,
                              sex,
                              edu,
                              hh_inc,
                              race,
                              province,
                              metro,
                              lang,
                              lifestages,
                              mar_status,
                              # pers_inc,
                              lsm,
                              lifestyle,
                              attitudes)


# save as

saveRDS(demographics_10, "demographics_10.rds")
demographics_10 <- readRDS("demographics_10.rds")

# libraries
library(stringr)
library(tidyverse)
library(caret)

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

names_issues_print_10 <- str_subset(print_10_labels, 'Number of different issues usually read or page through') %>%
        str_replace('.+\\s-', '') %>%
        str_trim()
vars_issues_print_10 <- str_subset(print_10_labels, 'Number of different issues usually read or page through') %>%
        str_replace('Number\\sof\\sdifferent.+', '') %>%
        str_trim()

##Newspapers
# fix names and get rid of some and save
# names_newspapers_10_issues <- names_issues_print_10[c(1:50)]
# fix(names_newspapers_10_issues)
# saveRDS(names_newspapers_10_issues, "names_newspapers_10_issues.rds")
names_newspapers_10_issues <- readRDS("names_newspapers_10_issues.rds")

# vector of variables
vars_newspapers_10_issues <- vars_issues_print_10[c(1:50)]
issues_newspapers_10 <- print_10[,vars_newspapers_10_issues]

# Magazines
# fix names and get rid of some (including MNet guides and save
# names_magazines_10_issues <- names_issues_print_10[c(52:65,67:79,81:87,89:107,110:134,136:165,167)]
# fix(names_magazines_10_issues)
# saveRDS(names_magazines_10_issues, "names_magazines_10_issues.rds")
names_magazines_10_issues <- readRDS("names_magazines_10_issues.rds")

# vector of variables
vars_magazines_10_issues <- vars_issues_print_10[c(52:65,67:79,81:87,89:107,110:134,136:165,167)]
issues_magazines_10 <- print_10[,vars_magazines_10_issues]

## THOUROUGHLY
names_thorough_print_10 <- str_subset(print_10_labels, 'How thoroughly respondent usually read') %>%
        str_replace('.+\\s-', '') %>%
        str_replace("\\'",'') %>%
        str_trim()
vars_thorough_print_10 <- str_subset(print_10_labels, 'How thoroughly respondent usually read') %>%
        str_replace('How\\sthoroughly.+', '') %>%
        str_trim()

##Newspapers
# get names and get rid of some and save (already sorted above)
# names_newspapers_10_thorough <- names_thorough_print_10[c(1:39,77)]
# fix(names_newspapers_10_thorough)
# saveRDS(names_newspapers_10_issues, "names_newspapers_10_issues.rds")

# vector of variables
vars_newspapers_10_thorough <- vars_thorough_print_10[c(1:50)]
thorough_newspapers_10 <- print_10[,vars_newspapers_10_thorough]
thorough_newspapers_10 <- 7 - thorough_newspapers_10

# Magazines
# fix names and get rid of some and save
# names_magazines_10_thorough <- names_thorough_print_10[c(77:99,103:107,109:157)]
# fix(names_magazines_10_issues)
# saveRDS(names_magazines_10_issues, "names_magazines_10_issues.rds")

# vector of variables
vars_magazines_10_thorough <- vars_thorough_print_10[c(52:65,67:79,81:87,89:107,110:134,136:165,167)]
thorough_magazines_10 <- print_10[,vars_magazines_10_thorough]

# # need to reverse numbering to serve as weights (see value_lables text file):
thorough_magazines_10 <- 7 - thorough_magazines_10

# create datasets ...for newspapers and magazines:
newspapers_engagement_10_all <- issues_newspapers_10 * thorough_newspapers_10
names(newspapers_engagement_10_all) <- names_newspapers_10_issues
magazines_engagement_10_all <- issues_magazines_10 * thorough_magazines_10
names(magazines_engagement_10_all) <- names_magazines_10_issues

newspapers_engagement_10_simple_all <- issues_newspapers_10
names(newspapers_engagement_10_simple_all) <- names_newspapers_10_issues
magazines_engagement_10_simple_all <- issues_magazines_10
names(magazines_engagement_10_simple_all) <- names_magazines_10_issues

# # # replace NAs with zeros
newspapers_engagement_10_all[is.na(newspapers_engagement_10_all)] <- 0
magazines_engagement_10_all[is.na(magazines_engagement_10_all)] <- 0

newspapers_engagement_10_simple_all[is.na(newspapers_engagement_10_simple_all)] <- 0
magazines_engagement_10_simple_all[is.na(magazines_engagement_10_simple_all)] <- 0

# save (alls)
saveRDS(newspapers_engagement_10_all, "newspapers_engagement_10_all.rds")
saveRDS(magazines_engagement_10_all, "magazines_engagement_10_all.rds")
saveRDS(newspapers_engagement_10_simple_all, "newspapers_engagement_10_simple_all.rds")
saveRDS(magazines_engagement_10_simple_all, "magazines_engagement_10_simple_all.rds")

## CLEAN UP and reduce variables
# for newspapers: include "Soccer Week", "Sondag" and "The Zimbabwean" as "other.news"
other.news <- as.vector(apply(newspapers_engagement_10_all[,c(34,35,49)], 1, mean))
newspapers_engagement_10 <- newspapers_engagement_10_all %>%
        mutate(other.news = other.news)
newspapers_engagement_10 <- newspapers_engagement_10[,-c(34,35,49)]

other.news_simple <- as.vector(apply(newspapers_engagement_10_simple_all[,c(34,35,49)], 1, mean))
newspapers_engagement_10_simple <- newspapers_engagement_10_simple_all %>%
        mutate(other.news = other.news_simple)
newspapers_engagement_10_simple <- newspapers_engagement_10_simple[,-c(34,35,49)]

# for magazines - deal with it in vehicle_cleaning project
magazines_engagement_10 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/vehicle_cleaning/magazines_engagement_10.rds")
magazines_engagement_10_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/vehicle_cleaning/magazines_engagement_10_simple.rds")

# save them in this project
saveRDS(newspapers_engagement_10, "newspapers_engagement_10.rds")
saveRDS(magazines_engagement_10, "magazines_engagement_10.rds")
saveRDS(newspapers_engagement_10_simple, "newspapers_engagement_10_simple.rds")
saveRDS(magazines_engagement_10_simple, "magazines_engagement_10_simple.rds")

magazines_engagement_10 <- readRDS("magazines_engagement_10.rds")
newspapers_engagement_10 <- readRDS("newspapers_engagement_10.rds")
magazines_engagement_10_simple <- readRDS("magazines_engagement_10_simple.rds")
newspapers_engagement_10_simple <- readRDS("newspapers_engagement_10_simple.rds")

## 2nd Electronic Media Set
# RADIO

names_radio_10_4w <- electr_10_labels %>%
        str_subset('Listened to this radio station in the past 4 weeks') %>%
        str_replace('.+-','') %>%
        str_trim()

# names_radio_10_4w <- names_radio_10_4w[c(1:94,97)] # get rid of "unsure" and "none" and summaries
# 

# most radio stations in 4 weeks, so use that to create names list
# names_radio_10 <- names_radio_10_4w

# # check_radio <- readRDS("names_radio_12_copy.rds")
# fix(names_radio_10)
# 
# saveRDS(names_radio_10, "names_radio_10.rds")
names_radio_10 <- readRDS('names_radio_10.rds')

names_radio_10_7 <- electr_10_labels %>%
        str_subset('Listened to this radio station in the past 7 days') %>%
        str_replace('.+-','') %>%
        str_trim()

names_radio_10_7 <- names_radio_10_7[c(1:83, 86)] # get rid of "unsure" and "none" and summaries


names_radio_10_y <- electr_10_labels %>%
        str_subset('Listened to this radio station yesterday') %>%
        str_replace('.+-','') %>%
        str_trim()

names_radio_10_y <- names_radio_10_y[c(2:67, 70)] # get rid of "unsure" and "none" and summaries

# get data...
# 4 weeks:
colnames_4weeks <- electr_10_labels %>%
        str_subset('Listened to this radio station in the past 4 weeks') %>%
        str_replace('Listened.+','') %>%
        str_trim()
colnames_4weeks <- colnames_4weeks[c(1:94,97)]
radio4weeks_10 <- electr_10[,names(electr_10) %in% colnames_4weeks]

# 7 days
colnames_7days <- electr_10_labels %>%
        str_subset('Listened to this radio station in the past 7 days') %>%
        str_replace('Listened.+','') %>%
        str_trim()
colnames_7days <- colnames_7days[c(1:83, 86)]
radio7days_10 <- electr_10[,names(electr_10) %in% colnames_7days]

# yesterday
colnames_yesterday <- electr_10_labels %>%
        str_subset('Listened to this radio station yesterday') %>%
        str_replace('Listened.+','') %>%
        str_trim()
colnames_yesterday <- colnames_yesterday[c(2:67,70)]
radioYesterday_10 <- electr_10[,names(electr_10) %in% colnames_yesterday]

# want to create two sparser sets for 7 days & yesterday to add up with 4week set

length(names_radio_10_4w[which(!names_radio_10_4w %in% names_radio_10_7)])
length(names_radio_10_4w[which(!names_radio_10_4w %in% names_radio_10_y)])

ind_7 <- c(38, 42, 47, 66, 70, 74, 75, 76, 88, 93, 94)
ind_y <- c(19, 35, 37, 38, 42, 45, 47, 58, 62, 66, 67, 70, 71, 72, 73, 74, 75, 76, 82, 83, 86, 87, 88, 89, 90, 91, 93, 94)

mat_7 <- data.frame(matrix(0, nrow = 25160, ncol = 95))
mat_y <- data.frame(matrix(0, nrow = 25160, ncol = 95))

mat_7[,-ind_7] <- radio7days_10
mat_y[,-ind_y] <- radioYesterday_10
        
radio_engagement_10_all <- radio4weeks_10 + mat_7 + mat_y
names(radio_engagement_10_all) <- names_radio_10

saveRDS(radio_engagement_10_all, "radio_engagement_10_all.rds")
radio_engagement_10_all <- readRDS("radio_engagement_10_all.rds")

# AFTER CLEANING (see vehicle cleaning project)
radio_engagement_10 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/vehicle_cleaning/radio_engagement_10.rds")
#save in this workspace
saveRDS(radio_engagement_10, "radio_engagement_10.rds")
radio_engagement <- readRDS("radio_engagement_10.rds")
## TV

# check with 2012 names:
# check_tv_names_12 <- readRDS("names_tv_12.rds")
# names_tv_10 <- electr_10_labels %>%
#         str_subset('Watched.+4\\sweeks') %>%
#         str_replace('.+Watched\\s','') %>%
#         str_replace('in\\sthe\\sPAST\\s4\\sWEEKS','') %>%
#         str_trim()
# 
# saveRDS(names_tv_10, "names_tv_10.rds")
# names_tv_10 <- readRDS("names_tv_10.rds")

# want to isolate only past 4 weeks (no DSTV yet) # no "other tv
tv4weeks_10 <- electr_10[,c('ca50co11_9', # "e TV"
                            'ca50co15_7', # "SABC 1"
                            'ca50co15_8', # "SABC 2"
                            'ca50co15_9', # "SABC 3"
                            'ca50co24_2', # "Cape Town TV" 
                            'ca50co16_2' # "Soweto TV"
                            )] 

# want to isolate only past 7 days...(no DSTV yet)
tv7days_10 <- electr_10[,c('ca50co31_9', # "e TV"
                           'ca50co35_7', # "SABC 1"
                           'ca50co35_8', # "SABC 2"
                           'ca50co35_9', # "SABC 3"
                           'ca50co44_2', # "Cape Town TV" 
                           'ca50co36_2' # "Soweto TV"
                           )] 

# want to isolate only yesterday...(no DSTV yet)
tvYesterday_10 <- electr_10[,c('ca50co51_9', # "e TV"
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

names(tv_engagement_10) <- c("e tv",
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
internet_engagement_10_simple <- internet_level1

saveRDS(internet_engagement_10, "internet_engagement_10.rds")
saveRDS(internet_engagement_10_simple, "internet_engagement_10_simple.rds")

internet_engagement_10 <- readRDS("internet_engagement_10.rds")
internet_engagement_10_simple <- readRDS("internet_engagement_10_simple.rds")

## create single dataframe for media10, including total_engagement columns (consider using media groupings .. follow up on this!)

# Level 1: Type
media_type_10 <- data.frame(cbind(qn = print_10$qn,
                                  rowSums(scale(newspapers_engagement_10)),
                                  rowSums(scale(magazines_engagement_10)),
                                  rowSums(scale(radio_engagement_10)),
                                  rowSums(scale(tv_engagement_10)),
                                  rowSums(scale(internet_engagement_10))))
names(media_type_10) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")
media_type_10 <- media_type_10 %>%
        mutate(all = as.vector(newspapers + magazines + radio + tv + internet)) 

media_type_10_simple <- data.frame(cbind(qn = print_10$qn,
                                  rowSums(scale(newspapers_engagement_10_simple)),
                                  rowSums(scale(magazines_engagement_10_simple)),
                                  rowSums(scale(radio_engagement_10)),
                                  rowSums(scale(tv_engagement_10)),
                                  scale(internet_engagement_10_simple)))

names(media_type_10_simple) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")
media_type_10_simple <- media_type_10_simple %>%
        mutate(all = as.vector(newspapers + magazines + radio + tv + internet))


# Level 2: Vehicles
media_vehicles_10 <- data.frame(cbind(qn = print_10$qn,
                                      newspapers_engagement_10,
                                      magazines_engagement_10,
                                      radio_engagement_10,
                                      tv_engagement_10,
                                      internet_engagement_10))

media_vehicles_10_simple <- data.frame(cbind(qn = print_10$qn,
                                      newspapers_engagement_10_simple,
                                      magazines_engagement_10_simple,
                                      radio_engagement_10,
                                      tv_engagement_10,
                                      internet_eng = internet_engagement_10_simple))

saveRDS(media_type_10, 'media_type_10.rds')
saveRDS(media_type_10_simple, 'media_type_10_simple.rds')
saveRDS(media_vehicles_10, 'media_vehicles_10.rds')
saveRDS(media_vehicles_10_simple, 'media_vehicles_10_simple.rds')

media_type_10 <- readRDS("media_type_10.rds")
media_type_10_simple <- readRDS("media_type_10_simple.rds")
media_vehicles_10 <- readRDS("media_vehicles_10.rds")
media_vehicles_10_simple <- readRDS("media_vehicles_10_simple.rds")

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




lifestages <- demogrs_10[,'ca91co77'] # need to sort this out.... check out files, otherwise drop it..




mar_status <- personal_10[,'ca56co09']

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
                              lsm,
                              lifestyle,
                              attitudes)

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

# lsm
demographics_10$lsm <- ifelse(demographics_10$lsm %in% c(1,2), 1, demographics_10$lsm)
demographics_10$lsm <- ifelse(demographics_10$lsm %in% c(3,4), 2, demographics_10$lsm)
demographics_10$lsm <- ifelse(demographics_10$lsm %in% c(5,6), 3, demographics_10$lsm)
demographics_10$lsm <- ifelse(demographics_10$lsm %in% c(7,8), 4, demographics_10$lsm)
demographics_10$lsm <- ifelse(demographics_10$lsm %in% c(9,10), 5, demographics_10$lsm)
demographics_10$lsm <- factor(demographics_10$lsm, ordered = TRUE)

demographics_10$lifestyle <- factor(demographics_10$lifestyle, ordered = FALSE)
demographics_10$attitudes <- factor(demographics_10$attitudes, ordered = FALSE)

saveRDS(demographics_10, "demographics_10.rds")
demographics_10 <- readRDS("demographics_10.rds")


# read datafiles again (if necessary)
magazines_engagement_10 <- readRDS("magazines_engagement_10.rds")
magazines_engagement_10_simple <- readRDS("magazines_engagement_10_simple.rds")
newspapers_engagement_10 <- readRDS("newspapers_engagement_10.rds")
newspapers_engagement_10_simple <- readRDS("newspapers_engagement_10_simple.rds")
radio_engagement_10 <- readRDS("radio_engagement_10.rds")
tv_engagement_10 <- readRDS("tv_engagement_10.rds")
internet_engagement_10 <- readRDS("internet_engagement_10.rds")
internet_engagement_10_simple <- readRDS("internet_engagement_10_simple.rds")

media_type_10 <- readRDS("media_type_10.rds")
media_type_10_simple <- readRDS("media_type_10_simple.rds")
media_vehicles_10 <- readRDS("media_vehicles_10.rds")
media_vehicles_10_simple <- readRDS("media_vehicles_10_simple.rds")

demographics_10 <- readRDS("demographics_10.rds")


# #create single dataset minus non metropolitans
set10 <- demographics_10 %>%
        left_join(media_type_10) %>%
        left_join(media_vehicles_10)
#%>%filter(metro != 0)

set10_simple <- demographics_10 %>%
        left_join(media_type_10_simple) %>%
        left_join(media_vehicles_10_simple)
#%>%filter(metro != 0)

# get rid of zero variances:
ind_10 <- nearZeroVar(set10[,16:ncol(set10)], saveMetrics = TRUE)
good_set <- set10[,16:ncol(set10)][,!ind_10$zeroVar]
set10 <- data.frame(cbind(set10[,1:15], good_set))

ind_10_simple <- nearZeroVar(set10_simple[,16:ncol(set10_simple)], saveMetrics = TRUE)
good_set_simple <- set10_simple[,16:ncol(set10_simple)][,!ind_10_simple$zeroVar]
set10_simple <- data.frame(cbind(set10_simple[,1:15], good_set_simple))


# scale media type and media vehicles
set10[,16:ncol(set10)] <- scale(set10[,16:ncol(set10)])
set10_simple[,16:ncol(set10_simple)] <- scale(set10_simple[,16:ncol(set10_simple)])

# save them:
saveRDS(set10, "set10.rds") # NB lifestages is NA...not available for 2010..
saveRDS(set10_simple, "set10_simple.rds")


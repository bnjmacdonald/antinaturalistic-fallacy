# file: cleaning.r
# This file reads in and cleans the raw survey data for wave 2 of the
# anti-naturalistic fallacy survey experiment.
# 
# Note: the resulting `wave1_clean.csv` data file INCLUDES responses from the
# beliefs study replication in the "veg" and "no products" treatments. These
# individuals are dropped in `merging_waves.r`, since all individuals in the
# naturalistic fallacy study must have been exposed to cultured meat
# information at baseline.

library(stringr)

# library(readstata13)
source('utils.r')

len <- length
tab <- function(..., exclude=NULL) table(..., exclude=exclude)

# reads in baseline data.
# df1 <- read.dta13("../data/raw/batches_baseline.dta")

# reads in followup data.
# df2 <- read.dta13("../data/raw/followup.dta")
# df <- read.dta('../data/raw/followup.dta')

# reads in treatment wave data.

df1 <- read_csv_qualtrics('../data/raw/Cultured_meat_revisions__naturalistic_baseline__backup_v1.csv')
colnames(df1)
dim(df1)

df2 <- read_csv_qualtrics('../data/raw/Cultured_meat_revisions__naturalistic_baseline__backup_v2.csv')
colnames(df2)
dim(df2)

# renames MTurkCode variable, since it actually represents completion code.
colnames(df1)[which(colnames(df1) == 'MTurkCode')] <- 'completionCode'
colnames(df2)[which(colnames(df2) == 'MTurkCode')] <- 'completionCode'
if ('MTurkCode' %in% colnames(df1) | 'MTurkCode' %in% colnames(df2)) stop('MTurkCode not successfully renamed.')

# CLEANING OUTCOME VARIABLES
var_names_dict <- list()  # dict holding variable name mappings.
var_names_dict[['V1']] <- 'respID'
var_names_dict[['completionCode']] <- 'completionCode'

tab(is.na(df1[,'completionCode']))
tab(is.na(df2[,'completionCode']))


# DIET
# TODO.

# FFQ (Q5_1-Q5_16) - number of servings per week.
# as an estimate for number of servings per week, I took the mid-point of each range 
# never = 0
# less than 1/week = 1
# 1-3 servings per week = 2
# 4-6 servings per week = 5
# 1 serving per day = 7
# 2-3 servings per day = 17.5 (2.5*7)
# 4 or more servings per day = 28 (4*7)

servings = list(
    "never"=0,
    "&lt; 1 serving per week"=1,
    "1-3 servings per week"=2,
    "4-6 servings per week"=5,
    "1 serving per day"=7,
    "2-3 servings per day"=17.5,
    "4 or more servings per day"=28
)

ffq_cols <- c(paste('Q5_', 1:12, '..1.1', sep=''), paste('Q5_', 13:16, '..1', sep=''))

df1_ffq_temp <- apply(df1[,ffq_cols], MARGIN=2, FUN=function(x) as.numeric(as.character(factor(x, levels=names(servings), labels=unlist(servings)))))
df2_ffq_temp <- apply(df2[,ffq_cols], MARGIN=2, FUN=function(x) as.numeric(as.character(factor(x, levels=names(servings), labels=unlist(servings)))))
tab(df1[,ffq_cols[5]], df1_ffq_temp[,5])
tab(df2[,ffq_cols[13]], df2_ffq_temp[,13])

df1[,ffq_cols] <- df1_ffq_temp
df2[,ffq_cols] <- df2_ffq_temp


# FFQ VARIABLES:
colnames_ffq <- c('FFQfreqDairy', 'FFQfreqChicken', 'FFQfreqTurkey',
                'FFQfreqFish', 'FFQfreqPork', 'FFQfreqBeef',
                'FFQfreqOther', 'FFQfreqEggs','FFQfreqFruit',
                'FFQfreqVegetables','FFQfreqNuts', 'FFQfreqBeans',
                'FFQfreqVegMeats', 'FFQfreqGrains', 'FFQfreqFats', 'FFQfreqSugar')
stopifnot(len(colnames_ffq)==16)

# FFQ SUMMING ACROSS MEATS

# a. FFQ MEAT SUM TOTAL (multiplied by serving size)
colnames_meat <- c('FFQfreqChicken', 'FFQfreqTurkey', 'FFQfreqFish', 'FFQfreqPork', 'FFQfreqBeef', 'FFQfreqOther')
meat_cols <- ffq_cols[which(colnames_ffq %in% colnames_meat)]

df1$ffq_total_sum_meat <- rowSums(df1[,meat_cols], na.rm=TRUE)
df2$ffq_total_sum_meat <- rowSums(df2[,meat_cols], na.rm=TRUE)
# rows with NA are treated as 0 in rowSums. we don't want this, so any individual with more than 3 NAs gets an ffq_total_sum_meat of NA:
threshold <- 3
num_meat_na1 <- rowSums(is.na(df1[,meat_cols]))
num_meat_na2 <- rowSums(is.na(df2[,meat_cols]))
df1$ffq_total_sum_meat[num_meat_na1 >= threshold] <- NA
df2$ffq_total_sum_meat[num_meat_na2 >= threshold] <- NA

summary(df1[c(ffq_cols, 'ffq_total_sum_meat')])
summary(df2[c(ffq_cols, 'ffq_total_sum_meat')])

# adds ffq columns to var_names_dict
var_names_dict[['ffq_total_sum_meat']] <- 'ffq_total_sum_meat'
for (i in 1:len(ffq_cols)) {
    var_names_dict[[ffq_cols[i]]] <- colnames_ffq[i]
}


# OUTCOME (Q37_1): "Now please rate the following according to how difficult you think each goal would be for you. Th...-Completely eliminating conventional meat products from your diet in the next year"
var_names_dict[['Q37_1']] <- 'ease_eliminate'
tab(df1$Q37_1)
tab(df2$Q37_1)
unique(df1$Q37_1, na.rm=FALSE)
unique(df2$Q37_1, na.rm=FALSE)
temp_Q37_1_1 <- as.numeric(ordered(df1$Q37_1, levels=c("Very difficult", "Difficult", "Somewhat difficult", "Neutral", "Somewhat easy", "Easy", "Very easy")))
temp_Q37_1_2 <- as.numeric(ordered(df2$Q37_1, levels=c("Very difficult", "Difficult", "Somewhat difficult", "Neutral", "Somewhat easy", "Easy", "Very easy")))
tab(temp_Q37_1_1, df1$Q37_1)
tab(temp_Q37_1_2, df2$Q37_1)
df1$Q37_1 <- temp_Q37_1_1
df2$Q37_1 <- temp_Q37_1_2

# OUTCOME (Q37_2): "Now please rate the following according to how difficult you think each goal would be for you. Th...-Reducing your consumption of conventional meat products by 25% in the next year"
var_names_dict[['Q37_2']] <- 'ease_reduce'
tab(df1$Q37_2)
tab(df2$Q37_2)
unique(df1$Q37_2, na.rm=FALSE)
unique(df2$Q37_2, na.rm=FALSE)
temp_Q37_2_1 <- as.numeric(ordered(df1$Q37_2, levels=c("Very difficult", "Difficult", "Somewhat difficult", "Neutral", "Somewhat easy", "Easy", "Very easy")))
temp_Q37_2_2 <- as.numeric(ordered(df2$Q37_2, levels=c("Very difficult", "Difficult", "Somewhat difficult", "Neutral", "Somewhat easy", "Easy", "Very easy")))
tab(temp_Q37_2_1, df1$Q37_2)
tab(temp_Q37_2_2, df2$Q37_2)
df1$Q37_2 <- temp_Q37_2_1
df2$Q37_2 <- temp_Q37_2_2


# OUTCOME (Q109_1-Q109_8): "Do you have any concerns about clean meat products? You may
# select multiple items."
# 1: It seems unhealthy
# 2: It may not be safe to eat
# 3: It may not taste as good as conventional meat
# 4: It may be more expensive
# 5: It seems unnatural
# 6: I have no concerns
# 7: Other (specify):
# 7_text: Other (specify):-TEXT
# 8: There is no reason to avoid eating conventional meat products

# cleans concerns outcome

cols_Q109 <- paste('Q109_', 1:8, sep='')
df1 <- clean_multichoice(df1, cols_Q109)
df2 <- clean_multichoice(df2, cols_Q109)

df1[,'num_concerns'] <- rowSums(df1[,cols_Q109], na.rm=TRUE)
df2[,'num_concerns'] <- rowSums(df2[,cols_Q109], na.rm=TRUE)

# adds column names to var_names_dict
colnames_Q109 <- paste('concern_', c('unhealthy', 'unsafe', 'taste', 'cost', 'unnatural', 'none', 'other', 'noreason'), sep='')
for (i in 1:len(cols_Q109)) {
    var_names_dict[[cols_Q109[i]]] <- colnames_Q109[i]
}
var_names_dict[['num_concerns']] <- 'num_concerns'


# OUTCOME (Q390_1-Q390_8): "Do you perceive any expected benefits to this product? You may select multiple items."
# 1: "It seems healthier than conventional meat"
# 2: "It seems safer to eat than conventional meat"
# 3: "It may be tastier than conventional meat"
# 4: "It may be cheaper than conventional meat"
# 5: "It may be more environmentally sustainable"
# 6: "I foresee no benefits"
# 7: "Other (specify):"
# 7_text: other text
# 8: "It may reduce farm animal suffering"

# cleans benefits outcome
cols_Q390 <- paste('Q390_', 1:8, sep='')
df1 <- clean_multichoice(df1, cols_Q390)
df2 <- clean_multichoice(df2, cols_Q390)

df1[,'num_benefits'] <- rowSums(df1[,cols_Q390], na.rm=TRUE)
df2[,'num_benefits'] <- rowSums(df2[,cols_Q390], na.rm=TRUE)

# adds column names to var_names_dict
colnames_Q390 <- paste('benefit_', c('healthier', 'safer', 'tastier', 'cheaper', 'envsust', 'none', 'other', 'suffering'), sep='')
for (i in 1:len(cols_Q390)) {
    var_names_dict[[cols_Q390[i]]] <- colnames_Q390[i]
}
var_names_dict[['num_benefits']] <- 'num_benefits'


# constructs cost-benefit variable.
df1[,'cost_benefit'] <- df1[,'num_benefits'] - df1[,'num_concerns']
df2[,'cost_benefit'] <- df2[,'num_benefits'] - df2[,'num_concerns']
var_names_dict[['cost_benefit']] <- 'cost_benefit'
# summary(df1[,'cost_benefit'])


# OUTCOME (Q99): "How do you feel about clean meat products?"
var_names_dict[['Q99']] <- 'feel'
tab(df1$Q99)
tab(df2$Q99)
unique(df1$Q99, na.rm=FALSE)
unique(df2$Q99, na.rm=FALSE)
temp_Q99_1 <- as.numeric(ordered(df1$Q99, levels=c('Extremely negative', 'Moderately negative', 'Slightly negative', 'Neither positive nor negative', 'Slightly positive', 'Moderately positive', 'Extremely positive')))
temp_Q99_2 <- as.numeric(ordered(df2$Q99, levels=c('Extremely negative', 'Moderately negative', 'Slightly negative', 'Neither positive nor negative', 'Slightly positive', 'Moderately positive', 'Extremely positive')))
tab(temp_Q99_1, df1$Q99)
tab(temp_Q99_2, df2$Q99)
df1$Q99 <- temp_Q99_1
df2$Q99 <- temp_Q99_2


# INTEREST: Q101
var_names_dict[['Q101']] <- 'interest_purchase'
tab(df1$Q101)
tab(df2$Q101)
unique(df2$Q101, na.rm=FALSE)
temp1_Q101 <- as.numeric(ordered(df1$Q101, levels=c("Not interested at all", "Slightly interested", "Moderately interested", "Very interested", "Extremely interested")))
temp2_Q101 <- as.numeric(ordered(df2$Q101, levels=c("Not interested at all", "Slightly interested", "Moderately interested", "Very interested", "Extremely interested")))
tab(df1$Q101, temp1_Q101)
tab(df2$Q101, temp2_Q101)
df1$Q101 <- temp1_Q101
df2$Q101 <- temp2_Q101

# WOULD EAT: Q103
var_names_dict[['Q103']] <- 'would_eat'
tab(df1[,'Q103'])
tab(df2[,'Q103'])
unique(df2$Q103, na.rm=FALSE)
temp1_Q103 <- as.numeric(ordered(df1$Q103, levels=c('Definitely not', 'Probably not', 'Might or might not', 'Probably yes', 'Definitely yes')))
temp2_Q103 <- as.numeric(ordered(df2$Q103, levels=c('Definitely not', 'Probably not', 'Might or might not', 'Probably yes', 'Definitely yes')))
tab(df1$Q103, temp1_Q103)
tab(df2$Q103, temp2_Q103)
df1$Q103 <- temp1_Q103
df2$Q103 <- temp2_Q103

# EXPECT_REDUCE: Q191
var_names_dict[['Q191']] <- 'expect_reduce'
tab(df1[,'Q191'])
tab(df2[,'Q191'])

# EXPECT_REDUCE_AMT: Q193
var_names_dict[['Q193']] <- 'expect_reduce_amt'
tab(df1[,'Q193'])
tab(df2[,'Q193'])


# OUTCOME (Q195): "Would you like to receive more information about how to incorporate more vegetarian products in y..."
tab(df1$Q195)
tab(df2$Q195)

# creates indicator variable for 'yes' response.
df1[,'receive_veg_info_yes'] <- 0
df1[,'receive_veg_info_yes'][df1[,'Q195'] == 'Yes'] <- 1
df1[,'receive_veg_info_yes'][is.na(df1[,'Q195'])] <- NA

df2[,'receive_veg_info_yes'] <- 0
df2[,'receive_veg_info_yes'][df2[,'Q195'] == 'Yes'] <- 1
df2[,'receive_veg_info_yes'][is.na(df2[,'Q195'])] <- NA

tab(df1[,'receive_veg_info_yes'], df1[,'Q195'])
tab(df2[,'receive_veg_info_yes'], df2[,'Q195'])

# creates indicator variable for 'yes' or 'maybe' response.
df1[,'receive_veg_info_yesmaybe'] <- 0
df1[,'receive_veg_info_yesmaybe'][df1[,'Q195'] == 'Yes' | df1[,'Q195'] == 'Maybe'] <- 1
df1[,'receive_veg_info_yesmaybe'][is.na(df1[,'Q195'])] <- NA

df2[,'receive_veg_info_yesmaybe'] <- 0
df2[,'receive_veg_info_yesmaybe'][df2[,'Q195'] == 'Yes' | df2[,'Q195'] == 'Maybe'] <- 1
df2[,'receive_veg_info_yesmaybe'][is.na(df2[,'Q195'])] <- NA

tab(df1[,'receive_veg_info_yesmaybe'], df1[,'Q195'])
tab(df2[,'receive_veg_info_yesmaybe'], df2[,'Q195'])

# adds these two new variables to the var_names_dict
var_names_dict[['receive_veg_info_yes']] <- 'receive_veg_info_yes'
var_names_dict[['receive_veg_info_yesmaybe']] <- 'receive_veg_info_yesmaybe'

# OUTCOME (Q197): "Would you like to be notified when clean meat is available in your area?"
tab(df1$Q197, exclude=NULL)
tab(df2$Q197, exclude=NULL)

# creates indicator variable for 'yes' response.
df1[,'notified_available_yes'] <- 0
df1[,'notified_available_yes'][df1[,'Q197'] == 'Yes'] <- 1
df1[,'notified_available_yes'][is.na(df1[,'Q197'])] <- NA

df2[,'notified_available_yes'] <- 0
df2[,'notified_available_yes'][df2[,'Q197'] == 'Yes'] <- 1
df2[,'notified_available_yes'][is.na(df2[,'Q197'])] <- NA

tab(df1[,'notified_available_yes'], df1[,'Q197'])
tab(df2[,'notified_available_yes'], df2[,'Q197'])

# creates indicator variable for 'yes' or 'maybe' response.
df1[,'notified_available_yesmaybe'] <- 0
df1[,'notified_available_yesmaybe'][df1[,'Q197'] == 'Yes' | df1[,'Q197'] == 'Maybe'] <- 1
df1[,'notified_available_yesmaybe'][is.na(df1[,'Q197'])] <- NA

df2[,'notified_available_yesmaybe'] <- 0
df2[,'notified_available_yesmaybe'][df2[,'Q197'] == 'Yes' | df2[,'Q197'] == 'Maybe'] <- 1
df2[,'notified_available_yesmaybe'][is.na(df2[,'Q197'])] <- NA

tab(df1[,'notified_available_yesmaybe'], df1[,'Q197'])
tab(df2[,'notified_available_yesmaybe'], df2[,'Q197'])

# adds these two new variables to the var_names_dict
var_names_dict[['notified_available_yes']] <- 'notified_available_yes'
var_names_dict[['notified_available_yesmaybe']] <- 'notified_available_yesmaybe'

# OUTCOME (Q199_1_TEXT): "Please enter your e-mail address to receive this one-time notification. You may leave this field...-E-mail address."
var_names_dict[['entered_email']] <- 'entered_email'
df1[,'entered_email'] <- as.integer(!is.na(df1[,'Q199_1_TEXT']))
df2[,'entered_email'] <- as.integer(!is.na(df2[,'Q199_1_TEXT']))

# tab(df2[,'entered_email'])


# OTHER VARIABLES

# [veg_moral] Q21, Q175: "Some people say that eating vegetarian food is morally preferable to eating meat produced via fac..."
# NOTE: this question was asked twice in the same survey.
var_names_dict[['Q21']] <- 'veg_moral'
tab(df1$Q21)
tab(df2$Q21)
tab(df1$Q175)
unique(df1$Q21, na.rm=FALSE)
temp1_Q21 <- as.numeric(ordered(df1$Q21, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
temp2_Q21 <- as.numeric(ordered(df2$Q21, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
tab(df1$Q21, temp1_Q21)
tab(df2$Q21, temp2_Q21)
df1$Q21 <- temp1_Q21
df2$Q21 <- temp2_Q21

# [sentient] Q23, Q177: "Do you think that farm animals are sentient (feel pleasure or pain)?"
# NOTE: this question was asked twice in the same survey.
var_names_dict[['Q23']] <- 'sentient'
tab(df1$Q23)
tab(df2$Q23)
tab(df1$Q177)
unique(df1$Q23, na.rm=FALSE)
temp1_Q23 <- as.numeric(ordered(df1$Q23, levels=c('Definitely not', 'Probably not', 'Might or might not', 'Probably yes', 'Definitely yes')))
temp2_Q23 <- as.numeric(ordered(df2$Q23, levels=c('Definitely not', 'Probably not', 'Might or might not', 'Probably yes', 'Definitely yes')))
tab(df1$Q23, temp1_Q23)
tab(df2$Q23, temp2_Q23)
df1$Q23 <- temp1_Q23
df2$Q23 <- temp2_Q23


# [harms] Q25, Q179: "How much do you think conventional meat production harms animals?"
# NOTE: this question was asked twice in the same survey.
var_names_dict[['Q25']] <- 'harms'
tab(df1$Q25)
tab(df2$Q25)
tab(df1$Q179)
unique(df1$Q25, na.rm=FALSE)
temp1_Q25 <- as.numeric(ordered(df1$Q25, levels=c('Not at all', 'A little', 'A moderate amount', 'A lot', 'A great deal')))
temp2_Q25 <- as.numeric(ordered(df2$Q25, levels=c('Not at all', 'A little', 'A moderate amount', 'A lot', 'A great deal')))
tab(df1$Q25, temp1_Q25)
tab(df2$Q25, temp2_Q25)
df1$Q25 <- temp1_Q25
df2$Q25 <- temp2_Q25


# [harms_concern] Q27, Q181: "How concerned are you about this harm?"
# NOTE: this question was asked twice in the same survey.
var_names_dict[['Q27']] <- 'harms_concern'
tab(df1$Q27)
tab(df2$Q27)
tab(df1$Q181)
unique(df1$Q27, na.rm=FALSE)
temp1_Q27 <- as.numeric(ordered(df1$Q27, levels=c('Not at all', 'A little', 'A moderate amount', 'A lot', 'A great deal')))
temp2_Q27 <- as.numeric(ordered(df2$Q27, levels=c('Not at all', 'A little', 'A moderate amount', 'A lot', 'A great deal')))
tab(df1$Q27, temp1_Q27)
tab(df2$Q27, temp2_Q27)
df1$Q27 <- temp1_Q27
df2$Q27 <- temp2_Q27


# [perceived_reduce] Q393: 
# Please indicate the degree to which you agree or disagree with the following 
# statements.-More and more people in the U.S. are reducing their meat
# consumption.
var_names_dict[['Q393']] <- 'perceived_reduce'
tab(df1$Q393)
tab(df2$Q393)
unique(df1$Q393, na.rm=FALSE)
temp1_Q393 <- as.numeric(ordered(df1$Q393, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
temp2_Q393 <- as.numeric(ordered(df2$Q393, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
tab(df1$Q393, temp1_Q393)
tab(df2$Q393, temp2_Q393)
df1$Q393 <- temp1_Q393
df2$Q393 <- temp2_Q393


# DEMOGRAPHICS

# ZIPCODE: Q201
# var_names_dict[['Q201']] <- 'zipcode'
# tab(df1[,'Q201'])
# tab(df2[,'Q201'])
# TODO: change invalid zipcodes to NA.


# AGE: Q203
var_names_dict[['Q203']] <- 'age'
tab(df1[,'Q203'])
tab(df2[,'Q203'])

# GENDER: Q205
var_names_dict[['Q205']] <- 'gender'
df1[,'Q205'][df1[,'Q205']=='Other'] <- NA
df2[,'Q205'][df2[,'Q205']=='Other'] <- NA
tab(df1[,'Q205'])
tab(df2[,'Q205'])

# HIGHEST EDUCATION: Q207
var_names_dict[['Q207']] <- 'educ'
tab(df1[,'Q207'])
tab(df2[,'Q207'])
unique(df1$Q207, na.rm=FALSE)
unique(df2$Q207, na.rm=FALSE)
temp1_Q207 <- as.numeric(ordered(df1$Q207, levels=c('Less than high school degree', 'High school degree', 'Some college, no degree', 'Associate or technical degree', 'College degree', "Master's degree or other professional degree", 'Ph.D.')))
temp2_Q207 <- as.numeric(ordered(df2$Q207, levels=c('Less than high school degree', 'High school degree', 'Some college, no degree', 'Associate or technical degree', 'College degree', "Master's degree or other professional degree", 'Ph.D.')))
tab(df1$Q207, temp1_Q207)
tab(df2$Q207, temp2_Q207)
df1$Q207 <- temp1_Q207
df2$Q207 <- temp2_Q207

# HOUSEHOLD INCOME: Q209
var_names_dict[['Q209']] <- 'income'
tab(df1[,'Q209'])
tab(df2[,'Q209'])
unique(df1$Q209, na.rm=FALSE)
unique(df2$Q209, na.rm=FALSE)
all(unique(df1$Q209, na.rm=FALSE) %in% unique(df2$Q209, na.rm=FALSE))
temp1_Q209 <- as.numeric(ordered(df1$Q209, levels=c("Less than $20,000", "$20,000 - $34,999", '$35,000 - $49,999', "$50,000 - $74,999", "$75,000 - $99,999", "$100,000 - $149,999", "$149,000 - $199,999", "$200,000+")))
temp2_Q209 <- as.numeric(ordered(df2$Q209, levels=c("Less than $20,000", "$20,000 - $34,999", '$35,000 - $49,999', "$50,000 - $74,999", "$75,000 - $99,999", "$100,000 - $149,999", "$149,000 - $199,999", "$200,000+")))
tab(df1$Q209, temp1_Q209)
tab(df2$Q209, temp2_Q209)
df1$Q209 <- temp1_Q209
df2$Q209 <- temp2_Q209

# POLITICAL IDEOLOGY: Q211
var_names_dict[['Q211']] <- 'ideology'
tab(df1[,'Q211'])
tab(df2[,'Q211'])
all(unique(df1$Q211, na.rm=FALSE) %in% unique(df2$Q211, na.rm=FALSE))
temp1_Q211 <- as.numeric(ordered(df1$Q211, levels=c("Very liberal", "Liberal", "Moderate", "Conservative", "Very conservative")))
temp2_Q211 <- as.numeric(ordered(df2$Q211, levels=c("Very liberal", "Liberal", "Moderate", "Conservative", "Very conservative")))
tab(df1$Q211, temp1_Q211)
tab(df2$Q211, temp2_Q211)
df1$Q211 <- temp1_Q211
df2$Q211 <- temp2_Q211

# RELIGION: Q213
var_names_dict[['Q213']] <- 'religion'
tab(df1[,'Q213'])
tab(df2[,'Q213'])

# BASELINE TREATMENT ASSIGNMENT (BELIEFS STUDY)
# (creates indicator for whether or not individual was exposed to cultured
# meat)
var_names_dict[['cultured_expose']] <- 'cultured_expose'

# , 'DO.BR.FL_94', 'DO.BR.FL_16', 'DO.BR.FL_96', 'DO.BR.FL_18', 'DO.BR.FL_63'
df1$cultured_expose <- as.integer(str_detect(df1[,'DO.BR.FL_28'], 'cultured'))
df2$cultured_expose <- as.integer(str_detect(df2[,'DO.BR.FL_28'], 'cultured'))

tab(df1[,'DO.BR.FL_28'], df1$cultured_expose)
tab(df2[,'DO.BR.FL_28'], df2$cultured_expose)

# EXPORTS CLEANED DATA TO DISK.
df1_clean <- df1[, names(var_names_dict)]
df2_clean <- df2[, names(var_names_dict)]
colnames(df1_clean) <- as.vector(unlist(var_names_dict))
colnames(df2_clean) <- as.vector(unlist(var_names_dict))
head(df1_clean)
head(df2_clean)

df_clean <- rbind(df1_clean, df2_clean)

# if (any(df1_clean[,'completionCode'] %in% df2_clean[,'completionCode'])) stop('Same MTurker took baseline survey twice.')
stopifnot(nrow(df_clean) == nrow(df1_clean) + nrow(df2_clean))

write.csv(df_clean, '../data/cleaned/wave1_clean.csv')

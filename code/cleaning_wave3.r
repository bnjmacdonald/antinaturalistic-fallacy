# file: cleaning.r
# This file reads in and cleans the raw survey data for wave 2 of the
# anti-naturalistic fallacy survey experiment.

library(stringr)
library(dplyr)


source('utils.r')

len <- length
tab <- function(..., exclude=NULL) table(..., exclude=exclude)

# reads in followup data.
# df <- read_csv_qualtrics('../data/raw/naturalistic_fallacy_treatment.csv')
df <- read_csv_qualtrics('../data/raw/naturalistic_fallacy_followup.csv')
# all(colnames(df) == colnames(df))
dim(df)

# renames MTurkCode variable, since it actually represents completion code.
colnames(df)[which(colnames(df) == 'MTurkCode')] <- 'completionCode'
if ('MTurkCode' %in% colnames(df)) stop('MTurkCode not successfully renamed.')


# open-ended responses ("what thoughts went through your mind..."):
# Q34_1_TEXT, Q34_2_TEXT, Q34_3_TEXT

# Todo: looks like 100% of peple were shown the placebo article. Check this
# by seeing whether any of Q34 open-ended responses reference both a treatment
# article and a placebo article.

# CLEANING VARIABLES
var_names_dict <- list()  # dict holding variable name mappings.
var_names_dict[['V1']] <- 'respID'
var_names_dict[['completionCode']] <- 'completionCode'


# [expect_reduce] Q2: "Do you expect to reduce your consumption of
# conventional meat products over the next few weeks? (..."
var_names_dict[['Q2']] <- 'expect_reduce'
tab(df[,'Q2'])

# [expect_reduce_amy]: Q4: "How much do you expect to reduce your consumption
# of conventional meat products over the next few..."
var_names_dict[['Q4']] <- 'expect_reduce_amt'
tab(df[,'Q4'])

# [ease_eliminate] (Q111_1): "Now please rate the following according to how difficult you think each goal would be for you. Th...-Completely eliminating conventional meat products from your diet in the next year"
var_names_dict[['Q111_1']] <- 'ease_eliminate'
table(df$Q111_1, exclude=NULL)
unique(df$Q111_1, na.rm=FALSE)
temp_Q111_1 <- as.numeric(ordered(df$Q111_1, levels=c("Very difficult", "Difficult", "Somewhat difficult", "Neutral", "Somewhat easy", "Easy", "Very easy")))
tab(temp_Q111_1, df$Q111_1)
df$Q111_1 <- temp_Q111_1

# [ease_reduce] (Q111_2): "Now please rate the following according to how difficult you think each goal would be for you. Th...-Reducing your consumption of conventional meat products by 25% in the next year"
var_names_dict[['Q111_2']] <- 'ease_reduce'
table(df$Q111_2, exclude=NULL)
unique(df$Q111_2, na.rm=FALSE)
temp_Q111_2 <- as.numeric(ordered(df$Q111_2, levels=c("Very difficult", "Difficult", "Somewhat difficult", "Neutral", "Somewhat easy", "Easy", "Very easy")))
tab(temp_Q111_2, df$Q111_2)
df$Q111_2 <- temp_Q111_2


# [recall] Q8: "How well do you recall the "clean" meat product you were recently told about? A picture of the pr..."
var_names_dict[['Q8']] <- 'recall'
tab(df[,'Q8'])
unique(df[,'Q8'], na.rm=FALSE)
temp_Q8 <- as.numeric(ordered(df$Q8, levels=c("Not well at all", "Slightly well", "Moderately well", "Very well", "Extremely well")))
tab(df$Q8, temp_Q8)
df$Q8 <- temp_Q8


# [feel] Q12: "How do you feel about clean meat products?"
var_names_dict[['Q12']] <- 'feel'
tab(df$Q12)
unique(df$Q12, na.rm=FALSE)
temp_Q12 <- as.numeric(ordered(df$Q12, levels=c('Extremely negative', 'Moderately negative', 'Slightly negative', 'Neither positive nor negative', 'Slightly positive', 'Moderately positive', 'Extremely positive')))
tab(df$Q12, temp_Q12)
df$Q12 <- temp_Q12

# [interest_purchase] Q14: "How interested are you in purchasing clean meat products?"
var_names_dict[['Q14']] <- 'interest_purchase'
tab(df$Q14)
unique(df$Q14, na.rm=FALSE)
temp_Q14 <- as.numeric(ordered(df$Q14, levels=c("Not interested at all", "Slightly interested", "Moderately interested", "Very interested", "Extremely interested")))
tab(df$Q14, temp_Q14)
df$Q14 <- temp_Q14

# [would_eat] Q16: "Would you eat this product?"
var_names_dict[['Q16']] <- 'would_eat'
tab(df$Q16)
unique(df$Q16, na.rm=FALSE)
temp_Q16 <- as.numeric(ordered(df$Q16, levels=c('Definitely not', 'Probably not', 'Might or might not', 'Probably yes', 'Definitely yes')))
tab(df$Q16, temp_Q16)
df$Q16 <- temp_Q16

# [concern] (Q18_1-Q18_8): "Do you have any concerns about clean meat products? You may
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
cols_Q18 <- paste('Q18_', 1:8, sep='')
df <- clean_multichoice(df, cols_Q18)

df[,'num_concerns'] <- rowSums(df[,cols_Q18], na.rm=TRUE)

# adds column names to var_names_dict
colnames_Q18 <- paste('concern_', c('unhealthy', 'unsafe', 'taste', 'cost', 'unnatural', 'none', 'other', 'noreason'), sep='')
for (i in 1:len(cols_Q18)) {
    var_names_dict[[cols_Q18[i]]] <- colnames_Q18[i]
}
var_names_dict[['num_concerns']] <- 'num_concerns'



# [benefit] (Q22_1-Q22_8): "Do you perceive any expected benefits to this product? You may select multiple items."
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
cols_Q22 <- paste('Q22_', 1:8, sep='')
df <- clean_multichoice(df, cols_Q22)

df[,'num_benefits'] <- rowSums(df[,cols_Q22], na.rm=TRUE)

# adds column names to var_names_dict
colnames_Q22 <- paste('benefit_', c('healthier', 'safer', 'tastier', 'cheaper', 'envsust', 'none', 'other', 'suffering'), sep='')
for (i in 1:len(cols_Q22)) {
    var_names_dict[[cols_Q22[i]]] <- colnames_Q22[i]
}
var_names_dict[['num_benefits']] <- 'num_benefits'

# constructs cost-benefit variable.
df[,'cost_benefit'] <- df[,'num_benefits'] - df[,'num_concerns']
var_names_dict[['cost_benefit']] <- 'cost_benefit'
# summary(df[,'cost_benefit'])


# [perceived_reduce] Q52_4: 
# Please indicate the degree to which you agree or disagree with the following 
# statements.-More and more people in the U.S. are reducing their meat
# consumption.
var_names_dict[['Q52_4']] <- 'perceived_reduce'
tab(df$Q52_4)
unique(df$Q52_4, na.rm=FALSE)
temp_Q52_4 <- as.numeric(ordered(df$Q52_4, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
tab(df$Q52_4, temp_Q52_4)
df$Q52_4 <- temp_Q52_4


# [perceived_people_us_interest_purchase] Q52_5: 
# Please indicate the degree to which you agree or disagree with the following 
# statements.-Most people in the U.S. would be interested in eating "clean
# meat" products.
var_names_dict[['Q52_5']] <- 'perceived_people_us_interest_purchase'
tab(df$Q52_5)
unique(df$Q52_5, na.rm=FALSE)
temp_Q52_5 <- as.numeric(ordered(df$Q52_5, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
tab(df$Q52_5, temp_Q52_5)
df$Q52_5 <- temp_Q52_5


# [perceived_people_personal_interest_purchase] Q52_6:
# Please indicate the degree to which you agree or disagree with the following 
# statements.-Most people I know personally would be interested in eating
# "clean meat" products.
var_names_dict[['Q52_6']] <- 'perceived_people_personal_interest_purchase'
tab(df$Q52_6)
unique(df$Q52_6, na.rm=FALSE)
temp_Q52_6 <- as.numeric(ordered(df$Q52_6, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
tab(df$Q52_6, temp_Q52_6)
df$Q52_6 <- temp_Q52_6


# [perceived_people_personal_concerns] Q52_7:
# Please indicate the degree to which you agree or disagree with the following 
# statements.-Most people I know personally would have concerns about the
# health or safety of "clean meat" products.
var_names_dict[['Q52_7']] <- 'perceived_people_personal_concerns'
tab(df$Q52_7)
unique(df$Q52_7, na.rm=FALSE)
temp_Q52_7 <- as.numeric(ordered(df$Q52_7, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
tab(df$Q52_7, temp_Q52_7)
df$Q52_7 <- temp_Q52_7


# [perceived_people_us_concerns] Q52_8:
# Please indicate the degree to which you agree or disagree with the following 
# statements.-Most people in the U.S. would have concerns about the health or
# safety of "clean meat" products.
var_names_dict[['Q52_8']] <- 'perceived_people_us_concerns'
tab(df$Q52_8)
unique(df$Q52_8, na.rm=FALSE)
temp_Q52_8 <- as.numeric(ordered(df$Q52_8, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
tab(df$Q52_8, temp_Q52_8)
df$Q52_8 <- temp_Q52_8


# [veg_moral] Q54: "Some people say that eating vegetarian food is morally preferable to eating meat produced via fac..."
var_names_dict[['Q54']] <- 'veg_moral'
tab(df$Q54)
unique(df$Q54, na.rm=FALSE)
temp_Q54 <- as.numeric(ordered(df$Q54, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
tab(df$Q54, temp_Q54)
df$Q54 <- temp_Q54

# [sentient] Q56: "Do you think that farm animals are sentient (feel pleasure or pain)?"
var_names_dict[['Q56']] <- 'sentient'
tab(df$Q56)
unique(df$Q56, na.rm=FALSE)
temp_Q56 <- as.numeric(ordered(df$Q56, levels=c('Definitely not', 'Probably not', 'Might or might not', 'Probably yes', 'Definitely yes')))
tab(df$Q56, temp_Q56)
df$Q56 <- temp_Q56

# [harms] Q58: "How much do you think conventional meat production harms animals?"
var_names_dict[['Q58']] <- 'harms'
tab(df$Q58)
unique(df$Q58, na.rm=FALSE)
temp_Q58 <- as.numeric(ordered(df$Q58, levels=c('Not at all', 'A little', 'A moderate amount', 'A lot', 'A great deal')))
tab(df$Q58, temp_Q58)
df$Q58 <- temp_Q58

# [harms_concern] (Q60): "How concerned are you about this harm?"
var_names_dict[['Q60']] <- 'harms_concern'
tab(df$Q60)
unique(df$Q60, na.rm=FALSE)
temp_Q60 <- as.numeric(ordered(df$Q60, levels=c('Not at all', 'A little', 'A moderate amount', 'A lot', 'A great deal')))
tab(df$Q60, temp_Q60)
df$Q60 <- temp_Q60

# TODO: clean other attitudinal variables here (Q62-Q??, Q72).


# OUTCOME (Q68_1-Q68_7): "Please rate each of the following according to the extent that you think they are intelligent or..."
# Q68_1: cows
# Q68_2: pigs
# Q68_3: chicken
# Q68_4: fish
# Q68_5: humans
# Q68_6: dogs
# Q68_7: horses
intell_cols <- paste('Q68_', 1:7, sep='')
colnames_intell <- c("intell_cows", "intell_pigs", "intell_chicken", "intell_fish", "intell_humans", "intell_dogs", "intell_horses")
apply(df[,intell_cols], MARGIN=2, FUN=function(x) table(x, exclude=NULL))
for (i in 1:length(intell_cols)) {
    df[,intell_cols[i]] <- as.numeric(ordered(df[,intell_cols[i]], levels=c('Very unintelligent', 'Unintelligent', 'Somewhat unintelligent', 'Neither intelligent nor unintelligent', 'Somewhat intelligent', 'Intelligent', 'Very intelligent')))
}
for (i in 1:len(intell_cols)) {
    var_names_dict[[intell_cols[i]]] <- colnames_intell[i]
}

# OUTCOME (Q70_1-Q70_7): "Please rate each of the following according to the extent that you think they are capable of expe..."
# Q70_1: cows
# Q70_2: pigs
# Q70_3: chicken
# Q70_4: fish
# Q70_5: humans
# Q70_6: dogs
# Q70_7: horses
suff_cols <- paste('Q70_', 1:7, sep='')
colnames_suff <- c("suff_cows", "suff_pigs", "suff_chicken", "suff_fish", "suff_humans", "suff_dogs", "suff_horses")
apply(df[,suff_cols], MARGIN=2, FUN=function(x) table(x, exclude=NULL))
for (i in 1:length(suff_cols)) {
    df[,suff_cols[i]] <- as.numeric(ordered(df[,suff_cols[i]], levels=c('Completely incapable', 'Mostly incapable', 'Somewhat incapable', 'Neither capable nor incapable', 'Somewhat capable', 'Mostly capable', 'Highly capable')))
}
for (i in 1:len(suff_cols)) {
    var_names_dict[[suff_cols[i]]] <- colnames_suff[i]
}


# TODO: clean open-ended.
# Q20; Q24, Q74

# Discrete choice understanding (Q26): "You will now be asked some questions comparing clean meatballs, conventional meatballs, and veget..."
var_names_dict[['Q26']] <- 'dce_understood'
tab(df$Q26)
unique(df$Q26, na.rm=FALSE)
temp_Q26 <- as.numeric(ordered(df$Q26, levels=c('No', 'Somewhat', 'Yes')))
tab(df$Q26, temp_Q26)
df$Q26 <- temp_Q26

# Discrete choice (Q28 - Q50, every second Q): "Suppose you faced a choice between the following 3 products. Which would you buy?"
dce_questions <- paste0('Q', seq(28, 50, 2))

# str_replace_all('<div style="text-align: center;">1 lb. clean meatballs<br /> / $5</div>', '<[^>]*>', '')
dce <- lapply(seq_along(dce_questions), FUN=function(i) {
    d <- data.frame(str_split_fixed(str_replace_all(df[, dce_questions[i]], "<[^>]*>", ""), ' \\/ \\$', 2), stringsAsFactors=FALSE)
    colnames(d) <- paste0('dce_q', i, '_', c("product", "cost"))
    d[d == ""] <- NA
    d[,1] <- recode(d[,1], '1 lb. clean meatballs'='clean', '1 lb. conventional meatballs'='conventional', '1 lb. vegetarian meatballs'='vegetarian')
    d[,2] <- as.numeric(d[,2])
    if (any(rowSums(is.na(d)) == 1)) stop('Something went wrong with str_replace.')
    return(d)
})
dce <- do.call('cbind', dce)

stopifnot(nrow(dce) == nrow(df))
if (any(!rowSums(is.na(dce)) %in% c(12, 24))) stop('Problem with DCE cleaning.')
tab(rowSums(is.na(dce)))

for (i in 1:len(colnames(dce))) {
    var_names_dict[[colnames(dce)[i]]] <- colnames(dce)[i]
}

df <- cbind(df, dce)

# saves cleaned data to disk.
# print(names(var_names_dict))
# print(names(var_names_dict) %in% colnames(df2))
df_clean <- df[, names(var_names_dict)]
colnames(df_clean) <- as.vector(unlist(var_names_dict))
# head(df_clean)

write.csv(df_clean, '../data/cleaned/wave3_clean.csv')

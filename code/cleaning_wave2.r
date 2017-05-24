# file: cleaning.r
# This file reads in and cleans the raw survey data for wave 2 of the
# anti-naturalistic fallacy survey experiment.

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

df2 <- read_csv_qualtrics('../data/raw/naturalistic_fallacy_treatment.csv')
colnames(df2)
dim(df2)

# renames MTurkCode variable, since it actually represents completion code.
colnames(df2)[which(colnames(df2) == 'MTurkCode')] <- 'completionCode'
if ('MTurkCode' %in% colnames(df2)) stop('MTurkCode not successfully renamed.')


# treat_social: 1 if individual was exposed to "social information" treatment,
#   0 otherwise. "Q4" is the question "Here is a selection of comments that other MTurk 
#   workers have made about clean meat: ...".
# treat_article: 0 if placebo article, 1 if XXXX, 2 if XXXX, 3 if XXXX.

# constructs "social information" treatment variable
# rows where Q4 is not NA received the social treatment. Rows where Q4 is NA and
# Q6 is NA suggest that the subject didn't respond, so it is hard to tell
# whether they were exposed to the social information treatment.
# NOTE: may want to return to this decision later.
treat_social <- as.integer(!is.na(df2[, 'Q4']))
treat_social[is.na(df2$Q4) & is.na(df2$Q6)] <- NA
# table(df2[,'DO.BR.FL_64'], treat_social)

# constructs "article" treatment variable
# placebo (Q28):                                0
# explicit debunking arm (Q10):                 1
# similar to foods you already eat arm (Q16):   2
# social norms (Q22):                           3
treat_article <- rep(0, nrow(df2))
treat_article[!is.na(df2$Q10)] <- 1  # explicit debunking
treat_article[!is.na(df2$Q16)] <- 2  # similar to foods you already eat
treat_article[!is.na(df2$Q22)] <- 3  # social norms
table(treat_article)  # cells are roughly equal in size, as expected.
# table(df2[,'DO.BR.FL_67'], treat_article)

# open-ended responses ("what thoughts went through your mind..."):
# Q34_1_TEXT, Q34_2_TEXT, Q34_3_TEXT

# Todo: looks like 100% of peple were shown the placebo article. Check this
# by seeing whether any of Q34 open-ended responses reference both a treatment
# article and a placebo article.

# CLEANING VARIABLES
var_names_dict <- list()  # dict holding variable name mappings.
var_names_dict[['V1']] <- 'respID'
var_names_dict[['completionCode']] <- 'completionCode'

# CLEANING PRE-TREATMENT VARIABLES
# Q4: "Here is a selection of comments that other MTurk workers have made about clean meat: ..."
var_names_dict[['Q4']] <- 'negative_agree'
tab(df2[,'Q4'])
unique(df2[,'Q4'], na.rm=FALSE)
temp_Q4 <- as.numeric(ordered(df2$Q4, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
tab(df2$Q4, temp_Q4)
df2$Q4 <- temp_Q4

# Q2 and Q6: "How well do you recall the "clean" meat product you were recently told about? A picture of the pr..."
var_names_dict[['Q2_6']] <- 'recall'
df2[,'Q2_6'] <- NA
df2[,'Q2_6'][!is.na(df2[,'Q2'])] <- df2[,'Q2'][!is.na(df2[,'Q2'])]
df2[,'Q2_6'][!is.na(df2[,'Q6'])] <- df2[,'Q6'][!is.na(df2[,'Q6'])]
tab(df2[,'Q2_6'], df2[,'Q2'])
tab(df2[,'Q2_6'], df2[,'Q6'])
stopifnot(all(table(df2[,'Q2_6']) == table(df2[,'Q2']) + table(df2[,'Q6'])))

unique(df2[,'Q2_6'], na.rm=FALSE)
temp_Q2_6 <- as.numeric(ordered(df2$Q2_6, levels=c("Not well at all", "Slightly well", "Moderately well", "Very well", "Extremely well")))
tab(df2$Q2_6, temp_Q2_6)
df2$Q2_6 <- temp_Q2_6

# CLEANING OUTCOME VARIABLES

# OUTCOME (Q36): "How do you feel about clean meat products?"
var_names_dict[['Q36']] <- 'feel'
tab(df2$Q36)
unique(df2$Q36, na.rm=FALSE)
temp_Q36 <- as.numeric(ordered(df2$Q36, levels=c('Extremely negative', 'Moderately negative', 'Slightly negative', 'Neither positive nor negative', 'Slightly positive', 'Moderately positive', 'Extremely positive')))
tab(temp_Q36, df2$Q36)
df2$Q36 <- temp_Q36

# OUTCOME (Q38): "How interested are you in purchasing clean meat products?"
var_names_dict[['Q38']] <- 'interest_purchase'
tab(df2$Q38)
unique(df2$Q38, na.rm=FALSE)
temp_Q38 <- as.numeric(ordered(df2$Q38, levels=c("Not interested at all", "Slightly interested", "Moderately interested", "Very interested", "Extremely interested")))
tab(temp_Q38, df2$Q38)
df2$Q38 <- temp_Q38

# OUTCOME (Q40): "Would you eat this product?"
var_names_dict[['Q40']] <- 'would_eat'
tab(df2$Q40)
unique(df2$Q40, na.rm=FALSE)
temp_Q40 <- as.numeric(ordered(df2$Q40, levels=c('Definitely not', 'Probably not', 'Might or might not', 'Probably yes', 'Definitely yes')))
tab(temp_Q40, df2$Q40)
df2$Q40 <- temp_Q40



# OUTCOME (Q42_1-Q42_8): "Do you have any concerns about clean meat products? You may
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
cols_Q42 <- paste('Q42_', 1:8, sep='')
df2 <- clean_multichoice(df2, cols_Q42)

df2[,'num_concerns'] <- rowSums(df2[,cols_Q42], na.rm=TRUE)

# adds column names to var_names_dict
colnames_Q42 <- paste('concern_', c('unhealthy', 'unsafe', 'taste', 'cost', 'unnatural', 'none', 'other', 'noreason'), sep='')
for (i in 1:len(cols_Q42)) {
    var_names_dict[[cols_Q42[i]]] <- colnames_Q42[i]
}
var_names_dict[['num_concerns']] <- 'num_concerns'

# OUTCOME (Q44_1-Q44_8): "Do you perceive any expected benefits to this product? You may select multiple items."
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
cols_Q44 <- paste('Q44_', 1:8, sep='')
df2 <- clean_multichoice(df2, cols_Q44)

df2[,'num_benefits'] <- rowSums(df2[,cols_Q44], na.rm=TRUE)

# adds column names to var_names_dict
colnames_Q44 <- paste('benefit_', c('healthier', 'safer', 'tastier', 'cheaper', 'envsust', 'none', 'other', 'suffering'), sep='')
for (i in 1:len(cols_Q44)) {
    var_names_dict[[cols_Q44[i]]] <- colnames_Q44[i]
}
var_names_dict[['num_benefits']] <- 'num_benefits'

# constructs cost-benefit variable.
df2[,'cost_benefit'] <- df2[,'num_benefits'] - df2[,'num_concerns']
var_names_dict[['cost_benefit']] <- 'cost_benefit'
# summary(df1[,'cost_benefit'])


# OUTCOME (Q111_1): "Now please rate the following according to how difficult you think each goal would be for you. Th...-Completely eliminating conventional meat products from your diet in the next year"
var_names_dict[['Q111_1']] <- 'ease_eliminate'
table(df2$Q111_1, exclude=NULL)
unique(df2$Q111_1, na.rm=FALSE)
temp_Q111_1 <- as.numeric(ordered(df2$Q111_1, levels=c("Very difficult", "Difficult", "Somewhat difficult", "Neutral", "Somewhat easy", "Easy", "Very easy")))
tab(temp_Q111_1, df2$Q111_1)
df2$Q111_1 <- temp_Q111_1

# OUTCOME (Q111_2): "Now please rate the following according to how difficult you think each goal would be for you. Th...-Reducing your consumption of conventional meat products by 25% in the next year"
var_names_dict[['Q111_2']] <- 'ease_reduce'
table(df2$Q111_2, exclude=NULL)
unique(df2$Q111_2, na.rm=FALSE)
temp_Q111_2 <- as.numeric(ordered(df2$Q111_2, levels=c("Very difficult", "Difficult", "Somewhat difficult", "Neutral", "Somewhat easy", "Easy", "Very easy")))
tab(temp_Q111_2, df2$Q111_2)
df2$Q111_2 <- temp_Q111_2

# OUTCOME (Q48): "Some people say that eating vegetarian food is morally preferable to eating meat produced via fac..."
var_names_dict[['Q48']] <- 'veg_moral'
tab(df2$Q48)
unique(df2$Q48, na.rm=FALSE)
temp_Q48 <- as.numeric(ordered(df2$Q48, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
tab(temp_Q48, df2$Q48)
df2$Q48 <- temp_Q48

# OUTCOME (Q50): "Do you think that farm animals are sentient (feel pleasure or pain)?"
var_names_dict[['Q50']] <- 'sentient'
tab(df2$Q50)
unique(df2$Q50, na.rm=FALSE)
temp_Q50 <- as.numeric(ordered(df2$Q50, levels=c('Definitely not', 'Probably not', 'Might or might not', 'Probably yes', 'Definitely yes')))
tab(temp_Q50, df2$Q50)
df2$Q50 <- temp_Q50

# OUTCOME (Q52): "How much do you think conventional meat production, i.e. factory farming, harms animals?"
var_names_dict[['Q52']] <- 'harms'
tab(df2$Q52)
unique(df2$Q52, na.rm=FALSE)
temp_Q52 <- as.numeric(ordered(df2$Q52, levels=c('Not at all', 'A little', 'A moderate amount', 'A lot', 'A great deal')))
tab(temp_Q52, df2$Q52)
df2$Q52 <- temp_Q52

# OUTCOME (Q54): "How concerned are you about this harm?"
var_names_dict[['Q54']] <- 'harms_concern'
tab(df2$Q54)
unique(df2$Q54, na.rm=FALSE)
temp_Q54 <- as.numeric(ordered(df2$Q54, levels=c('Not at all', 'A little', 'A moderate amount', 'A lot', 'A great deal')))
tab(df2$Q54, temp_Q54)
df2$Q54 <- temp_Q54

# OUTCOME (Q66): ""More and more people in the U.S. are reducing their meat consumption." Do you agree or disagree..."
var_names_dict[['Q66']] <- 'perceived_reduce'
table(df2$Q66, exclude=NULL)
unique(df2$Q66, na.rm=FALSE)
temp_Q66 <- as.numeric(ordered(df2$Q66, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
tab(df2$Q66, temp_Q66)
df2$Q66 <- temp_Q66

# OUTCOME (Q100): "Would you like to receive more information about how to incorporate more vegetarian products in y..."
table(df2$Q100, exclude=NULL)

# creates indicator variable for 'yes' response.
df2[,'receive_veg_info_yes'] <- 0
df2[,'receive_veg_info_yes'][df2[,'Q100'] == 'Yes'] <- 1
df2[,'receive_veg_info_yes'][is.na(df2[,'Q100'])] <- NA
tab(df2[,'receive_veg_info_yes'], df2[,'Q100'])

# creates indicator variable for 'yes' or 'maybe' response.
df2[,'receive_veg_info_yesmaybe'] <- 0
df2[,'receive_veg_info_yesmaybe'][df2[,'Q100'] == 'Yes' | df2[,'Q100'] == 'Maybe'] <- 1
df2[,'receive_veg_info_yesmaybe'][is.na(df2[,'Q100'])] <- NA
tab(df2[,'receive_veg_info_yesmaybe'], df2[,'Q100'])

# adds these two new variables to the var_names_dict
var_names_dict[['receive_veg_info_yes']] <- 'receive_veg_info_yes'
var_names_dict[['receive_veg_info_yesmaybe']] <- 'receive_veg_info_yesmaybe'

# OUTCOME (Q102): "Would you like to be notified when clean meat is available in your area?"
table(df2$Q102, exclude=NULL)

# creates indicator variable for 'yes' response.
df2[,'notified_available_yes'] <- 0
df2[,'notified_available_yes'][df2[,'Q102'] == 'Yes'] <- 1
df2[,'notified_available_yes'][is.na(df2[,'Q102'])] <- NA
tab(df2[,'notified_available_yes'], df2[,'Q102'])

# creates indicator variable for 'yes' or 'maybe' response.
df2[,'notified_available_yesmaybe'] <- 0
df2[,'notified_available_yesmaybe'][df2[,'Q102'] == 'Yes' | df2[,'Q102'] == 'Maybe'] <- 1
df2[,'notified_available_yesmaybe'][is.na(df2[,'Q102'])] <- NA
tab(df2[,'notified_available_yesmaybe'], df2[,'Q102'])

# adds these two new variables to the var_names_dict
var_names_dict[['notified_available_yes']] <- 'notified_available_yes'
var_names_dict[['notified_available_yesmaybe']] <- 'notified_available_yesmaybe'

# OUTCOME (Q104_1_TEXT): "Please enter your e-mail address to receive this one-time notification. You may leave this field...-E-mail address."
var_names_dict[['entered_email']] <- 'entered_email'
df2[,'entered_email'] <- as.integer(!is.na(df2[,'Q104_1_TEXT']))


# Todo: clean Q56-Q64 and Q68.

# OUTCOME (Q62_1-Q62_7): "Please rate each of the following according to the extent that you think they are intelligent or..."
# Q62_1: cows
# Q62_2: pigs
# Q62_3: chicken
# Q62_4: fish
# Q62_5: humans
# Q62_6: dogs
# Q62_7: horses
intell_cols <- paste('Q62_', 1:7, sep='')
colnames_intell <- c("intell_cows", "intell_pigs", "intell_chicken", "intell_fish", "intell_humans", "intell_dogs", "intell_horses")
apply(df2[,intell_cols], MARGIN=2, FUN=function(x) table(x, exclude=NULL))
for (i in 1:length(intell_cols)) {
  df2[,intell_cols[i]] <- as.numeric(ordered(df2[,intell_cols[i]], levels=c('Very unintelligent', 'Unintelligent', 'Somewhat unintelligent', 'Neither intelligent nor unintelligent', 'Somewhat intelligent', 'Intelligent', 'Very intelligent')))
}
for (i in 1:len(intell_cols)) {
    var_names_dict[[intell_cols[i]]] <- colnames_intell[i]
}

# OUTCOME (Q64_1-Q64_7): "Please rate each of the following according to the extent that you think they are capable of expe..."
# Q64_1: cows
# Q64_2: pigs
# Q64_3: chicken
# Q64_4: fish
# Q64_5: humans
# Q64_6: dogs
# Q64_7: horses
suff_cols <- paste('Q64_', 1:7, sep='')
colnames_suff <- c("suff_cows", "suff_pigs", "suff_chicken", "suff_fish", "suff_humans", "suff_dogs", "suff_horses")
apply(df2[,suff_cols], MARGIN=2, FUN=function(x) table(x, exclude=NULL))
for (i in 1:length(suff_cols)) {
  df2[,suff_cols[i]] <- as.numeric(ordered(df2[,suff_cols[i]], levels=c('Completely incapable', 'Mostly incapable', 'Somewhat incapable', 'Neither capable nor incapable', 'Somewhat capable', 'Mostly capable', 'Highly capable')))
}
for (i in 1:len(suff_cols)) {
    var_names_dict[[suff_cols[i]]] <- colnames_suff[i]
}


# combines treatment variables with cleaned outcomes
# print(names(var_names_dict))
# print(names(var_names_dict) %in% colnames(df2))
df_clean <- df2[, names(var_names_dict)]
colnames(df_clean) <- as.vector(unlist(var_names_dict))
head(df_clean)

stopifnot(nrow(df_clean) == len(treat_social) & nrow(df_clean) == len(treat_article))
df_clean <- cbind(df_clean, treat_social, treat_article)
# table(df_clean$treat_social)
# table(df_clean$treat_article)
# table(df_clean$interest_purchase)

write.csv(df_clean, '../data/cleaned/wave2_clean.csv')
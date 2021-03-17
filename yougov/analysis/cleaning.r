# file: cleaning.r
# This file reads in and cleans the raw survey data for wave 1 of the
# anti-naturalistic fallacy survey experiment.
# 
# Note: the resulting `wave1_clean.csv` data file INCLUDES responses from the
# beliefs study replication in the "veg" and "no products" treatments. These
# individuals are dropped in `merging_waves.r`, since all individuals in the
# naturalistic fallacy study must have been exposed to cultured meat
# information at baseline.

# library(stringr)
library(tidyverse)
library(haven)


# library(readstata13)
source('yougov/analysis/utils.r')

len <- length
tab <- function(..., exclude=NULL) table(..., exclude=exclude)

# reads in baseline data.
# df1 <- read.dta13("../data/raw/batches_baseline.dta")

# reads in followup data.
# df2 <- read.dta13("../data/raw/followup.dta")
# df <- read.dta('../data/raw/followup.dta')

# reads in treatment wave data.

df <- zap_labels(read_sav('yougov/data/raw/AWEL0001_merged_output.sav'))
# colnames(df)
# dim(df)

# write_csv(df, "/Users/bmacwell/Downloads/teemp.csv")

# drops individuals from wave1 in "veg" or "no products" treatments (from
# beliefs study replication). i.e. only keeps individuals exposed to information
# about cultured meat.
df = df %>% filter(conditions == 1)

# if (any(df[df[,'cultured_expose']==0, id_var] %in% data_wave2[,id_var]) | any(data_wave1[data_wave1[,'cultured_expose']==0, id_var] %in% data_wave3[,id_var])) stop('One more more individuals in the "veg" or "no product" groups at baseline were re-surveyed.')

# CLEANING VARIABLES
var_names_dict <- list()  # dict holding variable name mappings.

# cleans news article treatment condition variable
col = "cleanmeatreat"
var_names_dict[[col]] <- 'treat_article'
df[col] = recode(pull(df, col), 
  `1`='embrace_unnatural',
  `2`='debunk_unnatural',
  `3`='descriptive_norm',
  `4`='control'
)

values = c('embrace_unnatural', 'debunk_unnatural', 'descriptive_norm', 'control')
stop_if_invalid_values(pull(df, col), values)


# cleans anti-clean meat priming treatment condition variable
col = "submeat"
var_names_dict[[col]] <- 'treat_social'
df[col] = recode(pull(df, col),
  `1`='exposed',
  `2`='not_exposed'
)

values = c('exposed', 'not_exposed')
stop_if_invalid_values(pull(df, col), values)


# DIET
# FFQ (Q5_1-Q5_16) - number of servings per week.
# Code    Label                       Mid-point (number of servings / week)
# -----   -----                       ------------------------------
# 1       Never                       0
# 2       Less than 1 time per week   1                    
# 3       1-6 times per week          3.5             
# 4       1-3 times per day           14            
# 5       4 or more times per day     28                  
n_ffq_cols = 16
servings_codes = c(0, 1, 3.5, 14, 28)
names(servings_codes) = c(1:5)
ffq_cols <- paste('Q1', letters[1:n_ffq_cols], sep='')

# sanity check
for (col in ffq_cols) {
  stop_if_invalid_values(pull(df, col), names(servings_codes))
}

df_ffq_temp = df %>%
  summarize(
    across(ffq_cols, function(x) {
      as.numeric(as.character(factor(x, levels=names(servings_codes), labels=servings_codes)))
    })
  )

# tab(pull(df,ffq_cols[5]), pull(df_ffq_temp, ffq_cols[5]))

df[,ffq_cols] = df_ffq_temp[,ffq_cols]


# FFQ VARIABLES:
colnames_ffq <- c(
  'FFQfreqDairy', 'FFQfreqChicken', 'FFQfreqTurkey',
  'FFQfreqFish', 'FFQfreqPork', 'FFQfreqBeef',
  'FFQfreqOther', 'FFQfreqEggs','FFQfreqFruit',
  'FFQfreqVegetables','FFQfreqNuts', 'FFQfreqBeans',
  'FFQfreqVegMeats', 'FFQfreqGrains', 'FFQfreqFats', 'FFQfreqSugar'
)
stopifnot(len(colnames_ffq) == n_ffq_cols)

# FFQ SUMMING ACROSS MEATS

# a. FFQ MEAT SUM TOTAL (multiplied by serving size)
colnames_meat <- c('FFQfreqChicken', 'FFQfreqTurkey', 'FFQfreqFish', 'FFQfreqPork', 'FFQfreqBeef', 'FFQfreqOther')
meat_cols <- ffq_cols[which(colnames_ffq %in% colnames_meat)]

df$ffq_total_sum_meat <- rowSums(df[,meat_cols], na.rm=TRUE)

# rows with NA are treated as 0 in rowSums. we don't want this, so any individual with more than 3 NAs gets an ffq_total_sum_meat of NA:
threshold <- 3
num_meat_na <- rowSums(is.na(df[,meat_cols]))
df$ffq_total_sum_meat[num_meat_na >= threshold] <- NA

# summary(df[c(ffq_cols, 'ffq_total_sum_meat')])

# adds ffq columns to var_names_dict
var_names_dict[['ffq_total_sum_meat']] <- 'ffq_total_sum_meat_wave1pre'
for (i in 1:len(ffq_cols)) {
    var_names_dict[[ffq_cols[i]]] <- paste0(colnames_ffq[i], '_wave1pre')
}



# OUTCOME: "Now please rate the following according to how
# difficult you think each goal would be for you. -Completely
# eliminating conventional meat products from your diet in the next
# year"
# "CON_Q7a",  # control
# "VEG_Q7a",  # veg substitute
# "MEAT_Q7a"  # clean meat
# wave 2:
# "Q3C_w2"  # control
# "Q5V_w2"  # veg substitute
# "Q6M_w2"  # clean meat
col_w1 = "MEAT_Q7a"
col_w2 = "Q6M_w2"

var_names_dict[[col_w1]] <- 'ease_eliminate_wave1'
var_names_dict[[col_w2]] <- 'ease_eliminate_wave2'

values = c(1:7, NA)
for (col in c(col_w1, col_w2)) {
  df[col] = reverse_ordinal(df[col])
  stop_if_invalid_values(pull(df, col), values)
}

# temp_ease_eliminate <- as.numeric(ordered(df[,col_w1], levels=c("Very difficult", "Difficult", "Somewhat difficult", "Neutral", "Somewhat easy", "Easy", "Very easy")))
# tab(temp_ease_eliminate, df[,col_w1])
# df[,col_w1] <- temp_ease_eliminate


# OUTCOME: "Now please rate the following according to how
# difficult you think each goal would be for you. -Reducing your
# consumption of conventional meat products by 25% in the next year"
col_w1 = "MEAT_Q7b"
col_w2 = "Q7M_w2"

var_names_dict[[col_w1]] <- 'ease_reduce_wave1'
var_names_dict[[col_w2]] <- 'ease_reduce_wave2'

values = c(1:7, NA)
for (col in c(col_w1, col_w2)) {
  df[col] = reverse_ordinal(df[col])
  stop_if_invalid_values(pull(df, col), values)
}


# INTEREST: MEAT_Q2
col_w1a = "MEAT_Q2"
col_w1b = "Cleanmeat_Q2"
col_w2 = "Q3M_w2"

var_names_dict[[col_w1a]] <- 'interest_purchase_wave1pre'
var_names_dict[[col_w1b]] <- 'interest_purchase_wave1'
var_names_dict[[col_w2]] <- 'interest_purchase_wave2'
# tab(pull(df, col_w1a), pull(df, col_w1b))

values = c(1:5, NA)
for (col in c(col_w1a, col_w1b, col_w2)) {
  df[col] = reverse_ordinal(df[col])
  stop_if_invalid_values(pull(df, col), values)
}


# WOULD EAT:
col_w1a = "MEAT_Q3"
col_w1b = "Cleanmeat_Q3"
col_w2 = "Q4M_w2"

var_names_dict[[col_w1a]] <- 'would_eat_wave1pre'
var_names_dict[[col_w1b]] <- 'would_eat_wave1'
var_names_dict[[col_w2]] <- 'would_eat_wave2'
# tab(pull(df, col_w1a), pull(df, col_w1b))

values = c(1:5, NA)
for (col in c(col_w1a, col_w1b, col_w2)) {
  df[col] = reverse_ordinal(df[col])
  stop_if_invalid_values(pull(df, col), values)
}


# EXPECT_REDUCE:
col_w1 = "MEAT_Q10"
col_w2 = "Q13M_w2"
var_names_dict[[col_w1]] <- 'expect_reduce_wave1'
var_names_dict[[col_w2]] <- 'expect_reduce_wave2'
values = c(0:10, NA)
stop_if_invalid_values(pull(df, col_w1), values)
stop_if_invalid_values(pull(df, col_w2), values)



# EXPECT_REDUCE_AMT:
col_w1 = "MEAT_Q11"
col_w2 = "Q14M_w2"
var_names_dict[[col_w1]] <- 'expect_reduce_amt_wave1'
var_names_dict[[col_w2]] <- 'expect_reduce_amt_wave2'
values = c(0:10, NA)
stop_if_invalid_values(pull(df, col_w1), values)
stop_if_invalid_values(pull(df, col_w2), values)



# OTHER VARIABLES


# [veg_moral]: "Some people say that eating vegetarian food
# is morally preferable to eating meat produced via fac..."
col_w1a = "Q8"
col_w1b = "MEAT_Q8"
var_names_dict[[col_w1a]] <- 'veg_moral_wave1pre'
var_names_dict[[col_w1b]] <- 'veg_moral_wave1'

values = c(1:7, NA)
df[col_w1a] = reverse_ordinal(df[col_w1a])
df[col_w1b] = reverse_ordinal(df[col_w1b])
stop_if_invalid_values(pull(df, col_w1a), values)
stop_if_invalid_values(pull(df, col_w1b), values)

# [harms]: "How much do you think conventional meat production harms animals?"
# NOTE: this question was asked twice in the same survey.
col_w1a = "Q9a"
col_w1b = "MEAT_Q9a"
col_w2 = "Q9M_w2"
var_names_dict[[col_w1a]] <- 'harms_wave1pre'
var_names_dict[[col_w1b]] <- 'harms_wave1'
var_names_dict[[col_w2]] <- 'harms_wave2'

values = c(1:5, NA)
df[col_w1a] = reverse_ordinal(df[col_w1a])
df[col_w1b] = reverse_ordinal(df[col_w1b])
df[col_w2] = reverse_ordinal(df[col_w2])
stop_if_invalid_values(pull(df, col_w1a), values)
stop_if_invalid_values(pull(df, col_w1b), values)
stop_if_invalid_values(pull(df, col_w2), values)

# [harms_concern]: "How concerned are you about this harm?"
# NOTE: this question was asked twice in the same survey.
col_w1a = "Q9b"
col_w1b = "MEAT_Q9b"
col_w2 = "Q10M_w2"
var_names_dict[[col_w1a]] <- 'harms_concern_wave1pre'
var_names_dict[[col_w1b]] <- 'harms_concern_wave1'
var_names_dict[[col_w2]] <- 'harms_concern_wave2'

values = c(1:5, NA)
df[col_w1a] = reverse_ordinal(df[col_w1a])
df[col_w1b] = reverse_ordinal(df[col_w1b])
df[col_w2] = reverse_ordinal(df[col_w2])
stop_if_invalid_values(pull(df, col_w1a), values)
stop_if_invalid_values(pull(df, col_w1b), values)
stop_if_invalid_values(pull(df, col_w2), values)




# DEMOGRAPHICS

# AGE: birthyr
col = 'birthyr'
year = 2020
df[col] = year - df[col] 
if (min(df[col]) < 18 | max(df[col]) > 100) stop("One or more respondents is <18 years of age or >100 yers of age.")
var_names_dict[[col]] <- 'age_wave1pre'

# GENDER: gender
col = 'gender'
var_names_dict[[col]] <- 'gender_wave1pre'
values = c(1:2)
stop_if_invalid_values(pull(df, col), values)
df[,col] = df[,col] - 1

# HIGHEST EDUCATION: educ
col = 'educ'
var_names_dict[[col]] <- 'educ_wave1pre'

values = c(1:6)
stop_if_invalid_values(pull(df, col), values)

# HOUSEHOLD INCOME: faminc_new
col = "faminc_new"
var_names_dict[[col]] <- 'income_wave1pre'
df[col] = na_if(df[col], 97)
values = c(1:16, NA)
stop_if_invalid_values(pull(df, col), values)


# POLITICAL IDEOLOGY: 
# presvote16post, pid3, pid7, ideo5
col = "ideo5"
var_names_dict[[col]] <- 'ideology_wave1pre'
df[col] = na_if(df[col], 6)
values = c(1:5, NA)
stop_if_invalid_values(pull(df, col), values)



# DISCRETE CHOICE / WILLINGNESS TO PAY QUESTIONS
colw1 = "wtp_wave1"
var_names_dict[[colw1]] <- colw1

df[colw1] = NULL
df[!is.na(df['Clean_WTP3a']) & df['Clean_WTP3a'] == 2, colw1] = 4  # Chooses 14 oz conventional meat over 42 oz clean
df[!is.na(df['Clean_WTP3a']) & df['Clean_WTP3a'] == 1, colw1] = 2.5  # Chooses 14 oz conventional meat over 28 oz clean but not 42 oz
df[!is.na(df['Clean_WTP3b']) & df['Clean_WTP3b'] == 2, colw1] = 1.79  # Chooses 14 oz conventional meat over 22 oz clean but not 28 oz
df[!is.na(df['Clean_WTP3b']) & df['Clean_WTP3b'] == 1, colw1] = 1.3  # Chooses 14 oz conventional meat over 14 oz clean but not 22 oz
df[!is.na(df['Clean_WTP3c']) & df['Clean_WTP3c'] == 2, colw1] = 1/1.25  # Chooses 21 oz conventional meat but not 14 oz over 14 oz clean
df[!is.na(df['Clean_WTP3c']) & df['Clean_WTP3c'] == 1, colw1] = 1/1.75  # Chooses 28 oz conventional meat but not 21 oz over 14 oz clean 
df[!is.na(df['Clean_WTP3d']) & df['Clean_WTP3d'] == 2, colw1] = 1/2.5  # Chooses 42 oz conventional meat but not 28 oz over 14 oz clean
df[!is.na(df['Clean_WTP3d']) & df['Clean_WTP3d'] == 1, colw1] = 1/4  # Does not choose 42 oz conventional meat over 14 oz clean


colw2 = "wtp_wave2"
var_names_dict[[colw2]] <- colw2

df[colw2] = NULL
df[!is.na(df['WTP3a2M_w2']) & df['WTP3a2M_w2'] == 2, colw2] = 4
df[!is.na(df['WTP3a2M_w2']) & df['WTP3a2M_w2'] == 1, colw2] = 2.5
df[!is.na(df['WTP3b2M_w2']) & df['WTP3b2M_w2'] == 2, colw2] = 1.79
df[!is.na(df['WTP3b2M_w2']) & df['WTP3b2M_w2'] == 1, colw2] = 1.3
df[!is.na(df['WTP3c2M_w2']) & df['WTP3c2M_w2'] == 2, colw2] = 1/1.25
df[!is.na(df['WTP3c2M_w2']) & df['WTP3c2M_w2'] == 1, colw2] = 1/1.75
df[!is.na(df['WTP3d2M_w2']) & df['WTP3d2M_w2'] == 2, colw2] = 1/2.5
df[!is.na(df['WTP3d2M_w2']) & df['WTP3d2M_w2'] == 1, colw2] = 1/4
 
# inverts the variables so that higher values indicate a preference for
# clean meat over conventional.
df[colw1] = 1 / df[colw1]
df[colw2] = 1 / df[colw2]
values = 1 / c(4, 2.5, 1.79, 1.3, 1/1.25, 1/1.75, 1/2.5, 1/4, NA)
stop_if_invalid_values(pull(df, colw1), values)
stop_if_invalid_values(pull(df, colw2), values)


# subsets data to clean variables
df_clean <- df[, names(var_names_dict)]
colnames(df_clean) <- as.vector(unlist(var_names_dict))
# head(df_clean)


# creates "change" variables for outcome variables that were asked pre-treatment
# in wave 1 and post-treatment in wave 1.
vars11 = c(
  'would_eat', 
  'interest_purchase',
  'veg_moral',
  'harms', 
  'harms_concern'
)

df_clean = create_chg_vars(df_clean, vars11, suffix1='_wave1pre', suffix2='_wave1')


# creates "change" variables for outcome variables that were asked pre-treatment
# in wave 1 and post-treatment in wave 2.
vars12 = c(
  'would_eat', 
  'interest_purchase',
  'harms', 
  'harms_concern'
)

df_clean = create_chg_vars(df_clean, vars12, suffix1='_wave1pre', suffix2='_wave2')


# EXPORTS CLEANED DATA TO DISK.

write_csv(df_clean, 'yougov/data/cleaned/all_waves_clean.csv')

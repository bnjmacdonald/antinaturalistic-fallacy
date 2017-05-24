# file: merging_waves.r
# merges the wave 1 and wave 2 cleaned data.

source('utils.r')

len <- length
tab <- function(..., exclude=NULL) table(..., exclude=exclude)

code_var = 'completionCode'
id_var = 'MTurkCode'

# reads in cleaned survey data.
data_wave1 = read.csv('../data/cleaned/wave1_clean.csv', stringsAsFactor=FALSE, na.strings=c("NA", ""))
data_wave2 = read.csv('../data/cleaned/wave2_clean.csv', stringsAsFactor=FALSE, na.strings=c("NA", ""))
colnames(data_wave1)
colnames(data_wave2)
dim(data_wave1)
dim(data_wave2)

# reads in MTurk response data.
input_dir1 = "../data/raw/mturk_completion_baseline"
fnames1 = paste(input_dir1, list.files(input_dir1, pattern="*.csv"), sep='/')
df_list1 = lapply(fnames1, FUN=read.csv, stringsAsFactor=FALSE, na.strings=c("NA", ""))
mturk_wave1 = do.call(rbind, df_list1)

input_dir2 = "../data/raw/mturk_completion_treatment"
fnames2 = paste(input_dir2, list.files(input_dir2, pattern="*.csv"), sep='/')
df_list2 = lapply(fnames2, FUN=read.csv, stringsAsFactor=FALSE, na.strings=c("NA", ""))
mturk_wave2 = do.call(rbind, df_list2)

colnames(mturk_wave1)
colnames(mturk_wave2)
dim(mturk_wave1)
dim(mturk_wave2)

# next, in order to merge the two survey waves, we need the MTurk ID of
# each survey response in data_wave1 and data_wave2. But these dataframes only
# store completion codes. The mapping from completion code to MTurk ID is
# stored in mturk_wave1 and mturk_wave2. The code below retrieves the MTurk
# IDs from these dataframes.

# replaces completion code with MTurk ID in survey wave data.
mturk_codes_wave1 = vector()
for (i in 1:nrow(data_wave1)) {
    completion_code = data_wave1[i,code_var]
    idx = which(mturk_wave1[,'Answer.surveycode'] == as.character(completion_code))
    mturk_code = NA
    if (len(idx) == 1) {
        mturk_code = mturk_wave1[idx, 'WorkerId']
    } else if (len(idx) == 0) {
        print(paste('No mturk id found for completion code ', completion_code, sep=''))
    } else {
        print('WARNING: more than one matching completion code found for this observation.')
    }
    mturk_codes_wave1[i] = mturk_code
}

mturk_codes_wave2 = vector()
for (i in 1:nrow(data_wave2)) {
    completion_code = data_wave2[i,code_var]
    idx = which(mturk_wave2[,'Answer.surveycode'] == as.character(completion_code))
    mturk_code = NA
    if (len(idx) == 1) {
        mturk_code = mturk_wave2[idx, 'WorkerId']
    } else if (len(idx) == 0) {
        print(paste('No mturk id found for completion code ', completion_code, sep=''))
    } else {
        print('WARNING: more than one matching completion code found for this observation.')
    }
    mturk_codes_wave2[i] = mturk_code
}

data_wave1[,id_var] = mturk_codes_wave1
data_wave2[,id_var] = mturk_codes_wave2

tab(is.na(data_wave1[,id_var]))
tab(is.na(data_wave2[,id_var]))
tab(is.na(data_wave1[,code_var]))
tab(is.na(data_wave2[,code_var]))

if (len(data_wave1[,id_var][duplicated(data_wave1[,id_var]) & !is.na(data_wave1[,id_var])])) stop('There are duplicated MTurk IDs')
if (len(data_wave2[,id_var][duplicated(data_wave2[,id_var]) & !is.na(data_wave2[,id_var])])) stop('There are duplicated MTurk IDs')

mturk_id_matches = data_wave2[,id_var] %in% data_wave1[,id_var]
if (any(!mturk_id_matches)) print(paste('WARNING: ',sum(!mturk_id_matches), ' Mturk IDs in wave 2 but not wave 1', sep=''))

head(data_wave1[,id_var], 20)
head(data_wave2[,id_var], 20)

# drops observations where id_var is NA.
data_wave1 = data_wave1[!is.na(data_wave1[,id_var]),]
data_wave2 = data_wave2[!is.na(data_wave2[,id_var]),]

# drops variables.
drop_vars = c(code_var, 'respID', 'X')
data_wave1 = data_wave1[, -which(colnames(data_wave1) %in% drop_vars)]
data_wave2 = data_wave2[, -which(colnames(data_wave2) %in% drop_vars)]

# drops individuals from wave1 in "veg" or "no products" treatments (from
# beliefs study replication).

if (any(data_wave1[data_wave1[,'cultured_expose']==0, id_var] %in% data_wave2[,id_var])) stop('One ore more individuals in the "veg" or "no product" groups at baseline were re-surveyed.')
data_wave1 = data_wave1[data_wave1[,'cultured_expose']==1,]

# merges wave1 and wave2 data.
data_merged = merge(data_wave1, data_wave2, by=id_var, all=TRUE, suffixes=c('_1', '_2'))
colnames(data_merged)
dim(data_merged)

# head(data_merged)

# computes survey recontact rate
nrow(data_wave1)
nrow(data_wave2)
nrow(data_wave2) / nrow(data_wave1)

# creates "change" variables
colnames(data_merged)
colnames_concern = paste('concern_', c('unhealthy', 'unsafe', 'taste', 'cost', 'unnatural', 'none', 'other', 'noreason'), sep='')
colnames_benefit = paste('benefit_', c('healthier', 'safer', 'tastier', 'cheaper', 'envsust', 'none', 'other', 'suffering'), sep='')
vars = c('feel', 'would_eat', 'interest_purchase', 'ease_reduce', 'ease_eliminate', 'entered_email', 'notified_available_yes', 'notified_available_yesmaybe', 'receive_veg_info_yes', 'receive_veg_info_yesmaybe', 'num_concerns', 'num_benefits', 'cost_benefit', colnames_concern, colnames_benefit)

data_merged = create_chg_vars(data_merged, vars, suffix1='_1', suffix2='_2')

# saves merged data to disk.
write.csv(data_merged, '../data/cleaned/all_waves_clean.csv')



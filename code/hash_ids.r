# file: hash_ids.r
# description: this file creates hash ids based on MTurk IDs.
# Note: this implementation is really slow for larger datasets since it is
# not vectorized.

library(digest)

out_path <- '../data/cleaned_shareable'
set.seed(566192)

# hashes IDs appearing in "mturkid_cols" in a dataframe ("df")
hash_ids <- function(df, mturkid_cols, mturkids_hashed) {
    for (col in mturkid_cols) {
        if (col %in% colnames(df)) {
            for (j in 1:nrow(df)) {
                if (!is.na(df[j,col]) & df[j,col] != '') {
                    df[j,col] <- mturkids_hashed[df[j,col]]
                }
            }
        }
    }
    return(df)
}


# reads in cleaned and merged data.
df <- read.csv('../data/cleaned/all_waves_clean.csv', stringsAsFactors=FALSE)
df_dce <- read.csv('../data/cleaned/dce_clean_long.csv', stringsAsFactors=FALSE)
# dim(df)

# MTurk ID columns
mturkid_cols <- c('MTurkCode')

# creates vector of unique MTurk IDs.
mturkids <- c()
for (col in mturkid_cols) {
    if (col %in% colnames(df)) {
        mturkids <- c(mturkids, df[,col])
    }
}
mturkids_uniq <- unique(mturkids)
mturkids_uniq <- mturkids_uniq[!is.na(mturkids_uniq) & mturkids_uniq != '']
length(mturkids_uniq)

# creates a hashed ID for each unique MTurk ID
mturkids_hashed <- sapply(mturkids_uniq, digest, algo='md5', serialize=FALSE)
if (length(mturkids_hashed) != length(unique(mturkids_hashed))) stop('One or more hashed IDs is not unique')

# replaces MTurk ID with hashed ID
df_hashed <- hash_ids(df, mturkid_cols, mturkids_hashed)
df_dce_hashed <- hash_ids(df_dce, mturkid_cols, mturkids_hashed)

# checks that all MTurk IDs are now hashed.
mturkids_new <- c()
for (col in mturkid_cols) {
    if (col %in% colnames(df_hashed)) {
        mturkids_new <- c(mturkids_new, df_hashed[,col])
    }
}
mturkids_new_uniq <- unique(mturkids_new)
if (length(intersect(mturkids_new_uniq, mturkids_uniq)) > 0) stop('Not all MTurk IDs were successfully hashed.')

mturkids_new <- c()
for (col in mturkid_cols) {
    if (col %in% colnames(df_dce_hashed)) {
        mturkids_new_dce <- c(mturkids_new, df_dce_hashed[,col])
    }
}
mturkids_new_uniq <- unique(mturkids_new)
if (length(intersect(mturkids_new_uniq, mturkids_uniq)) > 0) stop('Not all MTurk IDs were successfully hashed.')

# removes columns from waves 1-4
cols_to_drop <- c('X', 'X...V1', 'V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'LocationLatitude', 'LocationLongitude', 'LocationAccuracy', 'recordedDate', 'X_recordId', '_recordId', 'ipAddress', 'QID74_1_44edb4750c3e43f892386f9dTopic0', 'status')
data_shareable <- df_hashed[, !colnames(df_hashed) %in% cols_to_drop]
data_shareable_dce <- df_dce_hashed[, !colnames(df_dce_hashed) %in% cols_to_drop]

# removes columns from mturk wave 1 batch data
# saves revised dataframes to file.
write.csv(data_shareable, paste(out_path, "all_waves_clean_shareable.csv", sep='/'), row.names=FALSE)
write.csv(data_shareable_dce, paste(out_path, "dce_clean_long_shareable.csv", sep='/'), row.names=FALSE)


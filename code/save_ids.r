# file: save_ids.r
# description: this file saves MTurk IDs from surveys to csv.

library(stringr)

out_path <- '../data/mturkids'

# reads in cleaned and merged data.
df <- read.csv('../data/cleaned/all_waves_clean.csv', stringsAsFactors=FALSE)
# dim(df)

# MTurk ID columns
mturkid_col <- c('MTurkCode')

# extracts all wave 1 column names.
wave2_cols <- str_detect(colnames(df), '[A-z]_2$')

# finds all rows where at least one wave 2 column is not NA.
in_wave2 <- rowSums(!is.na(df[,wave2_cols]), na.rm=FALSE) > 0
stopifnot(sum(in_wave2) > 2000)

# subsets to unique MTurk IDs.
ids <- df[in_wave2 ,mturkid_col]
mturkids_uniq <- unique(ids)
mturkids_uniq <- mturkids_uniq[!is.na(mturkids_uniq) & mturkids_uniq != '']
print(length(mturkids_uniq))

# saves mturk ids to csv
write.csv(mturkids_uniq, paste(out_path, "mturkids_wave2.csv", sep='/'), row.names=FALSE)

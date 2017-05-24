# clean_multichoice
# function for cleaning a multichoice outcome (i.e. "select any that apply").
# Arguments:
#   df: dataframe.
#   cols: vector of strings representing column names.
clean_multichoice <- function(df, cols) {
    temp_counts <- apply(df[,cols], MARGIN=2, FUN=function(x) sum(!is.na(x)))
    for (col in cols) {
        df[,col][!is.na(df[,col])] <- 1
        df[,col][is.na(df[,col])] <- 0
        df[,col] <- as.integer(df[,col])
    }
    temp_counts2 <- apply(df[,cols], MARGIN=2, FUN=function(x) sum(x))
    stopifnot(all(temp_counts == temp_counts2))
    return(df)
}

# read_csv_qualtrics:
# function for reading in a csv exported from Qualtrics.
# Arguments:
#   fname: string representing file name.
read_csv_qualtrics <- function(fname) {
    temp_lines <- readLines(fname)
    skip_second <- temp_lines[-2]
    df <- read.csv(textConnection(skip_second), header=TRUE, stringsAsFactors=FALSE, na.strings=c("NA", ""))
    return(df)
}

# function: create_chg_vars.
# description: creates "change" variables based on list of
#   vars in df. It is assumed that vars are named the same 
#   in wave 1 as in wave 3 (or 4).
create_chg_vars <- function(df, vars, suffix1, suffix2) {
    for (i in 1:length(vars)) {
        var_name_1 <- paste(vars[i],suffix1, sep='')
        var_name_2 <- paste(vars[i],suffix2, sep='')
        var_name_chg <- paste(vars[i],'_chg',suffix1, suffix2, sep='')
        df[,var_name_chg] <- df[,var_name_2] - df[,var_name_1]
    }
    return(df)
}

sem <- function(x, na.rm=FALSE) {
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    return(sd(x) / sqrt(length(x)))
}

conf_interval <- function(mean, n, se, q) {
    alpha <- (1 - q/100)/2
    lower_ci <- mean - qt(1 - alpha, df=n-1)*se
    upper_ci <- mean + qt(1 - alpha, df=n-1)*se
    result <- data.frame(lower_ci, upper_ci)
    # q_str <- str_split(as.character(q), '\\.')[[1]][2]
    lower_ci_name <- paste('lower_ci',q,sep='')
    upper_cir_name <- paste('upper_ci',q,sep='')
    colnames(result) <- c(lower_ci_name, upper_cir_name)
    return(result)
}

to_title <- function(x, suffix=NULL) {
    if (!is.null(suffix)) {
        x <- str_replace(x, paste(suffix, '$', sep=''), '')
    }
    return(str_to_title(str_replace_all(x, '_+', ' ')))
}

# from: http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization
get_legend<-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

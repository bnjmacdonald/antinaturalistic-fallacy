


# function: stop_if_invalid_values 
# Raises an error if a vector `v`
# contains an unexpected/invalid value. Value values are provided in
# `values`.
# Arguments:
#   v: vector or vector-like array.
#   values: vector or vecctor-like array of acceptable values.
stop_if_invalid_values <- function(v, values) {
  if (!all(v %in% values)) {
    stop(paste("Unexpected values in ", 
      col, 
      ". Expected values ", 
      paste(values, collapse=" "),
      ", but found ",
      paste(unique(v), collapse=" "),
      sep=''
    ))
  }
}


# function: reverse_ordinal
# description: reverses an ordinal scale
reverse_ordinal <- function(v) {
  max_val = max(v, na.rm=TRUE)
  min_val = min(v, na.rm=TRUE)
  v2 = max_val - (v - min_val)
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

get_pvalue <- function(statistic, df, twosided=TRUE) {
    pvalue = pt(-abs(statistic), df=df)
    if (twosided) {
      pvalue = pvalue * 2
    }
    return(pvalue)
}

ttest_coef_diff <- function(beta1, beta2, se1, se2, df, twosided=TRUE) {
    diff = beta1-beta2
    test_stat = diff / sqrt( se1^2 + se2^2 )
    pvalue = get_pvalue(test_stat, df=df, twosided=twosided)
    result = c(diff, pvalue)
    names(result) = c('difference', 'pvalue')
    return(result)
}

to_title <- function(x, suffix=NULL) {
    if (!is.null(suffix)) {
        x <- str_replace(x, paste(suffix, '$', sep=''), '')
    }
    return(str_trim(str_to_title(str_replace_all(x, '_+', ' '))))
}

# from: http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization
get_legend<-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

# function: pvalue_to_str
# description: converts pvalue to a string typical seen in papers.
# Example: convert pvalue of 0.0041 to "p < 0.01"
# 
# tests:
# pvalue_to_str(0.0) == 'p < 0.001'
# pvalue_to_str(0.043) == 'p < 0.05'
# pvalue_to_str(0.01) == 'p < 0.01'
# pvalue_to_str(0.0101) == 'p < 0.05'
# pvalue_to_str(0.049999) == 'p < 0.05'
# pvalue_to_str(0.05) == 'p < 0.05'
# pvalue_to_str(0.14) == 'p > 0.1'
# pvalue_to_str(0.56) == 'p > 0.1'
# is.na(pvalue_to_str(1.1))
pvalue_to_str <- function(pvalue) {
    breaks = c(0.0, 0.001, 0.01, 0.05, 0.1, 1.0)
    labels = paste0('p < ', breaks[2:6])
    labels[5] = 'p > 0.1'
    str = cut(pvalue, breaks=breaks, labels=labels, include.lowest=TRUE)
    return(as.character(str))
}

# function: clean_treatment_names
# description: single-purpose helper function that removes 'treat_article' and
# 'treat_social' (or other treatment_names) from the beginning of strings in
# the "term" column output of get_coef_tables().
clean_treatment_names <- function(data, treatment_names) {
    data = lapply(data, FUN=function(x) {
        x$term = str_replace(x$term, paste0('^', treatment_names, collapse='|^'), '')
        return(x)
    })
    return(data)
}

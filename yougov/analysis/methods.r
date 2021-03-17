# --------------------------------- #
# function: get_formula.
# ---------------------------
# Returns a formula object.
# Arguments:
#   y: string representing name of outcome.
#   x: vector of strings representing independent variable(s).
#   interact: boolean representing whether to interact x variables. Default:
#       FALSE.
# --------------------------------- #
get_formula <- function(y, x, interact=FALSE) {
    c <- '+'
    if (interact) {
        c <- '*'
    }
    right <- paste(x, collapse=c)
    form <- formula(paste(y, right, sep='~'))
    return(form)
}

# --------------------------------- #
# function: get_coef_tables
# -------------------------
# Estimates ATE for each outcome in outcomes and returns a table of treatment
# effects.
# --------------------------------- #
get_coef_tables <- function(df, outcomes, treatments) {
    results <- list()
    for (outcome in outcomes) {
        form <- get_formula(outcome, treatments)
        lm0 <- lm(form, data=df)
        result_table <- tidy(lm0)
        result_table[,-1] <- round(result_table[,-1], 4)
        result_table[,'n'] <- nobs(lm0)
        results[[outcome]] <- result_table
    }
    return(results)
}

# --------------------------------- #
# function: get_coef_tables_interact
# -------------------------
# Estimates ATE for each outcome in outcomes and returns a table of treatment
# effects.
# --------------------------------- #
get_coef_tables_interact <- function(df, outcome, treatment, moderator) {
    form <- get_formula(outcome, paste(treatment,'*',moderator, sep=''))
    lm0 <- lm(form, data=df)
    lm0_tidy <- tidy(lm0)
    lm0_vcov <- vcov(lm0)

    result_table <- data.frame(matrix(nrow=len(levels(df[,treatment])[-1])*len(levels(df[,moderator])), ncol=4))
    colnames(result_table) <- c('treatment', 'moderator', 'estimate', 'se')
    i <- 1
    for (treat_val in levels(df[,treatment])[-1]) {
        this_treatment <- paste0(treatment, treat_val)
        treat_coef <- lm0_tidy$estimate[lm0_tidy$term==this_treatment]
        treat_se <- lm0_tidy$std.error[lm0_tidy$term==this_treatment]
        result_table[i,] <- c(treat_val, levels(df[,moderator])[1], treat_coef, treat_se)
        i <- i + 1
        for (mod_val in levels(df[,moderator])[-1]) {
            this_moderator <- paste0(moderator,mod_val)
            this_interaction <- paste0(this_treatment, ':', this_moderator)
            coef <- treat_coef + 1*lm0_tidy$estimate[lm0_tidy$term==this_interaction]
            se <- sqrt(lm0_vcov[this_treatment, this_treatment] + lm0_vcov[this_moderator, this_moderator] + 2 * lm0_vcov[this_treatment, this_moderator])
            result_table[i,] <- c(treat_val, mod_val, coef, se)
            i <- i + 1
        }
    }
    result_table[,'estimate'] <- round(as.numeric(result_table[,'estimate']), 4)
    result_table[,'se'] <- round(as.numeric(result_table[,'se']), 4)
    result_table[,'n'] <- nobs(lm0)
    ci <- conf_interval(result_table$estimate, result_table$n, result_table$se, q=95)
    result_table[,colnames(ci)[1]] <- ci[,1]
    result_table[,colnames(ci)[2]] <- ci[,2]
    ci <- conf_interval(result_table$estimate, result_table$n, result_table$se, q=90)
    result_table[,colnames(ci)[1]] <- ci[,1]
    result_table[,colnames(ci)[2]] <- ci[,2]
    return(result_table)
}

# --------------------------------- #
# Function: get_group_means
# ---------------------------
# Returns a dataframe of group means. All variables in 'x' must be discrete.
# 
# Useful for plotting means by subgroup and treatment condition.
# ---------------------------
get_group_means <- function(df, groupby, y) {
    df_temp_long <- df[,c(groupby, y)] %>%
        gather_(key='group_name', value='group', groupby) %>%
        rename_(y=y)
    df_temp_long <- df_temp_long[complete.cases(df_temp_long),]
    data_to_plot <- df_temp_long %>% 
        group_by(group_name, group) %>%
        summarise(
            mean=mean(y, na.rm=TRUE),
            sem=sem(y, na.rm=TRUE),
            n=sum(!is.na(y))
        )
    ci <- conf_interval(data_to_plot$mean, data_to_plot$n, data_to_plot$sem, q=95)
    data_to_plot[,colnames(ci)[1]] <- ci[,1]
    data_to_plot[,colnames(ci)[2]] <- ci[,2]
    ci <- conf_interval(data_to_plot$mean, data_to_plot$n, data_to_plot$sem, q=90)
    data_to_plot[,colnames(ci)[1]] <- ci[,1]
    data_to_plot[,colnames(ci)[2]] <- ci[,2]
    data_to_plot$group <- factor(data_to_plot$group)
    # data_to_plot$group <- ifelse(data_to_plot[,'moderator'] == 1, 'high moderator', 'low moderator')
    return(data_to_plot)
}

# --------------------------------- #
# Function: get_subgroup_means
# ---------------------------
# Returns a dataframe of subgroup means.
# 
# Useful for plotting means by subgroup and treatment condition.
# ---------------------------
get_subgroup_means <- function(df, treatment, outcomes, moderator) {
    df_temp_long <- df[,c(treatment, moderator, outcomes)] %>% gather_(key='outcome', value='value', outcomes)
    df_temp_long <- df_temp_long %>% rename_(treatment=treatment, moderator=moderator)
    data_to_plot <- df_temp_long %>% 
        group_by(moderator, treatment, outcome) %>%
        summarise(
            mean=mean(value, na.rm=TRUE),
            sem=sem(value, na.rm=TRUE),
            n=sum(!is.na(value))
        )
    data_to_plot <- data_to_plot[!is.na(data_to_plot[,'moderator']) & !is.na(data_to_plot[,'treatment']),]
    # data_to_plot$group <- NA

    ci <- conf_interval(data_to_plot$mean, data_to_plot$n, data_to_plot$sem, q=95)
    data_to_plot[,colnames(ci)[1]] <- ci[,1]
    data_to_plot[,colnames(ci)[2]] <- ci[,2]
    ci <- conf_interval(data_to_plot$mean, data_to_plot$n, data_to_plot$sem, q=90)
    data_to_plot[,colnames(ci)[1]] <- ci[,1]
    data_to_plot[,colnames(ci)[2]] <- ci[,2]
    # data_to_plot$group <- ifelse(data_to_plot[,'moderator'] == 1, 'high moderator', 'low moderator')
    return(data_to_plot)
}
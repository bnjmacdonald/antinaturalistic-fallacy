# file: main_analysis.r
# This file contains the main specifications examining average treatment
# effects in the anti-naturalistic fallacy study.

library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(gridExtra)
library(stringr)
# library(lazyeval)

len <- length
nunique <- function(x) {return(len(unique(x)))}
tab <- function(..., exclude=NULL) table(..., exclude=exclude)

source('utils.r')

# constants for plots
fig_output_dir <- '../output/figures'
dpi <- 200
height <- 12
# height_per_subplot <- 2.5
width <- 16
theme_set(theme_bw())
tableau10_cb <- c('#006ba4', '#ff800e', '#ababab', '#595959', '#5f9ed1', '#c85200', '#898989', '#a3c8ec', '#ffbc79', '#cfcfcf')
# grid::grid.raster(tableau10_cb, int=F)
scale_colour_discrete <- function(...) scale_colour_manual(..., values=tableau10_cb)

plot_theme <- theme(legend.position="bottom", legend.direction="horizontal", legend.box="horizontal", text=element_text(size=11), legend.text=element_text(size=13), plot.margin = unit(c(0.1,0.1,0.1,0.1), 'cm'), axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title=element_text(hjust = 0.5, face="bold"))

facetplot_theme <- theme(legend.position="bottom", legend.direction="horizontal", legend.box="horizontal", text=element_text(size=11), legend.text=element_text(size=13), plot.margin = unit(c(0.1,0.1,0.1,0.1), 'cm'), panel.background=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title=element_text(hjust = 0.5, face="bold"))


# --------------------------------- #
# Function: gg_coefplot
# ---------------------------
# This function creates a coefficient plot showing treatment effects.
#
# --------------------------------- #
gg_coefplot <- function(data_to_plot, ...) {
    p <- ggplot(data_to_plot, aes(x=estimate, y=outcome, ...)) + 
        geom_vline(xintercept=0, linetype='dashed', color='gray30') +
        geom_errorbarh(aes(xmin=lower_ci90, xmax=upper_ci90), height=0, size=1.0, position=position_dodge(width=0.2)) +
        geom_errorbarh(aes(xmin=lower_ci95, xmax=upper_ci95), height=0, size=0.4, position=position_dodge(width=0.2)) +
        labs(x='ATE', y=NULL) +
        geom_point(size=3, position=position_dodge(width=0.2)) + 
        # geom_line(size=1, position=position_dodge(width=0.1)) + 
        # facet_wrap(~color, ncol=3, scales='fixed') + 
        guides(color=guide_legend(title=NULL, reverse=FALSE)) +
        plot_theme
        # coord_flip()
    return(p)
}

# --------------------------------- #
# Function: gg_facet_coefplot
# ---------------------------
# This function creates a facetted plot showing treatment effects.
# 
# Arguments:
#   coef_table: table of coefficients and standard errors
#   ncol: integer representing number of columns in facet_wrap.
#   intercept: boolean indicating whether intercept should be plotted.
#   coord_flip: boolean indicating whether coordinates should be flipped.
#   hline_zero: boolean indicating whether a horizontal line at zero should be
#       placed. Default: TRUE.
#   scales: scales argument to pass to facet_wrap(). Default: 'free_y'.
# --------------------------------- #
gg_facet_coefplot <- function(coef_table, ncol=NULL, intercept=FALSE, coordFlip=FALSE, hlineZero=TRUE, scales='free_y') {
    if (class(coef_table) == 'list') {
        # adds 'outcome' columns to each coef_table in the list.
        coef_table <- lapply(seq_along(coef_table), FUN=function(x, n, i) { 
                x[[i]][,'outcome'] <- n[[i]]
                x[[i]]
            },
            x=coef_table, n=names(coef_table)
        )
        # rbinds all coef_tables in the list into a single long table.
        coef_table <- do.call('rbind', coef_table)
    }
    # removes intercepts.
    if (!intercept) {
        coef_table <- coef_table[coef_table[,'term'] != '(Intercept)',]
    }
    # computes upper and lower CIs
    if (!'n' %in% colnames(coef_table)) {
        coef_table[,'n'] <- 200  # Note: kludge! Deal with this properly later.
    }
    coef_table$lowerCI <- coef_table[,'estimate'] - qt(0.975, df=coef_table[,'n']-1)*coef_table[,'std.error']
    coef_table$upperCI <- coef_table[,'estimate'] + qt(0.975, df=coef_table[,'n']-1)*coef_table[,'std.error']
    # modifies outcome labels for plotting.
    # uniq_outcomes <- unique(coef_table[,'outcome'])
    # for (i in 1:length(uniq_outcomes)) {
    #     coef_table[,'outcome'][coef_table[,'outcome'] == uniq_outcomes[i]] <- paste("(",i,") ", uniq_outcomes[i], sep="")
    # }
    # creates the facet grid.
    ylabel <- 'Average treatment effect (ATE)'
    p <- ggplot(coef_table, aes_string(x='term', y='estimate', colour='term', fill=NULL))
    if (nrow(coef_table) > 1) {
        p <- p + facet_wrap(~outcome, ncol=ncol, scales=scales)
    }
    # adds horizontal line.
    if (hlineZero) {
        p <- p + geom_hline(yintercept=0, linetype=2, color='gray60')
    }
    # plots the data.
    p <- p + geom_point(size=3) +
            geom_errorbar(aes(ymin=lowerCI, ymax=upperCI, colour=term), width=0, size=1) +
            labs(x=NULL, y=ylabel) +
            scale_colour_discrete(name="") +
            guides(fill=guide_legend(title=NULL, reverse = TRUE)) +
            # coord_cartesian(ylim = c(5,10)) + 
            theme(
                legend.position="none",
                legend.direction="horizontal",
                legend.box="horizontal",
                text=element_text(size=11),
                plot.margin=unit(c(0.1,0.1,0.1,0.1), 'cm'),
                axis.text.x=element_text(angle = 45, hjust=1)
            )
    # flips the coordinates.
    if (coordFlip) {
        p <- p + coord_flip()
    }
    return(p)
}

# --------------------------------- #
# Function: gg_facet_meanplot
# ---------------------------
# This function creates a facetted plot showing group means.
# Arguments:
#   y: variable to use as y-axis.
#   x: variable to use as x-axis.
#   facet_col: variable to facet by.
#   ncol: integer representing number of columns in facet_wrap.
#   coord_flip: boolean indicating whether coordinates should be flipped.
#   scales: scales argument to pass to facet_wrap(). Default: 'free_y'.
# --------------------------------- #
gg_facet_meanplot <- function(y, x, facet_col, ncol=NULL, coordFlip=FALSE, scales='free_y') {
    # data <- df[,c(interest_vars, treatments)]
    data <- data.frame(y=y, x=x, facet_col=facet_col)
    data <- data[complete.cases(data),]
    p <- ggplot(data=data, aes(y=y, x=x, group=facet_col)) + 
        stat_summary(fun.data=mean_se, width=0, geom="errorbar") + 
        stat_summary(fun.y='mean', geom="line") +
        stat_summary(fun.y='mean', geom="point", size=1.5) +
        facet_wrap(~facet_col, ncol=ncol, scales=scales)
    # flips the coordinates.
    if (coordFlip) {
        p <- p + coord_flip()
    }
    return(p)
}

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
# Function: gg_subgroup_coefplot
# ---------------------------
# Plots subgroup treatment effects.
# 
# Useful for plotting treatment effects by subgroup.
# NOTE: x=y and y=x because coordinates are later flipped. This is necessary
# for position_dodge.
# ---------------------------
gg_subgroup_coefplot <- function(data_to_plot, x='estimate', y='moderator', color='treatment') {
    p <- ggplot(data_to_plot, aes_string(x=y, y=x, color=color)) + 
        geom_hline(yintercept=0, linetype='dashed', color='gray30') +
        geom_errorbar(aes(ymin=lower_ci90, ymax=upper_ci90), width=0, size=0.5, position=position_dodge(width=0.7)) +
        geom_errorbar(aes(ymin=lower_ci95, ymax=upper_ci95), width=0, size=0.2, position=position_dodge(width=0.7)) +
        geom_point(size=2, position=position_dodge(width=0.7)) + 
        # geom_line(size=1, position=position_dodge(width=0.1)) + 
        # facet_wrap(~color, ncol=3, scales='fixed') + 
        guides(color=guide_legend(title=NULL, reverse=FALSE)) +
        plot_theme + 
        coord_flip()
    return(p)
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
# Function: gg_subgroup_meanplot
# ---------------------------
# Plots group means.
# 
# ---------------------------
gg_group_meanplot <- function(means_to_plot) {
    p <- ggplot(means_to_plot, aes(x=group_name, y=mean, color=group, group=group)) + 
        geom_errorbar(aes(ymin=lower_ci90, ymax=upper_ci90), width=0, size=1.0, position=position_dodge(width=0.25)) +
        geom_errorbar(aes(ymin=lower_ci95, ymax=upper_ci95), width=0, size=0.4, position=position_dodge(width=0.25)) +
        geom_point(size=3, position=position_dodge(width=0.25)) + 
        # geom_line(size=1, position=position_dodge(width=0.1)) + 
        # facet_wrap(~color, ncol=3, scales='fixed') + 
        guides(color=guide_legend(title=NULL, reverse=TRUE)) +
        coord_flip() + 
        # facet_wrap(~)
        plot_theme
    return(p)
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

# --------------------------------- #
# Function: gg_subgroup_meanplot
# ---------------------------
# Plots subgroup means.
# 
# ---------------------------
gg_subgroup_meanplot <- function(data_to_plot) {
    p <- ggplot(data=data_to_plot, aes(y=mean, x=treatment, color=moderator, group=moderator)) + 
        geom_errorbar(aes(ymin=lower_ci90, ymax=upper_ci90), width=0, size=1.0, position=position_dodge(width=0.1)) +
        geom_errorbar(aes(ymin=lower_ci95, ymax=upper_ci95), width=0, size=0.4, position=position_dodge(width=0.1)) +
        labs(x='Treatment Appeal', y='y') +
        geom_point(size=3, position=position_dodge(width=0.1)) + 
        geom_line(size=1, position=position_dodge(width=0.1)) + 
        # facet_wrap(~color, ncol=3, scales='fixed') + 
        guides(color=guide_legend(title=NULL, reverse = TRUE)) +
        plot_theme
    return(p)
}


# reads in cleaned data.
df <- read.csv('../data/cleaned/all_waves_clean.csv', stringsAsFactors=FALSE)
str(df)
dim(df)
colnames(df)

# counts number of missing values for each variable.
sort(apply(df, MARGIN=2, FUN=function(x) sum(is.na(x))), decreasing=TRUE)

# converts treatments to factor variables.
df$treat_social <- factor(df$treat_social)
df$treat_article <- factor(df$treat_article)

# colnames(df)
treatments <- c('treat_article', 'treat_social')

# converts gender to factor
# df$gender <- as.numeric(factor(df$gender, levels=c('Male', 'Female')))

# outcome variable sets
interest_vars <- c('feel', 'interest_purchase', 'would_eat', 'notified_available_yes', 'entered_email')  # 'notified_available_yesmaybe'

concern_vars <- c('concern_unhealthy', 'concern_unsafe', 'concern_taste', 'concern_cost', 'concern_unnatural')  #  'concern_none', 'concern_other', 'concern_noreason', 'cost_benefit', 'num_concerns'

benefit_vars <- c('benefit_healthier', 'benefit_safer', 'benefit_tastier', 'benefit_cheaper', 'benefit_envsust', 'benefit_suffering')  # 'benefit_none', 'benefit_other', 'num_benefits'

ease_vars <- c('ease_eliminate', 'ease_reduce')

meat_attitude_vars <- c('veg_moral', 'sentient', 'harms', 'harms_concern', 'perceived_reduce', 'receive_veg_info_yes')  # 'receive_veg_info_yesmaybe'

# covariate variable sets
demographics <- c('age', 'educ', 'income', 'gender') # religion, 

other_vars <- c('ideology', 'expect_reduce', 'expect_reduce_amt')

# animal intelligence
# animal suffering

# EXPERIMENTAL ARM SIZES
# ----------------------

arm_sizes <- df %>% group_by(treat_article, treat_social) %>% 
    summarise(n=sum(!is.na(treat_article)))
arm_sizes <- arm_sizes[complete.cases(arm_sizes),]
print(arm_sizes)

# PRETREATMENT BALANCE
# --------------------

colnames(df)
# Todo: add more variables to balance checks.
pretreat_vars <- c('age', 'income', 'ideology', 'educ', colnames(df)[str_detect(colnames(df), '_1$')])
balance_tables <- get_coef_tables(df, pretreat_vars, treatments)

# ATTITUDES TOWARDS CLEAN MEAT
# ----------------------------

# subsets to control observations only.
df_control <- df[df$treat_article == 0 & df$treat_social == 0 & !is.na(df$treat_article) & !is.na(df$treat_social), ]
dim(df_control)

outcomes_chg <- paste0(interest_vars, '_chg_1_2')
outcomes_1 <- paste0(interest_vars, '_1')
outcomes_2 <- paste0(interest_vars, '_2')

means <- df_control %>% 
    gather_(key='outcome', value='value', c(outcomes_1, outcomes_2, outcomes_chg)) %>%
    group_by(outcome) %>%
    summarise(mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE), min=min(value, na.rm=TRUE), q25=quantile(value, 0.25, na.rm=TRUE), q50=quantile(value, 0.25, na.rm=TRUE), q75=quantile(value, 0.25, na.rm=TRUE), max=max(value, na.rm=TRUE))
print(means)

prop.table(table(df_control[,'would_eat_2']>3))
prop.table(table(df_control[,'interest_purchase_2']>3))

melted <- df_control %>% gather_(key='outcome', value='value', c(outcomes_2))
melted$outcome <- to_title(str_replace(melted$outcome, '_2$', ''))
p <- ggplot(melted, aes(x=factor(value), group=outcome)) + 
    geom_bar(fill=tableau10_cb[7]) +
    facet_wrap(~outcome, scales='free_x', ncol=5) +
    plot_theme + 
    labs(x=NULL, y='Count') +
    theme(strip.background = element_blank())
fname <- 'interest.png'
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width+5, height=height-5, units="cm", dpi=dpi)


# APPEAL EFFECTS
# --------------

suffix <- '_chg_1_2'

# EFFECTS ON INTEREST IN CLEAN MEAT

outcomes <- paste0(interest_vars, suffix)
coef_tables <- get_coef_tables(df, outcomes, treatments)

appeals <- paste0(treatments[1], 1:3)
effects_appeal1 <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeals[1],]))
effects_appeal2 <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeals[2],]))
effects_appeal3 <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeals[3],]))

effects_appeal1$outcome <- to_title(str_replace(rownames(effects_appeal1), paste0(suffix, '$'), ''))
effects_appeal2$outcome <- to_title(str_replace(rownames(effects_appeal2), paste0(suffix, '$'), ''))
effects_appeal3$outcome <- to_title(str_replace(rownames(effects_appeal3), paste0(suffix, '$'), ''))

effects_appeals <- rbind(effects_appeal1, effects_appeal2, effects_appeal3)
effects_appeals$term <- factor(effects_appeals$term)
rownames(effects_appeals) <- NULL

ci <- conf_interval(effects_appeals$estimate, effects_appeals$n, effects_appeals$std.error, q=95)
effects_appeals[,colnames(ci)[1]] <- ci[,1]
effects_appeals[,colnames(ci)[2]] <- ci[,2]
ci <- conf_interval(effects_appeals$estimate, effects_appeals$n, effects_appeals$std.error, q=90)
effects_appeals[,colnames(ci)[1]] <- ci[,1]
effects_appeals[,colnames(ci)[2]] <- ci[,2]

p1 <- gg_coefplot(effects_appeals, color=effects_appeals$term) + 
    facet_wrap(~term, scales='fixed') + 
    theme(strip.background = element_blank()) + 
    guides(color=FALSE)
    # labs(title='Appeal 1') +
fname <- 'effects_appeals_interest.png'
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p1, width=width, height=height-7, units="cm", dpi=dpi)



# EFFECTS ON CONCERNS
outcomes <- paste0(concern_vars, suffix)
coef_tables <- get_coef_tables(df, outcomes, treatments)

appeals <- paste0(treatments[1], 1:3)
effects_appeal1 <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeals[1],]))
effects_appeal2 <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeals[2],]))
effects_appeal3 <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeals[3],]))

effects_appeal1$outcome <- to_title(str_replace(rownames(effects_appeal1), paste0(suffix, '$'), ''))
effects_appeal2$outcome <- to_title(str_replace(rownames(effects_appeal2), paste0(suffix, '$'), ''))
effects_appeal3$outcome <- to_title(str_replace(rownames(effects_appeal3), paste0(suffix, '$'), ''))

effects_appeals <- rbind(effects_appeal1, effects_appeal2, effects_appeal3)
effects_appeals$outcome <- str_replace(effects_appeals$outcome, '^Concern ', '')
effects_appeals$term <- factor(effects_appeals$term)
rownames(effects_appeals) <- NULL

ci <- conf_interval(effects_appeals$estimate, effects_appeals$n, effects_appeals$std.error, q=95)
effects_appeals[,colnames(ci)[1]] <- ci[,1]
effects_appeals[,colnames(ci)[2]] <- ci[,2]
ci <- conf_interval(effects_appeals$estimate, effects_appeals$n, effects_appeals$std.error, q=90)
effects_appeals[,colnames(ci)[1]] <- ci[,1]
effects_appeals[,colnames(ci)[2]] <- ci[,2]

p1 <- gg_coefplot(effects_appeals, color=effects_appeals$term) + 
    facet_wrap(~term, scales='fixed') + 
    theme(strip.background = element_blank()) + 
    guides(color=FALSE)
fname <- 'effects_appeals_concerns.png'
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p1, width=width, height=height-7, units="cm", dpi=dpi)


# EFFECTS ON BENEFITS
outcomes <- paste0(benefit_vars, suffix)
coef_tables <- get_coef_tables(df, outcomes, treatments)

appeals <- paste0(treatments[1], 1:3)
effects_appeal1 <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeals[1],]))
effects_appeal2 <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeals[2],]))
effects_appeal3 <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeals[3],]))

effects_appeal1$outcome <- to_title(str_replace(rownames(effects_appeal1), paste0(suffix, '$'), ''))
effects_appeal2$outcome <- to_title(str_replace(rownames(effects_appeal2), paste0(suffix, '$'), ''))
effects_appeal3$outcome <- to_title(str_replace(rownames(effects_appeal3), paste0(suffix, '$'), ''))

effects_appeals <- rbind(effects_appeal1, effects_appeal2, effects_appeal3)
effects_appeals$outcome <- str_replace(effects_appeals$outcome, '^Benefit ', '')
effects_appeals$term <- factor(effects_appeals$term)
rownames(effects_appeals) <- NULL

ci <- conf_interval(effects_appeals$estimate, effects_appeals$n, effects_appeals$std.error, q=95)
effects_appeals[,colnames(ci)[1]] <- ci[,1]
effects_appeals[,colnames(ci)[2]] <- ci[,2]
ci <- conf_interval(effects_appeals$estimate, effects_appeals$n, effects_appeals$std.error, q=90)
effects_appeals[,colnames(ci)[1]] <- ci[,1]
effects_appeals[,colnames(ci)[2]] <- ci[,2]

p1 <- gg_coefplot(effects_appeals, color=effects_appeals$term) + 
    facet_wrap(~term, scales='fixed') + 
    theme(strip.background = element_blank()) + 
    guides(color=FALSE)
fname <- 'effects_appeals_benefits.png'
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p1, width=width, height=height-7, units="cm", dpi=dpi)


# EFFECTS ON EASE REDUCE
outcomes <- paste0(ease_vars, suffix)
coef_tables <- get_coef_tables(df, outcomes, treatments)

appeals <- paste0(treatments[1], 1:3)
effects_appeal1 <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeals[1],]))
effects_appeal2 <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeals[2],]))
effects_appeal3 <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeals[3],]))

effects_appeal1$outcome <- to_title(str_replace(rownames(effects_appeal1), paste0(suffix, '$'), ''))
effects_appeal2$outcome <- to_title(str_replace(rownames(effects_appeal2), paste0(suffix, '$'), ''))
effects_appeal3$outcome <- to_title(str_replace(rownames(effects_appeal3), paste0(suffix, '$'), ''))

effects_appeals <- rbind(effects_appeal1, effects_appeal2, effects_appeal3)
effects_appeals$term <- factor(effects_appeals$term)
rownames(effects_appeals) <- NULL

ci <- conf_interval(effects_appeals$estimate, effects_appeals$n, effects_appeals$std.error, q=95)
effects_appeals[,colnames(ci)[1] ] <- ci[,1]
effects_appeals[,colnames(ci)[2] ] <- ci[,2]
ci <- conf_interval(effects_appeals$estimate, effects_appeals$n, effects_appeals$std.error, q=90)
effects_appeals[, colnames(ci)[1] ] <- ci[,1]
effects_appeals[, colnames(ci)[2] ] <- ci[,2]

p1 <- gg_coefplot(effects_appeals, color=effects_appeals$term) + 
    facet_wrap(~term, scales='fixed') + 
    theme(strip.background=element_blank()) + 
    guides(color=FALSE)
fname <- 'effects_appeals_ease.png'
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p1, width=width, height=height-9, units="cm", dpi=dpi)


# EFFECTS ON MEAT ATTITUDES
outcomes <- sapply(meat_attitude_vars, FUN=function(x) ifelse(x %in% colnames(df), x, paste(x, suffix, sep='')))
names(outcomes) <- NULL
coef_tables <- get_coef_tables(df, outcomes, treatments)

appeals <- paste0(treatments[1], 1:3)
effects_appeal1 <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeals[1],]))
effects_appeal2 <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeals[2],]))
effects_appeal3 <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeals[3],]))

effects_appeal1$outcome <- to_title(str_replace(rownames(effects_appeal1), paste0(suffix, '$'), ''))
effects_appeal2$outcome <- to_title(str_replace(rownames(effects_appeal2), paste0(suffix, '$'), ''))
effects_appeal3$outcome <- to_title(str_replace(rownames(effects_appeal3), paste0(suffix, '$'), ''))

effects_appeals <- rbind(effects_appeal1, effects_appeal2, effects_appeal3)
effects_appeals$term <- factor(effects_appeals$term)
rownames(effects_appeals) <- NULL

ci <- conf_interval(effects_appeals$estimate, effects_appeals$n, effects_appeals$std.error, q=95)
effects_appeals[,colnames(ci)[1] ] <- ci[,1]
effects_appeals[,colnames(ci)[2] ] <- ci[,2]
ci <- conf_interval(effects_appeals$estimate, effects_appeals$n, effects_appeals$std.error, q=90)
effects_appeals[,colnames(ci)[1] ] <- ci[,1]
effects_appeals[,colnames(ci)[2] ] <- ci[,2]

p1 <- gg_coefplot(effects_appeals, color=effects_appeals$term) + 
    facet_wrap(~term, scales='fixed') + 
    theme(strip.background = element_blank()) + 
    guides(color=FALSE)
fname <- 'effects_appeals_meat_attitudes.png'
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p1, width=width, height=height-5, units="cm", dpi=dpi)




# EFFECTS ON INTEREST IN CLEAN MEAT
# NOTE: these plots put all appeals on the same plot, rather than facetting by
# treatment.

# interest_vars_suffix <- paste(interest_vars, suffix, sep='')
# fname <- 'interest.png'
# coef_tables <- get_coef_tables(df, interest_vars_suffix, treatments)
# p <- gg_facet_coefplot(coef_tables, ncol=3, intercept=FALSE, coordFlip=FALSE, hlineZero=TRUE, scales='free_y')
# ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)


# SOCIAL INFORMATION EFFECTS
# --------------------------
outcomes <- sapply(c(interest_vars, concern_vars, ease_vars, meat_attitude_vars), FUN=function(x) ifelse(x %in% colnames(df), x, paste(x, suffix, sep='')))
names(outcomes) <- NULL
coef_tables <- get_coef_tables(df, outcomes, treatments)

appeals <- paste0(treatments[1], 1:3)
effects_appeal1 <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeals[1],]))
effects_appeal2 <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeals[2],]))
effects_appeal3 <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeals[3],]))
effects_appeal1$outcome <- str_replace(rownames(effects_appeal1), paste0(suffix, '$'), '')
effects_appeal2$outcome <- str_replace(rownames(effects_appeal2), paste0(suffix, '$'), '')
effects_appeal3$outcome <- str_replace(rownames(effects_appeal3), paste0(suffix, '$'), '')
effects_appeals <- rbind(effects_appeal1, effects_appeal2, effects_appeal3)
effects_appeals$term <- factor(effects_appeals$term)
rownames(effects_appeals) <- NULL

social <- paste0(treatments[2], 1)
effects_social <- do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==social[1],]))
effects_social$outcome <- str_replace(rownames(effects_social), paste0(suffix, '$'), '')
effects_social$term <- factor(effects_social$term)
rownames(effects_social) <- NULL

ci <- conf_interval(effects_social$estimate, effects_social$n, effects_social$std.error, q=95)
effects_social[,colnames(ci)[1] ] <- ci[,1]
effects_social[,colnames(ci)[2] ] <- ci[,2]
ci <- conf_interval(effects_social$estimate, effects_social$n, effects_social$std.error, q=90)
effects_social[,colnames(ci)[1] ] <- ci[,1]
effects_social[,colnames(ci)[2] ] <- ci[,2]

effects_social$outcome_group <- to_title(ifelse(effects_social$outcome %in% interest_vars, 'interest',
    ifelse(effects_social$outcome %in% concern_vars, 'concern',
    ifelse(effects_social$outcome %in% ease_vars, 'ease', 'other'))))
effects_appeals$outcome_group <- to_title(ifelse(effects_appeals$outcome %in% interest_vars, 'interest',
    ifelse(effects_appeals$outcome %in% concern_vars, 'concern',
    ifelse(effects_appeals$outcome %in% ease_vars, 'ease', 'other'))))

effects_social$outcome <- to_title(effects_social$outcome)
effects_appeals$outcome <- to_title(effects_appeals$outcome)

p1 <- gg_coefplot(effects_social) + 
    facet_wrap(~outcome_group, scales='free_y', ncol=1) + 
    geom_point(aes(x=estimate, y=outcome, color=term), data=effects_appeals, size=1.0) +
    theme(strip.background = element_blank()) +
    guides(color=guide_legend(title=NULL, size=2))
fname <- 'effects_social.png'
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p1, width=width, height=height+4, units="cm", dpi=dpi)


means <- df %>% group_by(treat_social, treat_article) %>%
    summarise(mean=mean(would_eat_chg_1_2, na.rm=TRUE))
print(means)


# INTERACTIONS BETWEEN TREATMENTS
# -------------------------------
suffix <- '_2'
outcomes <- paste(c('feel', 'would_eat', 'interest_purchase', 'notified_available_yes', 'entered_email'), suffix, sep='')

moderator <- 'treat_social'

coef_tables <- lapply(outcomes, FUN=function(y) get_coef_tables_interact(df, y, treatments[1], moderator))
names(coef_tables) <- outcomes
coef_table <- do.call(rbind, coef_tables)
coef_table$y <- to_title(str_replace(rownames(coef_table), paste0(suffix, '.[0-9]+$'), ''))
coef_table$treatment <- paste0('Appeal ', coef_table$treatment)
rownames(coef_table) <- NULL
coef_table$moderator <- ifelse(coef_table$moderator==0, 'Not exposed', 'Exposed')

p <- gg_subgroup_coefplot(coef_table, x='estimate', y='y', color='treatment')
p <- p + facet_wrap(~moderator, ncol=1) + theme(strip.background = element_blank()) + labs(title='', y='ATE', x=NULL)
fname <- paste('subgroup_effects_negative.png', sep='')
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height+5, units="cm", dpi=dpi)

# OLD
x <- df[,'treat_social']
facet_col <- df[,'treat_article']

# treatment interactions: interest_purchase
y <- df[,'interest_purchase']
p <- gg_facet_meanplot(y, x, facet_col, ncol=nunique(facet_col), coordFlip=FALSE, scales='fixed')
fname <- 'interact_interest_purchase.png'
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)

# treatment interactions: would_eat
y <- df[,'would_eat']
p <- gg_facet_meanplot(y, x, facet_col, ncol=nunique(facet_col), coordFlip=FALSE, scales='fixed')
fname <- 'interact_would_eat.png'
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)

# treatment interactions: concern_unnatural
y <- df[,'concern_unnatural']
p <- gg_facet_meanplot(y, x, facet_col, ncol=nunique(facet_col), coordFlip=FALSE, scales='fixed')
fname <- 'interact_concern_unnatural.png'
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)

# treatment interactions: concern_unsafe
y <- df[,'concern_unsafe']
p <- gg_facet_meanplot(y, x, facet_col, ncol=nunique(facet_col), coordFlip=FALSE, scales='fixed')
fname <- 'interact_concern_unsafe.png'
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)

# mean plots
outcomes <- c('would_eat', 'interest_purchase')
group_vars <- c('treat_social', 'treat_article')
df_temp_long <- df[,c(group_vars, outcomes)] %>% gather_(key='outcome', value='value', outcomes)
data_to_plot <- df_temp_long %>% 
    group_by(treat_article, treat_social, outcome) %>%
    summarise(
        mean=mean(value, na.rm=TRUE),
        sem=sem(value, na.rm=TRUE),
        n=sum(!is.na(value)))
data_to_plot <- data_to_plot[!is.na(data_to_plot$treat_social),]
# data_to_plot$group <- NA

ci <- conf_interval(data_to_plot$mean, data_to_plot$n, data_to_plot$sem, q=95)
data_to_plot[,colnames(ci)[1] ] <- ci[,1]
data_to_plot[,colnames(ci)[2] ] <- ci[,2]
ci <- conf_interval(data_to_plot$mean, data_to_plot$n, data_to_plot$sem, q=90)
data_to_plot[,colnames(ci)[1] ] <- ci[,1]
data_to_plot[,colnames(ci)[2] ] <- ci[,2]
data_to_plot$group <- ifelse(data_to_plot$treat_social == 1, 'negative info', 'no negative info')
data_to_plot$treatment <- data_to_plot$treat_article

for (y in outcomes) {
    p <- gg_subgroup_meanplot(data_to_plot[data_to_plot[,'outcome']==y,])
    p <- p + labs(y=str_replace(y, '_', ' '))
    fname <- paste('interact_means_', y, '.png', sep='')
    ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)
}


# HETEROGENEOUS EFFECTS
# ---------------------

suffix <- '_2'
outcomes <- paste(c('feel', 'would_eat', 'interest_purchase', 'notified_available_yes', 'entered_email'), suffix, sep='')

# Appeal effects by whether respondent agreed with negative social information.
df[,'negative_agree_binary'] <- as.integer(df[,'negative_agree'] > 4)
# tab(df[,'negative_agree'], df[,'negative_agree_binary'])
moderator <- 'negative_agree_binary'

df_temp_long <- df[,c(treatments, moderator, outcomes)] %>% gather_(key='outcome', value='value', outcomes)
data_to_plot <- df_temp_long %>% 
    group_by(negative_agree_binary, treat_social, treat_article, outcome) %>%
    summarise(
        mean=mean(value, na.rm=TRUE),
        sem=sem(value, na.rm=TRUE),
        n=sum(!is.na(value)))
data_to_plot <- data_to_plot[!is.na(data_to_plot$treat_social),]
data_to_plot$group <- NA
data_to_plot$group[data_to_plot$negative_agree_binary == 1 & data_to_plot$treat_social == 1] <- 'negative info + agree'
data_to_plot$group[data_to_plot$negative_agree_binary == 0 & data_to_plot$treat_social == 1] <- 'negative info + disagree'
data_to_plot$group[data_to_plot$treat_social == 0] <- 'no negative info'

ci <- conf_interval(data_to_plot$mean, data_to_plot$n, data_to_plot$sem, q=95)
data_to_plot[,colnames(ci)[1] ] <- ci[,1]
data_to_plot[,colnames(ci)[2] ] <- ci[,2]
ci <- conf_interval(data_to_plot$mean, data_to_plot$n, data_to_plot$sem, q=90)
data_to_plot[,colnames(ci)[1] ] <- ci[,1]
data_to_plot[,colnames(ci)[2] ] <- ci[,2]

data_to_plot$treatment <- data_to_plot$treat_article
for (y in outcomes) {
    p <- gg_subgroup_meanplot(data_to_plot[data_to_plot[,'outcome']==y,])
    p <- p + labs(y=str_replace(y, '_', ' '))
    fname <- paste('subgroup_negative_agree_', y, '.png', sep='')
    ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)
}


# Appeal effects by respondent recollection of clean meat.
df[,'recall_binary'] <- as.integer(df[,'recall'] > 3)
moderator <- 'recall_binary'  # 

data_to_plot <- get_subgroup_means(df, treatments[1], outcomes, moderator)

for (y in outcomes) {
    p <- gg_subgroup_meanplot(data_to_plot[data_to_plot[,'outcome']==y,])
    p <- p + labs(y=str_replace(y, '_', ' '))
    fname <- paste('subgroup_recall_', y, '.png', sep='')
    ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)
}



# EFFECTS BY BASELINE INTEREST IN CLEAN MEAT
# Answer to question: Are effects driven by individuals already favorable towards clean meat?

df[,'feel_1_cut'] <- cut(df[,'feel_1'], breaks=c(1,4,5,8), include.lowest=TRUE, right=FALSE, labels=c('low', 'neutral', 'high'))
tab(df[,'feel_1'], df[,'feel_1_cut'])
moderator <- 'feel_1_cut'

# data_to_plot <- list()
# for (outcome in outcomes) {
#     coef_table <- get_coef_tables_interact(df, outcome, treatments[1], moderator)
#     coef_table$moderator <- factor(coef_table$moderator, levels=levels(df[,moderator]))
#     data_to_plot[[outcome]] <- coef_table
# }

coef_tables <- lapply(outcomes, FUN=function(y) get_coef_tables_interact(df, y, treatments[1], moderator))
names(coef_tables) <- outcomes
coef_table <- do.call(rbind, coef_tables)
coef_table$y <- to_title(str_replace(rownames(coef_table), paste0(suffix, '.[0-9]+$'), ''))
coef_table$treatment <- paste0('Appeal ', coef_table$treatment)
rownames(coef_table) <- NULL
coef_table$moderator <- factor(coef_table$moderator, levels=levels(df[,moderator]))

p1 <- gg_subgroup_coefplot(coef_table, x='estimate', y='y', color='treatment')
legend <- get_legend(p1)
p1 <- p1 + facet_wrap(~moderator, ncol=1) + 
    theme(strip.background=element_blank()) + 
    labs(title=paste0('Baseline ', to_title(str_replace(moderator, '_1_cut$', ''))), y='ATE', x=NULL) + 
    guides(color=FALSE)
# p <- p + labs(title=to_title(y)) + guides(color=guide_legend(title=paste0(to_title(moderator),': '), reverse=FALSE))


df[,'interest_purchase_1_cut'] <- cut(df[,'interest_purchase_1'], breaks=c(1,3,4,6), include.lowest=TRUE, right=FALSE, labels=c('low', 'neutral', 'high'))
tab(df[,'interest_purchase_1'], df[,'interest_purchase_1_cut'])
moderator <- 'interest_purchase_1_cut'

# data_to_plot <- list()
# for (outcome in outcomes) {
#     coef_table <- get_coef_tables_interact(df, outcome, treatments[1], moderator)
#     coef_table$moderator <- factor(coef_table$moderator, levels=levels(df[,moderator]))
#     data_to_plot[[outcome]] <- coef_table
# }

coef_tables <- lapply(outcomes, FUN=function(y) get_coef_tables_interact(df, y, treatments[1], moderator))
names(coef_tables) <- outcomes
coef_table <- do.call(rbind, coef_tables)
coef_table$y <- to_title(str_replace(rownames(coef_table), paste0(suffix, '.[0-9]+$'), ''))
coef_table$treatment <- paste0('Appeal ', coef_table$treatment)
rownames(coef_table) <- NULL
coef_table$moderator <- factor(coef_table$moderator, levels=levels(df[,moderator]))

p2 <- gg_subgroup_coefplot(coef_table, x='estimate', y='y', color='treatment')
p2 <- p2 + facet_wrap(~moderator, ncol=1) + 
    theme(strip.background=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
    labs(title=paste0('Baseline ', to_title(str_replace(moderator, '_1_cut$', ''))), y='ATE') +
    guides(color=FALSE)

fname <- paste('subgroup_effects_interest.png', sep='')
lay <- rbind(
    c(1,2),
    c(3,3)
)
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), grid.arrange(p1, p2, legend, heights=c(10,1), widths=c(8,5), layout_matrix=lay), width=width, height=height, units="cm", dpi=dpi)

# data_to_plot <- get_subgroup_means(df, treatments[1], outcomes, moderator)
# for (y in outcomes) {
#     p <- gg_subgroup_meanplot(data_to_plot[data_to_plot[,'outcome']==y,])
#     p <- p + facet_wrap(~moderator, ncol=1, scales='free_y')
#     p <- p + labs(y=str_replace(y, '_', ' '))
#     fname <- paste('subgroup_means_', moderator, '_', y, '.png', sep='')
#     ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)
#     # grid.arrange(p1, p2, ncol = 2)
# }


# EFFECTS BY BASELINE MEAT CONSUMPTION
# Answer to question: are effects concentrated among those who already eat very little meat?
df[,'ffq_total_sum_meat_cut'] <- cut(df[,'ffq_total_sum_meat'], breaks=quantile(df[,'ffq_total_sum_meat'], c(0.0, 0.25, 0.5, 0.75, 1.0), na.rm=TRUE), include.lowest=TRUE, right=TRUE, labels=c('Bottom quartile', '2nd quartile', '3rd quartile', 'Top quartile'))
quantile(df[,'ffq_total_sum_meat'], c(0.0, 0.25, 0.5, 0.75, 1.0), na.rm=TRUE)
tab(df[,'ffq_total_sum_meat'], df[,'ffq_total_sum_meat_cut'])
tab(df[,'ffq_total_sum_meat_cut'])
moderator <- 'ffq_total_sum_meat_cut'

coef_tables <- lapply(outcomes, FUN=function(y) get_coef_tables_interact(df, y, treatments[1], moderator))
names(coef_tables) <- outcomes
coef_table <- do.call(rbind, coef_tables)
coef_table$y <- to_title(str_replace(rownames(coef_table), paste0(suffix, '.[0-9]+$'), ''))
coef_table$treatment <- paste0('Appeal ', coef_table$treatment)
rownames(coef_table) <- NULL
coef_table$moderator <- factor(coef_table$moderator, levels=levels(df[,moderator]))

p <- gg_subgroup_coefplot(coef_table, x='estimate', y='y', color='treatment')
p <- p + facet_wrap(~moderator, ncol=1) + theme(strip.background = element_blank()) + labs(title='Baseline servings of meat per week', y='ATE', x=NULL)
fname <- paste('subgroup_effects_meat.png', sep='')
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height+5, units="cm", dpi=dpi)


# EFFECTS BY BASELINE EASE REDUCE
df[,'ease_eliminate_1_cut'] <- cut(df[,'ease_eliminate_1'], breaks=c(1,4,5,8), include.lowest=TRUE, right=FALSE, labels=c('low', 'neutral', 'high'))
tab(df[,'ease_eliminate_1'], df[,'ease_eliminate_1_cut'])
moderator <- 'ease_eliminate_1_cut'

coef_tables <- lapply(outcomes, FUN=function(y) get_coef_tables_interact(df, y, treatments[1], moderator))
names(coef_tables) <- outcomes
coef_table <- do.call(rbind, coef_tables)
coef_table$y <- str_replace(rownames(coef_table), '.[0-9]+$', '')
rownames(coef_table) <- NULL
coef_table$moderator <- factor(coef_table$moderator, levels=levels(df[,moderator]))

p <- gg_subgroup_coefplot(coef_table, x='estimate', y='y', color='treatment')
p <- p + facet_wrap(~moderator, ncol=1) + theme(strip.background = element_blank()) + labs(x=to_title(moderator), y='ATE')
fname <- paste('subgroup_effects_', moderator, '.png', sep='')
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)



# EFFECTS BY BASELINE EXPECTED REDUCE
df[,'expect_reduce_cut'] <- cut(df[,'expect_reduce'], breaks=quantile(df[,'expect_reduce'], c(0.0, 0.5, 1.0), na.rm=TRUE), include.lowest=TRUE, right=TRUE, labels=c('low', 'high'))
tab(df[,'expect_reduce'], df[,'expect_reduce_cut'])
moderator <- 'expect_reduce_cut'

coef_tables <- lapply(outcomes, FUN=function(y) get_coef_tables_interact(df, y, treatments[1], moderator))
names(coef_tables) <- outcomes
coef_table <- do.call(rbind, coef_tables)
coef_table$y <- str_replace(rownames(coef_table), '.[0-9]+$', '')
rownames(coef_table) <- NULL
coef_table$moderator <- factor(coef_table$moderator, levels=levels(df[,moderator]))

p <- gg_subgroup_coefplot(coef_table, x='estimate', y='y', color='treatment')
p <- p + facet_wrap(~moderator, ncol=1) + theme(strip.background = element_blank()) + labs(x=to_title(moderator), y='ATE')
fname <- paste('subgroup_effects_', moderator, '.png', sep='')
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)


# EFFECTS BY BASELINE IDEOLOGY
tab(df[,'ideology'])
df[,'ideology_cut'] <- cut(df[,'ideology'], breaks=c(1,3,4,6), right=FALSE, labels=c('left', 'middle', 'right'))
# tab(df[,'ideology'], df[,'ideology_cut'])
moderator <- 'ideology_cut'

coef_tables <- lapply(outcomes, FUN=function(y) get_coef_tables_interact(df, y, treatments[1], moderator))
names(coef_tables) <- outcomes
coef_table <- do.call(rbind, coef_tables)
coef_table$y <- to_title(str_replace(rownames(coef_table), paste0(suffix, '.[0-9]+$'), ''))
coef_table$treatment <- paste0('Appeal ', coef_table$treatment)
rownames(coef_table) <- NULL
coef_table$moderator <- factor(coef_table$moderator, levels=levels(df[,moderator]))

p <- gg_subgroup_coefplot(coef_table, x='estimate', y='y', color='treatment')
p <- p + facet_wrap(~moderator, ncol=1) + theme(strip.background = element_blank()) + labs(title=paste0('Baseline ', to_title(str_replace(moderator, '_cut$', ''))), y='ATE', x=NULL)
fname <- paste('subgroup_effects_ideology.png', sep='')
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height+5, units="cm", dpi=dpi)




# EFFECTS BY BASELINE EXPECTED REDUCE AMOUNT
df[,'expect_reduce_amt_cut'] <- cut(df[,'expect_reduce_amt'], breaks=quantile(df[,'expect_reduce_amt'], c(0.0, 0.5, 1.0), na.rm=TRUE), include.lowest=TRUE, right=TRUE, labels=c('low', 'high'))
tab(df[,'expect_reduce_amt'], df[,'expect_reduce_amt_cut'])
moderator <- 'expect_reduce_amt_cut'

coef_table <- get_coef_tables_interact(df, outcome, treatments[1], moderator)
coef_table$moderator <- factor(coef_table$moderator, levels=levels(df[,moderator]))

y <- outcomes[3]
p <- gg_subgroup_coefplot(coef_table)
p <- p + labs(title=to_title(y)) + guides(color=guide_legend(title=paste0(to_title(moderator),': '), reverse=FALSE))
fname <- paste('subgroup_effects_', moderator, '_', y, '.png', sep='')
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)


data_to_plot <- get_subgroup_means(df, treatments[1], outcomes, moderator)
for (y in outcomes) {
    p <- gg_subgroup_meanplot(data_to_plot[data_to_plot[,'outcome']==y,])
    # p <- p + facet_wrap(~moderator, ncol=1, scales='free_y')
    p <- p + labs(y=str_replace(y, '_', ' '))
    fname <- paste('subgroup_means_', moderator, '_', y, '.png', sep='')
    ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)
    # grid.arrange(p1, p2, ncol = 2)
}


# EFFECTS BY BASELINE COST-BENEFIT DIFFERENCE
df[,'cost_benefit_1_cut'] <- cut(df[,'cost_benefit_1'], breaks=quantile(df[,'cost_benefit_1'], c(0.0, 0.5, 1.0), na.rm=TRUE), include.lowest=TRUE, right=TRUE, labels=c('low', 'high'))
tab(df[,'cost_benefit_1'], df[,'cost_benefit_1_cut'])
moderator <- 'cost_benefit_1_cut'

data_to_plot <- get_subgroup_means(df, treatments[1], outcomes, moderator)

for (y in outcomes) {
    p <- gg_subgroup_meanplot(data_to_plot[data_to_plot[,'outcome']==y,])
    p <- p + facet_wrap(~moderator, ncol=1, scales='free_y')
    p <- p + labs(y=str_replace(y, '_', ' '))
    fname <- paste('subgroup_', moderator, '_', y, '.png', sep='')
    ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)
    # grid.arrange(p1, p2, ncol = 2)
}


# EFFECTS BY BASELINE NUMBER OF CONCERNS
df[,'num_concerns_1_cut'] <- cut(df[,'num_concerns_1'], breaks=quantile(df[,'num_concerns_1'], c(0.0, 0.5, 1.0), na.rm=TRUE), include.lowest=TRUE, right=TRUE, labels=c('low', 'high'))
tab(df[,'num_concerns_1'], df[,'num_concerns_1_cut'])
moderator <- 'num_concerns_1_cut'

data_to_plot <- get_subgroup_means(df, treatments[1], outcomes, moderator)

for (y in outcomes) {
    p <- gg_subgroup_meanplot(data_to_plot[data_to_plot[,'outcome']==y,])
    p <- p + facet_wrap(~moderator, ncol=1, scales='free_y')
    p <- p + labs(y=str_replace(y, '_', ' '))
    fname <- paste('subgroup_', moderator, '_', y, '.png', sep='')
    ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)
    # grid.arrange(p1, p2, ncol = 2)
}


# EFFECTS BY BASELINE NUMBER OF BENEFITS
df[,'num_benefits_1_cut'] <- cut(df[,'num_benefits_1'], breaks=quantile(df[,'num_benefits_1'], c(0.0, 0.5, 1.0), na.rm=TRUE), include.lowest=TRUE, right=FALSE, labels=c('low', 'high'))
tab(df[,'num_benefits_1'], df[,'num_benefits_1_cut'])
moderator <- 'num_benefits_1_cut'

data_to_plot <- get_subgroup_means(df, treatments[1], outcomes, moderator)

for (y in outcomes) {
    p <- gg_subgroup_meanplot(data_to_plot[data_to_plot[,'outcome']==y,])
    # p <- p + facet_wrap(~moderator, ncol=1, scales='free_y')
    p <- p + labs(y=str_replace(y, '_', ' '))
    fname <- paste('subgroup_', moderator, '_', y, '.png', sep='')
    ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)
    # grid.arrange(p1, p2, ncol = 2)
}


# EFFECTS BY BASELINE EDUCATION
tab(df[,'educ'])
df[,'educ_cut'] <- cut(df[,'educ'], breaks=c(1,5,8), right=FALSE, labels=c('low', 'high'))
# tab(df[,'educ'], df[,'educ_cut'])
moderator <- 'educ_cut'

coef_table <- get_coef_tables_interact(df, outcome, treatments[1], moderator)
coef_table$moderator <- factor(coef_table$moderator, levels=levels(df[,moderator]))

y <- outcomes[3]
p <- gg_subgroup_coefplot(coef_table)
p <- p + labs(title=to_title(y)) + guides(color=guide_legend(title=paste0(to_title(moderator),': '), reverse=FALSE))
fname <- paste('subgroup_effects_', moderator, '_', y, '.png', sep='')
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)

data_to_plot <- get_subgroup_means(df, treatments[1], outcomes, moderator)

for (y in outcomes) {
    p <- gg_subgroup_meanplot(data_to_plot[data_to_plot[,'outcome']==y,])
    p <- p + facet_wrap(~moderator, ncol=1, scales='free_y')
    p <- p + labs(y=str_replace(y, '_', ' '))
    fname <- paste('subgroup_means_', moderator, '_', y, '.png', sep='')
    ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)
    # grid.arrange(p1, p2, ncol = 2)
}


# EFFECTS BY BASELINE AGE
tab(df[,'age'])
cutpoints <- c(18, 25, 31, 41, max(df$age, na.rm=TRUE))
labels <- c('18-24', '25-30', '31-40', '41-87')
# hist(data[,subgroup])
df[,'age_cut'] <- cut(df[,'age'], breaks=cutpoints, include.lowest=TRUE, right=FALSE, labels=labels)
tab(df$age, df$age_cut)
moderator <- 'age_cut'

coef_table <- get_coef_tables_interact(df, outcome, treatments[1], moderator)
coef_table$moderator <- factor(coef_table$moderator, levels=levels(df[,moderator]))

y <- outcomes[3]
p <- gg_subgroup_coefplot(coef_table)
p <- p + labs(title=to_title(y)) + guides(color=guide_legend(title=paste0(to_title(moderator),': '), reverse=FALSE))
fname <- paste('subgroup_effects_', moderator, '_', y, '.png', sep='')
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)

data_to_plot <- get_subgroup_means(df, treatments[1], outcomes, moderator)

for (y in outcomes) {
    p <- gg_subgroup_meanplot(data_to_plot[data_to_plot[,'outcome']==y,])
    # p <- p + facet_wrap(~moderator, ncol=1, scales='free_y')
    p <- p + labs(y=str_replace(y, '_', ' '))
    fname <- paste('subgroup_means_', moderator, '_', y, '.png', sep='')
    ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)
    # grid.arrange(p1, p2, ncol = 2)
}


# RELATIONSHIPS BETWEEN MEASURES OF INTEREST IN CLEAN MEAT
# --------------------------------------------------------

# measures of interest in clean meat.
suffix <- '_2'
outcomes_likert <- paste(c('feel', 'would_eat', 'interest_purchase'), suffix, sep='')
outcomes_binary <- paste(c('notified_available_yes', 'entered_email'), suffix, sep='')
outcomes <- c(outcomes_likert, outcomes_binary)

# wave 1 measures
outcomes_1 <- str_replace(outcomes, '_2$', '_1')

# change measures
outcomes_chg <- str_replace(outcomes, '_2$', '_chg_1_2')

# relationship between outcomes in wave 1 versus wave 2.
diag(cor(x=df_control[,outcomes_1], y=df_control[,outcomes], use='pairwise.complete.obs', method='pearson'))

# relationship between outcomes in wave 2.
cor(df_control[,outcomes], use='pairwise.complete.obs', method='pearson')

# relationship between outcomes and all other (numeric) variables.
numeric_cols <- sapply(df_control, is.numeric)
corrs <- round(data.frame(cor(x=df_control[,numeric_cols], y=df_control[,outcomes], use='pairwise.complete.obs', method='pearson')), 4)
corrs[order(-abs(corrs[, outcomes[1] ])),]


# ATTITUDINAL PREDICTORS OF INTEREST IN CLEAN MEAT
# ------------------------------------------------

# these are the attitudes regarding clean meat, vegetarianism, and food that
# might predict interest in clean meat.
# Variables:
#   concern variables
#   benefit variables

concern_vars_1 <- paste(concern_vars, '_1', sep='')
benefit_vars_1 <- paste(benefit_vars, '_1', sep='')

concern_vars_chg <- paste(concern_vars, '_chg_1_2', sep='')
concern_vars_2 <- paste(concern_vars, '_2', sep='')

cor(df_control[,c(concern_vars_1) ], use='pairwise.complete.obs', method='spearman')

# colMeans(df_control[, concern_vars_1 ], na.rm=TRUE)
colMeans(df_control[, benefit_vars_1 ], na.rm=TRUE)

# plots difference in means for interest in clean meat by concerns raised.
outcome <- outcomes[1]
concern_means <- get_group_means(df_control, concern_vars_1, outcome)
concern_means$group <- ifelse(concern_means$group == 1, 'Concerned', 'Not concerned')
concern_means$group_name <- str_replace(to_title(concern_means$group_name, '_1'), '^Concern ', '')
p1 <- gg_group_meanplot(concern_means)
p1 <- p1 + scale_colour_manual(values=tableau10_cb[c(6, 5)])
legend <- get_legend(p1)
p1 <- p1 + labs(x=NULL, y='', title=to_title(outcome, suffix), size=20) + guides(color=FALSE)
p1 <- p1 + ylim(limits=c(1.0, 6.0))


outcome <- outcomes[2]
concern_means <- get_group_means(df_control, concern_vars_1, outcome)
concern_means$group <- ifelse(concern_means$group == 1, 'Concerned', 'Not concerned')
concern_means$group_name <- str_replace(to_title(concern_means$group_name, '_1'), '^Concern ', '')
p2 <- gg_group_meanplot(concern_means)
p2 <- p2 + labs(x=NULL, y='', title=to_title(outcome, suffix), size=20) + guides(color=FALSE)
p2 <- p2 + ylim(limits=c(1.0, 6.0))
p2 <- p2 + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
p2 <- p2 + scale_colour_manual(values=tableau10_cb[c(6, 5)])

outcome <- outcomes[3]
concern_means <- get_group_means(df_control, concern_vars_1, outcome)
concern_means$group <- ifelse(concern_means$group == 1, 'Concerned', 'Not concerned')
concern_means$group_name <- str_replace(to_title(concern_means$group_name, '_1'), '^Concern ', '')
p3 <- gg_group_meanplot(concern_means)
p3 <- p3 + labs(x=NULL, y='', title=to_title(outcome, suffix), size=20) + guides(color=FALSE)
p3 <- p3 + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
p3 <- p3 + ylim(limits=c(1.0, 6.0))
p3 <- p3 + scale_colour_manual(values=tableau10_cb[c(6, 5)])

outcome <- outcomes[5]
concern_means <- get_group_means(df_control, concern_vars_1, outcome)
concern_means$group <- ifelse(concern_means$group == 1, 'Concerned', 'Not concerned')
concern_means$group_name <- str_replace(to_title(concern_means$group_name, '_1'), '^Concern ', '')
p4 <- gg_group_meanplot(concern_means)
p4 <- p4 + labs(x=NULL, y='', title=to_title(outcome, suffix), size=20) + guides(color=FALSE)
p4 <- p4 + ylim(limits=c(0.0, 1.0))
p4 <- p4 + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
p4 <- p4 + scale_colour_manual(values=tableau10_cb[c(6, 5)])

fname <- 'barrier_concerns.png'
lay <- rbind(
    c(1,2,3,4),
    # c(3,4),
    c(5,5)
)
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), grid.arrange(p1, p2, p3, p4, legend, layout_matrix=lay, heights=c(5,1), widths=c(6,5,5,5,1)), width=width+10, height=height-5, units="cm", dpi=dpi)


# benefit_means <- get_group_means(df_control, benefit_vars_1, outcome)

# regressions examining relationship between attitudes and interest in clean meat.
covs <- c('as.factor(educ)', 'age', 'gender', 'ideology', 'ffq_total_sum_meat')
form <- get_formula(outcomes[5], c(concern_vars_1, covs))
summary(lm(form, data=df_control))

form <- get_formula(outcomes[5], c(concern_vars_1))
summary(lm(form, data=df_control))

# relationship between change in concern and change in interest in clean meat.
cor(x=df_control[,concern_vars_chg], y=df_control[,outcomes_chg], use='pairwise.complete.obs', method='spearman')

form <- get_formula(outcomes_chg[3], c(concern_vars_chg, covs))
summary(lm(form, data=df_control))

# do "unnatural" and "unsafe" concerns subside over time?

# means_1 <- colMeans(df_control[,c(concern_vars_1)], na.rm=TRUE)
# means_2 <- colMeans(df_control[,c(concern_vars_2)], na.rm=TRUE)
# means_chg <- colMeans(df_control[,c(concern_vars_chg)], na.rm=TRUE)

df_control_temp <- df_control[complete.cases(df_control[,concern_vars_1]),]
means_1 <- apply(df_control_temp[,concern_vars_1], MARGIN=2, FUN=function(x) {
        return(data.frame(
            mean=mean(x, na.rm=TRUE),
            sem=sem(x, na.rm=TRUE),
            n=len(x[!is.na(x)]))
        )
    }
)
means_2 <- apply(df_control_temp[,concern_vars_2], MARGIN=2, FUN=function(x) {
        return(data.frame(
            mean=mean(x, na.rm=TRUE),
            sem=sem(x, na.rm=TRUE),
            n=len(x[!is.na(x)]))
        )
    }
)
means_chg <- apply(df_control_temp[,concern_vars_chg], MARGIN=2, FUN=function(x) {
        return(data.frame(
            mean=mean(x, na.rm=TRUE),
            sem=sem(x, na.rm=TRUE),
            n=len(x[!is.na(x)]))
        )
    }
)
means <- rbind(do.call(rbind, means_1), do.call(rbind, means_2), do.call(rbind, means_chg))
# names(means_1) <- names(means_2) <- names(means_chg) <- concern_vars
# means <- cbind(means_1, means_2, means_chg)
print(means)

for (i in 1:len(concern_vars)) {
    test <- t.test(x=df_control_temp[, concern_vars_2[i] ], y=df_control_temp[, concern_vars_1[i] ], paired=TRUE)
    print(c(concern_vars[i], test$estimate, test$p.value))
}




# LIFESTYLE PREDICTORS OF INTEREST IN CLEAN MEAT
# ----------------------------------------------

# these are the variables beyond food-related attitudes that reflect broader
# lifestyles and worldviews that might predict interest in clean meat.

predictors <- c('ffq_total_sum_meat', 'FFQfreqVegMeats', 'expect_reduce', 'ease_reduce_1', 'ease_eliminate_1')  # 'expect_reduce_amt', 

# relationship between predictors and outcomes.
cor(x=df_control[,predictors], y=df_control[,outcomes], use='pairwise.complete.obs', method='pearson')

# regressions examining relationship between attitudes and interest in clean meat.
covs <- c('as.factor(educ)', 'age', 'gender', 'ideology')
form <- get_formula(outcomes[5], c(predictors))
summary(lm(form, data=df_control))

form <- get_formula(outcomes[1], c(predictors, covs))
summary(lm(form, data=df_control))


# old?

# bivariate plots
df_temp_long <- df_control[,c(predictors, outcomes_likert)] %>% gather_(key='outcome', value='outcome_value', outcomes_likert) %>% gather_(key='predictor', 'predictor_value', predictors)
p <- ggplot(df_temp_long, aes(x=predictor_value, outcome_value, outcome_value, color=outcome), fill='gray10') + 
    geom_point(size=0.2, alpha=0.8, position=position_jitter(width=0.1, height=0.2)) +
    geom_smooth(method="loess", size=1.2, se=FALSE) + 
    facet_wrap(~predictor, scales='free_x') +
    facetplot_theme
p

df_temp_long <- df_control[,c(predictors, outcomes_binary)] %>% gather_(key='outcome', value='outcome_value', outcomes_binary) %>% gather_(key='predictor', 'predictor_value', predictors)
p <- ggplot(df_temp_long, aes(x=predictor_value, outcome_value, outcome_value, color=outcome), fill='gray10') +
    geom_point(size=0.2, alpha=0.8, position=position_jitter(width=0.2, height=0.05)) +
    # geom_jitter(width=0.01) + 
    geom_smooth(method="loess", size=1.2, se=FALSE) +
    facet_wrap(~predictor, scales='free_x') + 
    facetplot_theme
p

# inter-quartile range effect plots.
iqr_effects <- data.frame(matrix(NA, nrow=len(outcomes)*len(predictors), ncol=3))
colnames(iqr_effects) <- c('outcome', 'predictor', 'iqr_effect')
row <- 1
for (i in 1:len(outcomes)) {
    outcome <- outcomes[i]
    for (j in 1:len(predictors)) {
        predictor <- predictors[j]
        form <- get_formula(outcome, predictor)
        lm0 <- lm(form, data=df_control)
        result_table <- tidy(lm0)
        coef <- result_table$estimate[result_table$term==predictor]
        iqr <- IQR(df_control[,predictor], na.rm=TRUE)
        coef_iqr <- coef*iqr
        iqr_effects[row,] <- c(outcome, predictor, coef_iqr)
        row <- row + 1
    }
}
iqr_effects$iqr_effect <- as.numeric(iqr_effects$iqr_effect)

p <- ggplot(iqr_effects, aes(x=iqr_effect, y=predictor, color=outcome)) + 
    geom_vline(xintercept=0, linetype='dashed', color='gray20') +
    geom_point(size=3.5, alpha=0.8) + 
    plot_theme
p

# DEMOGRAPHIC PREDICTORS OF INTEREST IN CLEAN MEAT
# ------------------------------------------------

# these are the predictors of interest in clean meat that are least
# theoretically interesting, but likely the most relevant to clean meat
# companies.

demographics <- c('age', 'educ', 'ideology', 'income')  # 'gender'

# relationship between demographics and outcomes.
cor(x=df_control[,demographics], y=df_control[,outcomes], use='pairwise.complete.obs', method='pearson')

df_temp_long <- df_control[,c(demographics, outcomes_likert)] %>% gather_(key='outcome', value='outcome_value', outcomes_likert) %>% gather_(key='predictor', 'predictor_value', demographics)
p <- ggplot(df_temp_long, aes(x=predictor_value, outcome_value, outcome_value, color=outcome), fill='gray10') + 
    geom_point(size=0.2, alpha=0.8, position=position_jitter(width=0.1, height=0.2)) +
    geom_smooth(method="loess", size=1.2, se=TRUE) + 
    facet_wrap(~predictor, scales='free_x') +
    facetplot_theme
p



# SUSCEPTIBILITY TO NATURALISTIC FALLACY
# --------------------------------------
# RQ4: Absent of any messaging or social information, what kinds of 
# individuals are most susceptible to bringing up the naturalistic fallacy 
# after learning about clean meat?

suffix <- '_2'
outcomes <- paste(c('concern_unnatural', 'concern_unsafe', 'concern_unhealthy', 'concern_un'), suffix, sep='')

df_control[,outcomes[4] ] <- rowSums(df_control[,outcomes[1:3] ], na.rm=TRUE)
# stopifnot(max(df_control[,outcomes[4]], na.rm=TRUE) == 3)


predictors <- c(ease_vars, meat_attitude_vars[-len(meat_attitude_vars)], demographics, 'expect_reduce', 'ffq_total_sum_meat', 'FFQfreqVegMeats')
predictors <- sapply(predictors, FUN=function(x) ifelse(x %in% colnames(df_control), x, paste(x, '_1', sep='')))
names(predictors) <- NULL
# numeric_cols <- sapply(df_control, is.numeric)

# relationship with predictors
corrs <- round(data.frame(cor(x=df_control[,predictors], y=df_control[,outcomes], use='pairwise.complete.obs', method='pearson')), 4)
temp <- apply(corrs, MARGIN=2, FUN=function(x) names(x[order(-abs(x))][1:10]))
top_vars <- as.character(unique(unlist(data.frame(temp))))
top_corrs <- corrs[top_vars,][order(-abs(corrs[top_vars,1])),]
top_corrs

form <- get_formula(outcomes[1], predictors)
summary(lm(form, data=df_control))

form <- get_formula(outcomes[2], predictors)
summary(lm(form, data=df_control))

form <- get_formula(outcomes[3], predictors)
summary(lm(form, data=df_control))

form <- get_formula(outcomes[4], predictors)
summary(lm(form, data=df_control))

# sort(apply(df, MARGIN=2, FUN=function(x) sum(is.na(x))), decreasing=TRUE)[1:10]

# Reduce(function(...) {
#         data <- merge(..., by='row.names', all=TRUE)
#         rownames(data) <- data[,"Row.names"]
#         data[,!names(data) %in% "Row.names"]
#     },
#     temp
# )


# demographics: 
#   zipcode
#   age,
#   gender,
#   education,
#   household income
#   liberal vs. conservative
#   religion
# Attitudes:
#   perception that conventional animal products harm the environment
#   
# Individual difference variables (i.e. psyhological characteristics):
#   

# TODO



# file: main_analysis.r
# This file contains the main specifications examining average treatment
# effects in the anti-naturalistic fallacy study.

library(stringr)
library(survival)
library(support.CEs)
library(jsonlite)
library(readr)
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(gridExtra)
# library(lmtest)
# library(margins)


# PRELIMINARIES
# -------------
len = length
nunique = function(x) {return(len(unique(x)))}
tab = function(..., exclude=NULL) table(..., exclude=exclude)

source('yougov/analysis/utils.r')
source('yougov/analysis/plotting.r')
source('yougov/analysis/methods.r')

# constants for plots
fig_output_dir = 'yougov/output/figures'
data_out_path = 'yougov/output/data_for_paper.js'
dpi = 200
height = 12
# height_per_subplot = 2.5
width = 16
theme_set(theme_bw())
tableau10_cb = c('#006ba4', '#ff800e', '#ababab', '#595959', '#5f9ed1', '#c85200', '#898989', '#a3c8ec', '#ffbc79', '#cfcfcf')
# grid::grid.raster(tableau10_cb, int=F)
scale_colour_discrete = function(...) scale_colour_manual(..., values=tableau10_cb)

# theme for plots
plot_theme = theme(
  legend.position="bottom",
  legend.direction="horizontal",
  legend.box="horizontal",
  text=element_text(size=15),
  strip.text.x = element_text(size=17, face="bold"),
  legend.text=element_text(size=16),
  plot.margin=unit(c(0.1,0.1,0.1,0.1), 'cm'),
  axis.line=element_line(colour = "black"),
  panel.border=element_blank(),
  panel.background=element_blank(),
  panel.grid.major=element_blank(),
  panel.grid.minor=element_blank(),
  plot.title=element_text(hjust = 0.5, face="bold")
)

facetplot_theme = theme(legend.position="bottom",
  legend.direction="horizontal",
  legend.box="horizontal",
  text=element_text(size=11),
  legend.text=element_text(size=14),
  plot.margin=unit(c(0.1,0.1,0.1,0.1), 'cm'),
  panel.background=element_blank(),
  panel.grid.major=element_blank(),
  panel.grid.minor=element_blank(),
  plot.title=element_text(hjust = 0.5, face="bold")
)

# data to be saved to JSON and inserted into the paper.
data_for_paper = list()


# READS IN CLEANED DATA
# ---------------------

# reads in cleaned data.
df = read_csv('yougov/data/cleaned/all_waves_clean.csv')
dim(df)
# str(df)
# colnames(df)
# counts number of missing values for each variable.
# sort(apply(df, MARGIN=2, FUN=function(x) sum(is.na(x))), decreasing=TRUE)

# colnames(df)
treatments = c('treat_article', 'treat_social')

# converts treatments to factor variables.
social_names = c('not_exposed', 'exposed')
appeal_names = c('control', 'debunk_unnatural', 'embrace_unnatural', 'descriptive_norm')
df$treat_social = factor(df$treat_social, levels=social_names, exclude=NA)
df$treat_article = factor(df$treat_article, levels=appeal_names, exclude=NA)

# df_dce$treat_social = factor(df_dce$treat_social, levels=c(0,1), labels=social_names, exclude=NA)
# df_dce$treat_article = factor(df_dce$treat_article, levels=c(0,1,2,3), labels=appeal_names, exclude=NA)


# SURVEY ATTRITION
# ----------------
wave1_vars = colnames(df)[str_detect(colnames(df), '_wave1$')]
wave2_vars = colnames(df)[str_detect(colnames(df), '_wave2$')]
n_wave1 = sum(rowSums(!is.na(df[,wave1_vars]), na.rm=TRUE) > 0)
n_wave2 = sum(rowSums(!is.na(df[,wave2_vars]), na.rm=TRUE) > 0)
print(c(n_wave1, n_wave2))
print(n_wave2 / n_wave1)

data_for_paper[['x0_1n']] = n_wave1
data_for_paper[['x0_2n']] = n_wave2
data_for_paper[['x0_2p']] = (n_wave2 / n_wave1) * 100


# REMOVES RESPONSES WHERE TREATMENT IS NA
# ---------------------------------------
# (these come from wave 1, before treatment was assigned)
df = df %>% filter(!is.na(treat_article) & !is.na(treat_social))
# df_dce = df_dce %>% filter(!is.na(treat_article) & !is.na(treat_social))
stopifnot(sum(is.na(df[,treatments])) == 0)
# stopifnot(sum(is.na(df_dce[,treatments])) == 0)

# dim(df_dce)
# dim(df)
# tab(df[,treatments[1]])
# tab(df[,treatments[2]])
# tab(df_dce[,treatments[1]])
# tab(df_dce[,treatments[2]])


# subsets to control observations only.
df_control = df[df$treat_article == 'control' & df$treat_social == 'not_exposed' & !is.na(df$treat_article) & !is.na(df$treat_social), ]
dim(df_control)

# converts gender to factor
# df$gender = as.numeric(factor(df$gender, levels=c('Male', 'Female')))


# NAMES OF OUTCOME VARIABLES
# --------------------------

# main outcome variables
interest_vars = c('interest_purchase', 'would_eat')
wtp_vars = c('wtp')

# peripheral outcome variables
ease_vars = c('ease_eliminate', 'ease_reduce')
intention_vars = c('expect_reduce', 'expect_reduce_amt')
meat_attitude_vars = c('veg_moral', 'harms', 'harms_concern')


# covariates
demographics = c('age', 'educ', 'income', 'gender')

# other...
other_vars = c('ideology')

# combines outcome variables into a list, with names by suffixes.
outcomes = list(
    wave1pre = paste0(c(interest_vars, wtp_vars), '_wave1pre'),
    wave1 = paste0(c(interest_vars, wtp_vars), '_wave1'),
    chg_wave1pre_wave1 = paste0(c(interest_vars, wtp_vars), '_chg_wave1pre_wave1'),
    chg_wave1pre_wave2 = paste0(c(interest_vars, wtp_vars), '_chg_wave1pre_wave2'),
    wave2 = paste0(c(interest_vars, wtp_vars), '_wave2'),
    # standardized outcomes.
    wave1pre_std = paste0(c(interest_vars, wtp_vars), '_wave1pre_std'),
    wave1_std = paste0(c(interest_vars, wtp_vars), '_wave1_std'),
    chg_wave1pre_wave1_std = paste0(c(interest_vars, wtp_vars), '_chg_wave1pre_wave1_std'),
    chg_wave1pre_wave2_std = paste0(c(interest_vars, wtp_vars), '_chg_wave1pre_wave2_std'),
    wave2_std = paste0(c(interest_vars, wtp_vars), '_wave2_std')
)


# STANDARDIZES VARIABLES
# ----------------------
regex = paste('^', c(interest_vars, ease_vars, intention_vars, meat_attitude_vars, wtp_vars), collapse='|', sep='')
vars_to_std = colnames(df)[str_detect(colnames(df), regex)]
df[,paste0(vars_to_std, '_std')] = apply(df[,vars_to_std], MARGIN=2, FUN=function(x) (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))

# sanity checks.
stopifnot(all(round(colMeans(df[,paste0(vars_to_std, '_std')], na.rm=TRUE), 5)==0))
stopifnot(all(round(apply(df[,paste0(vars_to_std, '_std')], MARGIN=2, FUN=sd, na.rm=TRUE), 5)==1))


# EXPERIMENTAL ARM SIZES
# ----------------------
arm_sizes = df %>% group_by(treat_article, treat_social) %>% 
    summarise(n=sum(!is.na(treat_article))) %>%
    ungroup()
arm_sizes = arm_sizes[complete.cases(arm_sizes),]
print(arm_sizes)

# number of control participants.
# data_for_paper[['x1']] = arm_sizes %>% filter(treat_article == 0, treat_social == 0) %>% select(n) %>% as.numeric(.)

# PRETREATMENT BALANCE
# --------------------
wave1_cols_isnumeric = unlist(lapply(df[colnames(df)[ str_detect(colnames(df), '_wave1pre$')] ], is.numeric))
# wave1_cols_isnumeric = wave1_cols_isnumeric[!str_detect(names(wave1_cols_isnumeric), '^FFQ')]
pretreat_vars = names(wave1_cols_isnumeric[wave1_cols_isnumeric])
balance_tables = get_coef_tables(df, pretreat_vars, treatments)
balance_tables = clean_treatment_names(balance_tables, treatments)
# print(balance_tables)

balance_table = bind_rows(balance_tables, .id="outcome") %>% filter(term!="(Intercept)") %>% select(outcome, term, p.value)   # %>% spread(key=term, value=p.value)
balance_table$p.value = ordered(pvalue_to_str(balance_table$p.value))
balance_table$outcome = to_title(str_replace(balance_table$outcome, '_wave1pre$', ''))
balance_table$term = to_title(balance_table$term)

color_palette <- colorRampPalette(c(tableau10_cb[6], "white"))(len(levels(balance_table$p.value)))

# plots the balance table.
p = ggplot(balance_table, aes(x=term, y=outcome)) +
    geom_tile(aes(fill=p.value), colour = "gray50") +
    scale_fill_manual(values=color_palette, name = "", labels=levels(balance_table$p.value)) +
    plot_theme +
    theme(
        axis.ticks=element_blank(),
        axis.title=element_text(size=20),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=12, vjust=1.1, hjust=1.2, angle=30),
        legend.position="top"
    ) +
    labs(x = "Treatment Arm", y = "Pre-treatment variable") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0))

fname = 'balance.png'
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height+14, units="cm", dpi=dpi)


# replicates balance tables for only those respondents who completed both
# survey waves.
completed_all_waves = rowSums(!is.na(df[,wave2_vars])) > 0
df_completed_all = df[completed_all_waves,]
stopifnot(nrow(df_completed_all) == sum(completed_all_waves))

wave1_cols_isnumeric = unlist(lapply(df_completed_all[colnames(df_completed_all)[ str_detect(colnames(df_completed_all), '_wave1pre$')] ], is.numeric))
# wave1_cols_isnumeric = wave1_cols_isnumeric[!str_detect(names(wave1_cols_isnumeric), '^FFQ')]
pretreat_vars = names(wave1_cols_isnumeric[wave1_cols_isnumeric])

balance_tables = get_coef_tables(df_completed_all, pretreat_vars, treatments)
balance_tables = clean_treatment_names(balance_tables, treatments)
# print(balance_tables)

balance_table = bind_rows(balance_tables, .id="outcome") %>% filter(term!="(Intercept)") %>% select(outcome, term, p.value)   # %>% spread(key=term, value=p.value)
balance_table$p.value = ordered(pvalue_to_str(balance_table$p.value))
balance_table$outcome = to_title(str_replace(balance_table$outcome, '_wave1pre$', ''))
balance_table$term = to_title(balance_table$term)

color_palette <- colorRampPalette(c(tableau10_cb[6], "white"))(len(levels(balance_table$p.value)))

# plots the balance table.
p = ggplot(balance_table, aes(x=term, y=outcome)) +
    geom_tile(aes(fill=p.value), colour = "gray50") +
    scale_fill_manual(values=color_palette, name = "", labels=levels(balance_table$p.value)) +
    plot_theme +
    theme(
        axis.ticks=element_blank(),
        axis.title=element_text(size=20),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=12, vjust=1.1, hjust=1.2, angle=30),
        legend.position="top"
    ) +
    labs(x = "Treatment Arm", y = "Pre-treatment variable") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0))

fname = 'balance_completed_all_waves.png'
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height+14, units="cm", dpi=dpi)



# UNIVARIATE DESCRIPTIVES
# -----------------------
# this section of code plots histograms and bar charts for some of the key
# outcome variables.

suffix = 'wave1pre'
temp_outcomes = outcomes[[suffix]]
temp_outcomes = temp_outcomes[temp_outcomes %in% colnames(df)]

# summary statistics for outcome variables for each wave.
summary_stats = df %>% 
    gather_(key='outcome', value='value', unlist(temp_outcomes, use.names=FALSE)) %>%
    group_by(outcome) %>%
    summarise(mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE), min=min(value, na.rm=TRUE), q25=quantile(value, 0.25, na.rm=TRUE), q50=quantile(value, 0.25, na.rm=TRUE), q75=quantile(value, 0.25, na.rm=TRUE), max=max(value, na.rm=TRUE))
print(summary_stats)

# summary statistics for outcome variables for each wave among control group only.
summary_stats_control = df_control %>% 
    gather_(key='outcome', value='value', unlist(temp_outcomes, use.names=FALSE)) %>%
    group_by(outcome) %>%
    summarise(mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE), min=min(value, na.rm=TRUE), q25=quantile(value, 0.25, na.rm=TRUE), q50=quantile(value, 0.25, na.rm=TRUE), q75=quantile(value, 0.25, na.rm=TRUE), max=max(value, na.rm=TRUE))
print(summary_stats_control)


# % of participants answering "probably yes" or "definitely" yes to would_eat
data_for_paper[['x2']] = mean(df[,paste('would_eat', suffix, sep='_')]>3, na.rm=TRUE) * 100

# % of participants answering "extremely interested" or "very interested"
# in clean meat.
data_for_paper[['x3']] = mean(df[,paste('interest_purchase', suffix, sep='_')]>3, na.rm=TRUE) * 100

# plots univariate descriptives for key outcome variables for wave 1.
melted = df %>% select_(.dots=temp_outcomes) %>% gather_(key='outcome', value='value', temp_outcomes, na.rm=TRUE)
melted = melted %>% group_by(outcome, value) %>% summarize(count=n()) %>% mutate(pct=count/sum(count)*100)
melted$outcome = to_title(str_replace(melted$outcome, sprintf('_%s$', suffix), ''))
p = ggplot(melted, aes(x=factor(value), y=pct, group=outcome)) + 
    geom_bar(stat='identity', fill=tableau10_cb[3]) +
    facet_wrap(~outcome, scales='free_x', ncol=5) +
    plot_theme + 
    scale_y_continuous(breaks=round(seq(0, 100, by=20), 5), limits=c(0, 100)) +
    labs(x=NULL, y='% of Respondents') +
    theme(strip.background = element_blank())
fname = sprintf('interest_%s.png', suffix)
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width+18, height=height, units="cm", dpi=dpi)


# RELATIONSHIPS BETWEEN MEASURES OF INTEREST IN CLEAN MEAT
# --------------------------------------------------------
# this section examines the relationships between our measures of interest in
# clean meat across the survey waves.

# measures of interest in clean meat.
# outcomes_likert = paste(c('feel', 'would_eat', 'interest_purchase'), suffix, sep='_')
# outcomes = c(outcomes_likert)  # , outcomes_binary
# outcomes_chg = str_replace(outcomes, paste0(suffix, '$'), paste0('_chg_1', suffix))

# correlations between outcomes measured in wave 1 versus waves 2 and 3.
wave1v2 = diag(cor(x=df_control[,outcomes[['wave1']]], y=df_control[,outcomes[['wave2']]], use='pairwise.complete.obs', method='pearson'))
names(wave1v2) = paste(outcomes[['wave1']], outcomes[['wave2']], sep="_vs_")
# print(data.frame(wave1v2))

# correlations between outcomes and all other (numeric) variables.
# note: this is useful for getting a general sense of what variables relate
# most strongly to the outcomes.
numeric_cols = df_control %>%
  select_if(function(x) !is.factor(x)) %>%
  select_if(function(x) is.numeric(x) & sd(x, na.rm=TRUE) > 0.0 & !is.na(sd(x, na.rm=TRUE))) %>%
  colnames()
corrs = round(data.frame(cor(x=df_control[,numeric_cols], y=df_control[,outcomes[['wave2']]], use='pairwise.complete.obs', method='pearson')), 4)
# print(corrs[order(-abs(corrs[, outcomes[['wave3']][1] ])),][1:30,])


# LIFESTYLE PREDICTORS OF INTEREST IN CLEAN MEAT
# ----------------------------------------------
# these are the variables beyond food-related attitudes that reflect broader
# lifestyles and worldviews that might predict interest in clean meat:

# TODO: complete these analyses.
# see commit GH-33e8ecc74d812ae032d0b66a2a479435a30dc77f for old code that
# produced bivariate plots and inter-quartile plots.

# predictors = c('ffq_total_sum_meat_wave1', 'FFQfreqVegMeats_wave1', 'expect_reduce_wave1', 'ease_reduce_wave1', 'ease_eliminate_wave1')  # 'expect_reduce_amt', 

# # relationship between predictors and outcomes.
# cor(x=df_control[,predictors], y=df_control[,outcomes_3], use='pairwise.complete.obs', method='pearson')

# # regressions examining relationship between attitudes and interest in clean meat.
# covs = c('as.factor(educ_1)', 'age_wave1', 'gender_wave1', 'ideology_wave1')
# form = get_formula(outcomes_3[2], c(predictors))
# summary(lm(form, data=df_control))

# form = get_formula(outcomes[1], c(predictors, covs))
# summary(lm(form, data=df_control))



# DEMOGRAPHIC PREDICTORS OF INTEREST IN CLEAN MEAT
# ------------------------------------------------

# these are the predictors of interest in clean meat that are least
# theoretically interesting, but likely the most relevant to clean meat
# companies.

# TODO: do these analyses.

# relationship between demographics and outcomes.
# cor(x=df_control[,demographics], y=df_control[,outcomes], use='pairwise.complete.obs', method='pearson')

# df_temp_long = df_control[,c(demographics, outcomes_likert)] %>% gather_(key='outcome', value='outcome_value', outcomes_likert) %>% gather_(key='predictor', 'predictor_value', demographics)
# p = ggplot(df_temp_long, aes(x=predictor_value, outcome_value, outcome_value, color=outcome), fill='gray10') + 
#     geom_point(size=0.2, alpha=0.8, position=position_jitter(width=0.1, height=0.2)) +
#     geom_smooth(method="loess", size=1.2, se=TRUE) + 
#     facet_wrap(~predictor, scales='free_x') +
#     facetplot_theme
# p


# SUSCEPTIBILITY TO NATURALISTIC FALLACY
# --------------------------------------
# RQ4: Absent of any messaging or social information, what kinds of 
# individuals are most susceptible to bringing up the naturalistic fallacy 
# after learning about clean meat?

# TODO: do these analyses.

# suffix = 'wave3'

# constructs vector of outcomes.
# temp_outcomes = paste(c('concern_unnatural', 'concern_unsafe', 'concern_unhealthy', 'concern_un'), suffix, sep='_')
# df_control[,temp_outcomes[4] ] = rowSums(df_control[,temp_outcomes[1:3] ], na.rm=TRUE)
# stopifnot(max(df_control[,temp_outcomes[4]], na.rm=TRUE) == 3)

# constructs vector of predictors.
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
# predictors = paste0(c(ease_vars, meat_attitude_vars, demographics, 'ideology', 'expect_reduce', 'ffq_total_sum_meat', 'FFQfreqVegMeats'), '_wave1')
# predictors = predictors[predictors %in% colnames(df_control)]
# numeric_cols = sapply(df_control[,predictors], is.numeric)
# predictors = predictors[numeric_cols]
# names(predictors) = NULL


# relationship with predictors
# corrs = round(data.frame(cor(x=df_control[,predictors], y=df_control[,temp_outcomes], use='pairwise.complete.obs', method='pearson')), 4)
# temp = apply(corrs, MARGIN=2, FUN=function(x) names(x[order(-abs(x))][1:10]))
# top_vars = as.character(unique(unlist(data.frame(temp))))
# top_corrs = corrs[top_vars,][order(-abs(corrs[top_vars,1])),]
# top_corrs

# form = get_formula(temp_outcomes[1], predictors)
# summary(lm(form, data=df_control))

# form = get_formula(temp_outcomes[2], predictors)
# summary(lm(form, data=df_control))

# form = get_formula(temp_outcomes[3], predictors)
# summary(lm(form, data=df_control))

# form = get_formula(temp_outcomes[4], predictors)
# summary(lm(form, data=df_control))

# sort(apply(df, MARGIN=2, FUN=function(x) sum(is.na(x))), decreasing=TRUE)[1:10]

# Reduce(function(...) {
#         data = merge(..., by='row.names', all=TRUE)
#         rownames(data) = data[,"Row.names"]
#         data[,!names(data) %in% "Row.names"]
#     },
#     temp
# )


# EFFECTS OF APPEALS AND SOCIAL INFORMATION ON INTEREST IN CLEAN MEAT
# -------------------------------------------------------------------
# this section examines the treatment effects of the pro-clean meat appeals
# and the anti-adoption social information.

# experimental cell means
# suffix = 'chg_wave1_wave3'
this_outcomes = c('would_eat_chg_wave1pre_wave1', 'interest_purchase_chg_wave1pre_wave1', 'wtp_wave1')
means = df %>% group_by(treat_social, treat_article) %>%
    select_(.dots=this_outcomes) %>%
    summarise_each(funs(mean(., na.rm=TRUE)))
print(means)


# The following block of commented code can be used to plot the effects of the
# appeals or the social information treatments on a vector of outcomes of
# interest. Just changed "suffix" and "temp_outcomes" as desired.
#
# suffix = 'chg_wave1_wave3_std'
# temp_outcomes = outcomes[[suffix]]
# 
# # extracts coef_tables.
# coef_tables = get_coef_tables(df, temp_outcomes, treatments)
# coef_tables = clean_treatment_names(coef_tables, treatments)
# 
# effects_appeal1 = do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeal_names[2],]))
# effects_appeal2 = do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeal_names[3],]))
# effects_appeal3 = do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeal_names[4],]))
# 
# effects_appeal1$outcome = to_title(str_replace(rownames(effects_appeal1), paste0(suffix, '$'), ''))
# effects_appeal2$outcome = to_title(str_replace(rownames(effects_appeal2), paste0(suffix, '$'), ''))
# effects_appeal3$outcome = to_title(str_replace(rownames(effects_appeal3), paste0(suffix, '$'), ''))
# 
# effects_appeals = rbind(effects_appeal1, effects_appeal2, effects_appeal3)
# effects_appeals$term = factor(to_title(effects_appeals$term))
# rownames(effects_appeals) = NULL
#
# ci = conf_interval(effects_appeals$estimate, effects_appeals$n, effects_appeals$std.error, q=95)
# effects_appeals[,colnames(ci)[1]] = ci[,1]
# effects_appeals[,colnames(ci)[2]] = ci[,2]
# ci = conf_interval(effects_appeals$estimate, effects_appeals$n, effects_appeals$std.error, q=90)
# effects_appeals[,colnames(ci)[1]] = ci[,1]
# effects_appeals[,colnames(ci)[2]] = ci[,2]
#
# # plots the results.
# p1 = gg_coefplot(effects_appeals, color=effects_appeals$term) + 
#     facet_wrap(~term, scales='fixed', ncol=1) + 
#     theme(strip.background = element_blank()) + 
#     guides(color=FALSE)
#     # labs(title='') +
# fname = paste0('effects_appeals_interest_', suffix, '.png')
# ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p1, width=width, height=height, units="cm", dpi=dpi)


# THE CODE BELOW ESTIMATES THE EFFECTS OF THE APPEALS AND SOCIAL INFORMATION
# TREATMENT FOR WAVES 2 AND 3.
# the code below constructs a table of treatment effects of the following
# form:
#              term estimate std.error statistic p.value    n           outcome
# 1   treat_social1  -0.4181    0.0381  -10.9805  0.0000 2622              feel
# 2   treat_social1  -0.3247    0.0384   -8.4512  0.0000 2622 interest_purchase
# 3   treat_social1  -0.3058    0.0385   -7.9331  0.0000 2622         would_eat
# 4   treat_social1   0.0701    0.0145    4.8185  0.0000 2622 concern_unhealthy
# 5   treat_social1   0.0626    0.0199    3.1506  0.0016 2622    concern_unsafe
# 6   treat_social1   0.0308    0.0207    1.4890  0.1366 2622     concern_taste
# 7   treat_social1   0.0176    0.0203    0.8680  0.3855 2622      concern_cost
# 8   treat_social1   0.0032    0.0192    0.1686  0.8661 2622 concern_unnatural
# 9  treat_article1   0.1073    0.0543    1.9757  0.0483 2622              feel
# 10 treat_article1   0.1004    0.0548    1.8314  0.0672 2622 interest_purchase 
# 
# In this table, each row is the treatment effect of a particular treatment on
# a particular outcome. (note: there is also a variable for survey wave)

# outcome variables to use.
vars_to_use = c(interest_vars, wtp_vars)  # main outcomes
# vars_to_use = c(ease_vars, intention_vars, meat_attitude_vars)  # peripheral outcomes

# constructs vector of outcomes.
suffix1 = 'wave1' # 'wave1', 'chg_wave1pre_wave1'
suffix1_std = paste0(suffix1, '_std')
suffix2 = 'wave2' # 'wave2', 'chg_wave1pre_wave2'
suffix2_std = paste0(suffix2, '_std')
temp_outcomes1 = sapply(vars_to_use, FUN=function(x) ifelse(paste(x, suffix1_std, sep='_') %in% colnames(df), paste(x, suffix1_std, sep='_'), paste(x, suffix1, sep='_')))
temp_outcomes2 = sapply(vars_to_use, FUN=function(x) ifelse(paste(x, suffix2_std, sep='_') %in% colnames(df), paste(x, suffix2_std, sep='_'), paste(x, suffix2, sep='_')))
temp_outcomes = c(temp_outcomes1, temp_outcomes2)
waves = c(rep('Wave 1', len(temp_outcomes1)), rep('Wave 2', len(temp_outcomes2)))
waves = waves[temp_outcomes %in% colnames(df)]
temp_outcomes = temp_outcomes[temp_outcomes %in% colnames(df)]
names(temp_outcomes) = NULL

# estimates treatment effects.
coef_tables = get_coef_tables(df, temp_outcomes, treatments)
coef_tables = clean_treatment_names(coef_tables, treatments)
stopifnot(all(names(coef_tables)==temp_outcomes) & len(coef_tables) == len(waves))

# extracts appeal treatment effect coefficients from coef_tables.
effects_appeal1 = do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeal_names[2],]))
effects_appeal2 = do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeal_names[3],]))
effects_appeal3 = do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==appeal_names[4],]))
effects_appeal1$outcome = rownames(effects_appeal1)
effects_appeal2$outcome = rownames(effects_appeal2)
effects_appeal3$outcome = rownames(effects_appeal3)
effects_appeals = bind_rows(effects_appeal1, effects_appeal2, effects_appeal3)
rownames(effects_appeals) = NULL
effects_appeals$wave = rep(waves, 3)

# extracts anti-adoption social information treatment effect coefficients from
# coef_tables.
effects_social = do.call(rbind, lapply(coef_tables, FUN=function(x) x[x$term==social_names[2],]))
effects_social$outcome = rownames(effects_social)
rownames(effects_social) = NULL
effects_social$wave = waves

# binds all coefficients into single table.
effects = bind_rows(effects_social, effects_appeals, id=NULL)

# constructs confidence intervals.
ci = conf_interval(effects$estimate, effects$n, effects$std.error, q=95)
effects[,colnames(ci)[1] ] = ci[,1]
effects[,colnames(ci)[2] ] = ci[,2]
ci = conf_interval(effects$estimate, effects$n, effects$std.error, q=90)
effects[,colnames(ci)[1] ] = ci[,1]
effects[,colnames(ci)[2] ] = ci[,2]


# cleans up text for plotting.
temp_regex = paste(paste0('_', c(suffix1, suffix1_std), '$', collapse='|'), paste0('_', c(suffix2, suffix2_std), '$', collapse='|'), sep='|')
effects$outcome = str_replace(effects$outcome, temp_regex, '')
effects$outcome_group = to_title(ifelse(effects$outcome %in% interest_vars, 'interest',
    ifelse(effects$outcome %in% ease_vars, 'ease',
    ifelse(effects$outcome %in% meat_attitude_vars, 'moral', 'other'))))
effects$outcome_group = factor(effects$outcome_group)
effects$outcome = to_title(effects$outcome)
effects$outcome = str_replace(effects$outcome, 'Interest Purchase', 'Int. Purch.')

effects$term = factor(paste0('Effects of "', to_title(effects$term), '"'))
effects$wave = factor(effects$wave)

# effects$size = ifelse(str_detect(effects$term, "Article"), 1.0, 2.0)

plots = effects %>%
    # mutate(label=tableau10_cb[as.numeric(term)]) %>%
    group_by(term) %>%
    do(plot=gg_coefplot(., color="wave") +
        scale_colour_manual(values=c("gray70", "black")) + 
        labs(title=.$term[1], y="", size=20) +
        scale_y_continuous(breaks=round(seq(-0.4, 0.4, by=0.1), 1), limits=c(-0.4, 0.4)) +
        # ylim(c(-0.5, 0.5)) +
        # facet_wrap(~outcome_group, scales='free_y', ncol=1, strip.position=c('left')) + 
        theme(
            strip.background = element_rect(fill="gray90", color="gray90"), 
            strip.text.y = element_text(angle=180, face="bold", hjust=0), 
            plot.margin=unit(c(0.4,0.2,0.4,0.2), "cm")
        ) +
        # axis.text.y=element_text(size=11), axis.text.x=element_text(size=11)
        guides(color=guide_legend(title=NULL, size=3))
    )

legend = get_legend(plots$plot[[1]])
plots$plot[[1]] = plots$plot[[1]] + guides(color=FALSE)
plots$plot[[2]] = plots$plot[[2]] + guides(color=FALSE)
plots$plot[[3]] = plots$plot[[3]] + guides(color=FALSE)
plots$plot[[4]] = plots$plot[[4]] + guides(color=FALSE)
plots$plot[[4]] = plots$plot[[4]] + labs(y='Average treatment effect (std.)')

# todo: figure out how to resize facets within the same facet plot.
# gt = ggplotGrob(plots$plot[[1]])

# # Get the column index in the gt layout corresponding to the panels.
# panelI <- gt$layout$l[grepl("panel", gt$layout$name)]

# # Replace the default panel heights.
# gt$heights[panelI] <- unit(c(0.1,0.2), "null")
# grid.newpage()
# grid.draw(gt)

# Add extra width between panels (assuming two panels)
# gt$widths[panelI[1] + 1] = unit(1, "cm")

lay = rbind(
    1,
    2,
    3,
    4,
    5
)
fname = 'effects.png'
grobs = plots$plot
grobs[[5]] = legend
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), 
    grid.arrange(grobs=grobs, layout_matrix=lay, heights=c(8,8,8,8,2), widths=c(6)), 
    width=width, 
    height=height+26, 
    units="cm", 
    dpi=dpi
)


# INTERACTIONS BETWEEN TREATMENTS
# -------------------------------
# in this section, we examine whether any of the appeals were significantly
# more/less effective when preceded by the negative social information than
# when not.

suffix = 'wave1' # 'chg_wave1pre_wave2'
suffix_std = paste0(suffix, '_std')
temp_outcomes = sapply(vars_to_use, FUN=function(x) ifelse(paste(x, suffix_std, sep='_') %in% colnames(df), paste(x, suffix_std, sep='_'), paste(x, suffix, sep='_')))

moderator = 'treat_social'

# constructs coefficient tables.
coef_tables = lapply(temp_outcomes, FUN=function(y) get_coef_tables_interact(as.data.frame(df), y, treatments[1], moderator))
names(coef_tables) = temp_outcomes
coef_table = do.call(rbind, coef_tables)

# cleans up outcome text.
temp_regex = paste0('_', suffix, '\\.[0-9]+$|_', suffix_std, '\\.[0-9]+$')
coef_table$outcome = str_replace(rownames(coef_table), temp_regex, '')
rownames(coef_table) = NULL

# cleans up text for plotting.
coef_table$outcome_group = to_title(ifelse(coef_table$outcome %in% interest_vars, 'interest', 'other'))
coef_table$outcome_group = factor(coef_table$outcome_group)
coef_table$outcome = to_title(coef_table$outcome)
coef_table$outcome = str_replace(coef_table$outcome, 'Interest Purchase', 'Int. Purch.')

coef_table$treatment = paste0('Effects of "', to_title(coef_table$treatment), '"')
coef_table$moderator = to_title(coef_table$moderator)

plots = coef_table %>%
    group_by(treatment) %>%
    do(plot=gg_coefplot(., color="moderator") +
        scale_colour_manual(values=c("black", "gray70")) + 
        labs(title=.$treatment[1], y="", size=18) +
        scale_y_continuous(breaks=round(seq(-0.4, 0.4, by=0.1), 2), limits=c(-0.4, 0.4)) +
        # ylim(c(-0.5, 0.5)) +
        # facet_wrap(~outcome_group, scales='free_y', ncol=1, strip.position=c('left')) + 
        theme(
            strip.background = element_rect(fill="gray90", color="gray90"),
            strip.text.y = element_text(size=10, angle=180, face="bold", hjust=0),
            plot.margin=unit(c(0.5,0.2,0.5,0.2), "cm")
        ) +
        # axis.text.y=element_text(size=11), axis.text.x=element_text(size=11)
        guides(color=guide_legend(title=NULL, size=3))
    )

legend = get_legend(plots$plot[[1]])
plots$plot[[1]] = plots$plot[[1]] + guides(color=FALSE)
plots$plot[[2]] = plots$plot[[2]] + guides(color=FALSE)
plots$plot[[3]] = plots$plot[[3]] + guides(color=FALSE)
plots$plot[[3]] = plots$plot[[3]] + labs(y='Average treatment effect (std.)')

lay = rbind(
    1,
    2,
    3,
    4
)
fname = paste0('subgroup_effects_negative_', suffix, '.png')
grobs = plots$plot
grobs[[4]] = legend
ggsave(
    filename=paste(fig_output_dir, '/', fname, sep=''), 
    grid.arrange(grobs=grobs, layout_matrix=lay, heights=c(8,8,8,2), widths=c(6)),
    width=width,
    height=height+22, 
    units="cm",
    dpi=dpi
)


# p = gg_subgroup_coefplot(coef_table, x='estimate', y='treatment', color='y')
# p = p + facet_wrap(~moderator, ncol=1) + theme(strip.background = element_blank(), strip.text.x=element_text(size=11, face="bold")) + labs(title='', y='ATE', x=NULL) + scale_y_continuous(breaks=round(seq(-0.8, 0.8, by=0.2), 2), limits=c(-0.8, 0.8))
# fname = paste0('subgroup_effects_negative', suffix, '.png')
# ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height, units="cm", dpi=dpi)


# HETEROGENEOUS EFFECTS (DOCS)
# ----------------------------

# The code below examines the subgroup effects of the three appeals and/or
# anti-adoption social information.
# 
# Some other variables to examine subgroup effects by: 
#  negative_agree,
#  recall,
#  ease_eliminate, expect_reduce, expect_reduce_amt, 
#  ideology, age, education,
#  baseline number of concerns, baseline number of benefits



# HETEROGENEOUS EFFECTS BY BASELINE INTEREST IN CLEAN MEAT
# --------------------------------------------------------
# Answer to the question: Are effects driven by individuals already favorable
# towards clean meat?

suffix = 'wave2' # 'chg_wave1pre_wave2_std'
temp_outcomes = paste(c(interest_vars, wtp_vars), suffix, sep='_')

# INTEREST_PURCHASE
df[,'interest_purchase_wave1pre_cut'] = cut(
    pull(df, 'interest_purchase_wave1pre'), 
    breaks=c(1,3,4,6), 
    include.lowest=TRUE, 
    right=FALSE, 
    labels=c('low', 'neutral', 'high')
)
# tab(df[,'interest_purchase_wave1'], df[,'interest_purchase_wave1_cut'])
moderator2 = 'interest_purchase_wave1pre_cut'

coef_tables = lapply(temp_outcomes, FUN=function(y) get_coef_tables_interact(as.data.frame(df), y, treatments[1], moderator2))
names(coef_tables) = temp_outcomes
coef_table = do.call(rbind, coef_tables)
coef_table$y = str_replace(rownames(coef_table), paste0('_', suffix, '\\.[0-9]+$'), '')
rownames(coef_table) = NULL
coef_table$moderator = factor(coef_table$moderator, levels=levels(pull(df,moderator2)))

coef_tables_social = lapply(temp_outcomes, FUN=function(y) get_coef_tables_interact(as.data.frame(df), y, treatments[2], moderator2))
names(coef_tables_social) = temp_outcomes
coef_table_social = do.call(rbind, coef_tables_social)
coef_table_social$y = str_replace(rownames(coef_table_social), paste0('_', suffix, '\\.[0-9]+$'), '')
rownames(coef_table_social) = NULL
coef_table_social$moderator = factor(coef_table_social$moderator, levels=levels(pull(df,moderator2)))

coef_table2 = bind_rows(coef_table, coef_table_social)


# WOULD_EAT
df[,'would_eat_wave1pre_cut'] = cut(pull(df,'would_eat_wave1pre'), breaks=c(1,3,4,6), include.lowest=TRUE, right=FALSE, labels=c('low', 'neutral', 'high'))
# tab(df[,'would_eat_wave1'], df[,'would_eat_wave1_cut'])
moderator3 = 'would_eat_wave1pre_cut'

coef_tables = lapply(temp_outcomes, FUN=function(y) get_coef_tables_interact(as.data.frame(df), y, treatments[1], moderator3))
names(coef_tables) = temp_outcomes
coef_table = do.call(rbind, coef_tables)
coef_table$y = str_replace(rownames(coef_table), paste0('_', suffix, '\\.[0-9]+$'), '')
rownames(coef_table) = NULL
coef_table$moderator = factor(coef_table$moderator, levels=levels(pull(df,moderator3)))

coef_tables_social = lapply(temp_outcomes, FUN=function(y) get_coef_tables_interact(as.data.frame(df), y, treatments[2], moderator3))
names(coef_tables_social) = temp_outcomes
coef_table_social = do.call(rbind, coef_tables_social)
coef_table_social$y = str_replace(rownames(coef_table_social), paste0('_', suffix, '\\.[0-9]+$'), '')
rownames(coef_table_social) = NULL
coef_table_social$moderator = factor(coef_table_social$moderator, levels=levels(pull(df,moderator3)))

coef_table3 = bind_rows(coef_table, coef_table_social)


# cleans up text for plotting.
coef_table2$y = to_title(coef_table2$y)
coef_table3$y = to_title(coef_table3$y)
coef_table2$treatment = to_title(coef_table2$treatment)
coef_table3$treatment = to_title(coef_table3$treatment)

# plots the results.
p2 = gg_subgroup_coefplot(coef_table2, x='estimate', y='moderator', color='y') +
    facet_wrap(~treatment, ncol=1) + 
    labs(title=paste0('Effects by baseline\n"', to_title(str_replace(moderator2, '_wave1pre_cut$', '')), '"'), y='ATE', x=NULL)
p3 = gg_subgroup_coefplot(coef_table3, x='estimate', y='moderator', color='y') +
    facet_wrap(~treatment, ncol=1) + 
    labs(title=paste0('Effects by baseline\n"', to_title(str_replace(moderator3, '_wave1pre_cut$', '')), '"'), y='ATE', x=NULL)
legend = get_legend(p2)

add_this_plot_theme = function(p) p + theme(
    strip.background=element_blank(), 
    plot.title=element_text(size=12),
    strip.text.x = element_text(size=11)
  ) + 
  scale_y_continuous(breaks=round(seq(-0.8, 0.8, by=0.4), 2), limits=c(-0.8, 0.8)) +
  guides(color=FALSE)
# p = p + labs(title=to_title(y)) + guides(color=guide_legend(title=paste0(to_title(moderator),': '), reverse=FALSE))
p2 = add_this_plot_theme(p2)
p3 = add_this_plot_theme(p3) + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

fname = paste0('subgroup_effects_interest_', suffix, '.png')
lay = rbind(
    c(1,2),
    c(4,4)
)
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), grid.arrange(p2, p3, legend, heights=c(9,1), widths=c(10,9), layout_matrix=lay), width=width+5, units="cm", dpi=dpi)


# HETEROGENEOUS EFFECTS BY BASELINE MEAT CONSUMPTION
# --------------------------------------------------
# Answer to the question: are effects concentrated among those who already eat
# very little meat?

suffix = 'wave2' # 'chg_wave1pre_wave2_std'
temp_outcomes = paste(c(interest_vars, wtp_vars), suffix, sep='_')

# df[,'ffq_total_sum_meat_cut'] = cut(df[,'ffq_total_sum_meat_1'], breaks=quantile(df[,'ffq_total_sum_meat_1'], c(0.0, 0.25, 0.5, 0.75, 1.0), na.rm=TRUE), include.lowest=TRUE, right=TRUE, labels=c('Bottom quartile', '2nd quartile', '3rd quartile', 'Top quartile'))
df[,'ffq_total_sum_meat_cut'] = cut(
    pull(df,'ffq_total_sum_meat_wave1pre'),
    breaks=quantile(
        df[,'ffq_total_sum_meat_wave1pre'],
        c(0.0, 0.33, 0.66, 1.0),
        na.rm=TRUE
    ), 
    include.lowest=TRUE, 
    right=TRUE, 
    labels=c('Bottom third', 'Middle third', 'Top third')
)
# df[,'ffq_total_sum_meat_cut'] = factor(df[,'ffq_total_sum_meat_cut'], levels=rev(levels(df[,'ffq_total_sum_meat_cut'])))
# quantile(df[,'ffq_total_sum_meat_wave1'], c(0.0, 0.25, 0.5, 0.75, 1.0), na.rm=TRUE)
# quantile(df[,'ffq_total_sum_meat_wave1'], c(0.0, 0.33, 0.66, 1.0), na.rm=TRUE)
# tab(df[,'ffq_total_sum_meat_wave1'], df[,'ffq_total_sum_meat_cut'])
# tab(df[,'ffq_total_sum_meat_cut'])
moderator = 'ffq_total_sum_meat_cut'

coef_tables = lapply(temp_outcomes, FUN=function(y) get_coef_tables_interact(as.data.frame(df), y, treatments[1], moderator))
names(coef_tables) = temp_outcomes
coef_table = do.call(rbind, coef_tables)
coef_table$y = str_replace(rownames(coef_table), paste0('_', suffix, '\\.[0-9]+$'), '')
rownames(coef_table) = NULL
coef_table$moderator = factor(coef_table$moderator, levels=levels(pull(df,moderator)))

coef_tables_social = lapply(temp_outcomes, FUN=function(y) get_coef_tables_interact(as.data.frame(df), y, treatments[2], moderator))
names(coef_tables_social) = temp_outcomes
coef_table_social = do.call(rbind, coef_tables_social)
coef_table_social$y = str_replace(rownames(coef_table_social), paste0('_', suffix, '\\.[0-9]+$'), '')
rownames(coef_table_social) = NULL
coef_table_social$moderator = factor(coef_table_social$moderator, levels=levels(pull(df,moderator)))

coef_table = bind_rows(coef_table, coef_table_social)

# cleans up text for plotting
coef_table$y = to_title(coef_table$y)
coef_table$treatment = paste0('Effects of "', to_title(coef_table$treatment), '"')

# plots the results.
p = gg_subgroup_coefplot(coef_table, x='estimate', y='moderator', color='y')
p = p + facet_wrap(~treatment, ncol=1) + 
    theme(strip.background = element_blank(), legend.text=element_text(size=13)) + 
    labs(x='Baseline servings of meat per week', y='ATE', title=NULL) + 
    guides(color=guide_legend(reverse=TRUE, title='Outcome: ')) + 
    scale_y_continuous(breaks=round(seq(-0.8, 0.8, by=0.4), 2), limits=c(-0.8, 0.8))
fname = paste0('subgroup_effects_meat_', suffix, '.png')
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height+5, units="cm", dpi=dpi)


# EXPORTS DATA FOR PAPER
# ----------------------
# the following code exports a .js file containing the data that will be
# inserted into the paper. This ensures that whenever the results change, the
# paper changes as well (and there are no transcription errors).

# function: write_json
# description: converts data to json and saves it to file. 
# arguments:
#   d: first argument passed to toJSON. Can be list, dataframe, or other
#       object that toJSON can handle.
#   path: location to save json file.
#   ...: other arguments passed to toJSON.
write_json = function(d, path, ...){
  write_lines('var data = ', path)
  toJSON(d, ...) %>%
     write_lines(path, append=TRUE)
  print(paste0('wrote data to ', path))
}

# rounds all numbers to N digits
max_digits = 2
to_trim = sapply(data_for_paper, is.numeric) & str_detect(data_for_paper, sprintf('\\.\\d{%s,}', max_digits))
data_for_paper_trimmed = lapply(seq_along(data_for_paper), function(i) ifelse(to_trim[i], sprintf('%.2f', data_for_paper[[i]]), data_for_paper[[i]]))
names(data_for_paper_trimmed) = names(data_for_paper)

# exorts data.
write_json(data_for_paper_trimmed, data_out_path, pretty=TRUE)


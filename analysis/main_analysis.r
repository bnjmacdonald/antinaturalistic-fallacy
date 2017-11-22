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

source('utils.r')
source('plotting.r')
source('methods.r')

# constants for plots
fig_output_dir = '../output/figures'
data_out_path = '../output/data_for_paper.js'
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
df = read.csv('../data/cleaned/all_waves_clean.csv', stringsAsFactors=FALSE)
dim(df)
# str(df)
# colnames(df)

# reads in cleaned discrete choice analysis data (long form).
df_dce = read.csv('../data/cleaned/dce_clean_long.csv', stringsAsFactors=FALSE)
dim(df_dce)
# str(df_dce)
# colnames(df_dce)

# counts number of missing values for each variable.
# sort(apply(df, MARGIN=2, FUN=function(x) sum(is.na(x))), decreasing=TRUE)

# colnames(df)
treatments = c('treat_article', 'treat_social')

# converts treatments to factor variables.
social_names = c('not_exposed', 'exposed')
appeal_names = c('control', 'debunk_unnatural', 'embrace_unnatural', 'descriptive_norm')
df$treat_social = factor(df$treat_social, levels=c(0,1), labels=social_names, exclude=NA)
df$treat_article = factor(df$treat_article, levels=c(0,1,2,3), labels=appeal_names, exclude=NA)

df_dce$treat_social = factor(df_dce$treat_social, levels=c(0,1), labels=social_names, exclude=NA)
df_dce$treat_article = factor(df_dce$treat_article, levels=c(0,1,2,3), labels=appeal_names, exclude=NA)


# SURVEY ATTRITION
# ----------------
wave1_vars = str_detect(colnames(df), '_wave1$')
wave2_vars = str_detect(colnames(df), '_wave2$')
wave3_vars = str_detect(colnames(df), '_wave3$')
n_wave1 = sum(rowSums(!is.na(df[,wave1_vars]), na.rm=TRUE) > 0)
n_wave2 = sum(rowSums(!is.na(df[,wave2_vars]), na.rm=TRUE) > 0)
n_wave3 = sum(rowSums(!is.na(df[,wave3_vars]), na.rm=TRUE) > 0)
print(c(n_wave1, n_wave2, n_wave3))
print(n_wave2 / n_wave1)
print(n_wave3 / n_wave1)

data_for_paper[['x0_1n']] = n_wave1
data_for_paper[['x0_2n']] = n_wave2
data_for_paper[['x0_3n']] = n_wave3
data_for_paper[['x0_2p']] = (n_wave2 / n_wave1) * 100
data_for_paper[['x0_3p']] = (n_wave3 / n_wave1) * 100


# REMOVES RESPONSES WHERE TREATMENT IS NA
# ---------------------------------------
# (these come from wave 1, before treatment was assigned)
df = df %>% filter(!is.na(treat_article) & !is.na(treat_social))
df_dce = df_dce %>% filter(!is.na(treat_article) & !is.na(treat_social))
stopifnot(sum(is.na(df[,treatments])) == 0)
stopifnot(sum(is.na(df_dce[,treatments])) == 0)

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

# outcome variable sets
interest_vars = c('feel', 'interest_purchase', 'would_eat')  # 'notified_available_yesmaybe'
interest_vars12 = c('notified_available_yes', 'entered_email')  # these variables only in waves 1 and 2.

concern_vars = c('concern_unhealthy', 'concern_unsafe', 'concern_taste', 'concern_cost', 'concern_unnatural')  #  'concern_none', 'concern_other', 'concern_noreason', 'cost_benefit', 'num_concerns'
benefit_vars = c('benefit_healthier', 'benefit_safer', 'benefit_tastier', 'benefit_cheaper', 'benefit_envsust', 'benefit_suffering')  # 'benefit_none', 'benefit_other', 'num_benefits'

ease_vars = c('ease_eliminate', 'ease_reduce')

meat_attitude_vars = c('veg_moral', 'sentient', 'harms', 'harms_concern', 'perceived_reduce')  # 'receive_veg_info_yesmaybe'
meat_attitude_vars12 = c('receive_veg_info_yes') # these variables only in waves 1 and 2.

# covariate variable sets
demographics = c('age', 'educ', 'income', 'gender') # religion, 

# other...
other_vars = c('ideology', 'expect_reduce', 'expect_reduce_amt')

# todo:
# animal intelligence
# animal suffering

# combines outcome variables into a list, with names by suffixes.
outcomes = list(
    chg_wave1_wave3 = paste0(interest_vars, '_chg_wave1_wave3'),
    wave1 = paste0(c(interest_vars, interest_vars12), '_wave1'),
    wave2 = paste0(c(interest_vars, interest_vars12), '_wave2'),
    wave3 = paste0(interest_vars, '_wave3'),
    # standardized outcomes.
    chg_wave1_wave3_std = paste0(interest_vars, '_chg_wave1_wave3_std'),
    wave1_std = c(paste0(interest_vars, '_wave1_std'), paste0(interest_vars12, '_wave1')),
    wave2_std = c(paste0(interest_vars, '_wave2_std'), paste0(interest_vars12, '_wave2')),
    wave3_std = paste0(interest_vars, '_wave3_std')
)


# STANDARDIZES VARIABLES
# ----------------------
regex = paste('^', c(interest_vars, ease_vars, meat_attitude_vars, other_vars), collapse='|', sep='')
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
# colnames(df)
# todo: add more variables to balance checks.
# todo: construct a single table containing all balance checks.

wave1_cols_isnumeric = unlist(lapply(df[colnames(df)[ str_detect(colnames(df), '_wave1$')] ], is.numeric))
wave1_cols_isnumeric = wave1_cols_isnumeric[!str_detect(names(wave1_cols_isnumeric), '^dce_|^FFQ')]
pretreat_vars = names(wave1_cols_isnumeric[wave1_cols_isnumeric])
balance_tables = get_coef_tables(df, pretreat_vars, treatments)
balance_tables = clean_treatment_names(balance_tables, treatments)
# print(balance_tables)

balance_table = bind_rows(balance_tables, .id="outcome") %>% filter(term!="(Intercept)") %>% select(outcome, term, p.value)   # %>% spread(key=term, value=p.value)
balance_table$p.value = ordered(pvalue_to_str(balance_table$p.value))
balance_table$outcome = to_title(str_replace(balance_table$outcome, '_wave1$', ''))
balance_table$term = to_title(balance_table$term)

color_palette <- colorRampPalette(c(tableau10_cb[6], "white"))(len(levels(balance_table$p.value)))

# plots the balance table.
p = ggplot(balance_table, aes(x=term, y=outcome)) +
    geom_tile(aes(fill=p.value), colour = "gray50") +
    scale_fill_manual(values=color_palette, name = "", labels=levels(balance_table$p.value)) +
    plot_theme +
    theme(axis.ticks=element_blank(), axis.title=element_text(size=20), axis.text.y=element_text(size=10), axis.text.x=element_text(size=12, vjust=1.0, hjust=1.2, angle=30), legend.position="top") +
    labs(x = "Treatment Arm",y = "Pre-treatment variable") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0))

fname = 'balance.png'
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height+14, units="cm", dpi=dpi)


# replicates balance tables for only those respondents who completed all three
# survey waves.
completed_all_waves = rowSums(!is.na(df[,wave3_vars])) > 0
df_completed_all = df[completed_all_waves,]
stopifnot(nrow(df_completed_all) == sum(completed_all_waves))

wave1_cols_isnumeric = unlist(lapply(df_completed_all[colnames(df_completed_all)[ str_detect(colnames(df_completed_all), '_wave1$')] ], is.numeric))
wave1_cols_isnumeric = wave1_cols_isnumeric[!str_detect(names(wave1_cols_isnumeric), '^dce_|^FFQ')]
pretreat_vars = names(wave1_cols_isnumeric[wave1_cols_isnumeric])

balance_tables = get_coef_tables(df_completed_all, pretreat_vars, treatments)
balance_tables = clean_treatment_names(balance_tables, treatments)
# print(balance_tables)

balance_table = bind_rows(balance_tables, .id="outcome") %>% filter(term!="(Intercept)") %>% select(outcome, term, p.value)   # %>% spread(key=term, value=p.value)
balance_table$p.value = ordered(pvalue_to_str(balance_table$p.value))
balance_table$outcome = to_title(str_replace(balance_table$outcome, '_wave1$', ''))
balance_table$term = to_title(balance_table$term)

color_palette <- colorRampPalette(c(tableau10_cb[6], "white"))(len(levels(balance_table$p.value)))

# plots the balance table.
p = ggplot(balance_table, aes(x=term, y=outcome)) +
    geom_tile(aes(fill=p.value), colour = "gray50") +
    scale_fill_manual(values=color_palette, name = "", labels=levels(balance_table$p.value)) +
    plot_theme +
    theme(axis.ticks=element_blank(), axis.title=element_text(size=20), axis.text.y=element_text(size=10), axis.text.x=element_text(size=12, vjust=1.0, hjust=1.2, angle=30), legend.position="top") +
    labs(x = "Treatment Arm",y = "Pre-treatment variable") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0))

fname = 'balance_completed_all_waves.png'
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height+14, units="cm", dpi=dpi)



# UNIVARIATE DESCRIPTIVES
# -----------------------
# this section of code plots histograms and bar charts for some of the key
# outcome variables.

suffix = 'wave1'
temp_outcomes = outcomes[[suffix]]

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

# % of participants entering email address.
data_for_paper[['x1']] = summary_stats %>% filter(outcome == paste('notified_available_yes', suffix, sep='_')) %>% mutate(pct=mean * 100) %>% select(pct) %>% as.numeric(.)

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
# outcomes_binary = paste(c('notified_available_yes', 'entered_email'), suffix, sep='_')
# outcomes = c(outcomes_likert)  # , outcomes_binary
# outcomes_chg = str_replace(outcomes, paste0(suffix, '$'), paste0('_chg_1', suffix))

# correlations between outcomes measured in wave 1 versus waves 2 and 3.
wave1v2 = diag(cor(x=df_control[,outcomes[['wave1']]], y=df_control[,outcomes[['wave2']]], use='pairwise.complete.obs', method='pearson'))
names(wave1v2) = paste(outcomes[['wave1']], outcomes[['wave2']], sep="_vs_")
# print(data.frame(wave1v2))

wave2v3 = diag(cor(x=df_control[,outcomes[['wave2']]], y=df_control[,outcomes[['wave3']]], use='pairwise.complete.obs', method='pearson'))
names(wave2v3) = paste(outcomes[['wave2']][1:3], outcomes[['wave3']], sep="_vs_")
# print(data.frame(wave2v3))

# correlations between outcomes and all other (numeric) variables.
# note: this is useful for getting a general sense of what variables relate
# most strongly to the outcomes.
numeric_cols = sapply(df_control, function(x) is.numeric(x) & sd(x, na.rm=TRUE) > 0.0 & !is.na(sd(x, na.rm=TRUE)))
corrs = round(data.frame(cor(x=df_control[,numeric_cols], y=df_control[,outcomes[['wave3']]], use='pairwise.complete.obs', method='pearson')), 4)
# print(corrs[order(-abs(corrs[, outcomes[['wave3']][1] ])),][1:30,])


# ATTITUDINAL PREDICTORS OF INTEREST IN CLEAN MEAT
# ------------------------------------------------
# This section answers the question: what concerns about cultured meat are
# most predictive of attitudes towards cultured meat?
#
# these are the attitudes regarding clean meat, vegetarianism, and food that
# might predict interest in clean meat:
#   concern variables
#   benefit variables

suffix = 'chg_wave1_wave3_std'
temp_outcomes = outcomes[[suffix]]

concern_vars_wave1 = paste(concern_vars, '_wave1', sep='')
concern_vars_wave2 = paste(concern_vars, '_wave2', sep='')
concern_vars_wave3 = paste(concern_vars, '_wave3', sep='')
concern_vars_chg_wave1_wave2 = paste(concern_vars, '_chg_wave1_wave2', sep='')
concern_vars_chg_wave1_wave3 = paste(concern_vars, '_chg_wave1_wave3', sep='')
benefit_vars_wave1 = paste(benefit_vars, '_wave1', sep='')

# correlations among concern variables
# cor(df_control[,c(concern_vars_wave1) ], use='pairwise.complete.obs', method='spearman')

# means of concern variables
colMeans(df[, concern_vars_wave1 ], na.rm=TRUE)
colMeans(df[, benefit_vars_wave1 ], na.rm=TRUE)

# extracts specific means for paper
temp_means = df %>% group_by(concern_unnatural_wave1) %>% summarise(avg=mean(entered_email_wave2, na.rm=TRUE))
data_for_paper[['x6_1']] = temp_means %>% filter(concern_unnatural_wave1==1) %>% .$avg * 100
data_for_paper[['x6_2']] = temp_means %>% filter(concern_unnatural_wave1==0) %>% .$avg * 100

temp_means = df %>% group_by(concern_taste_wave1) %>% summarise(avg=mean(entered_email_wave2, na.rm=TRUE))
data_for_paper[['x7_1']] = temp_means %>% filter(concern_taste_wave1==1) %>% .$avg * 100
data_for_paper[['x7_2']] = temp_means %>% filter(concern_taste_wave1==0) %>% .$avg * 100


# plots difference in means in interest in clean meat by whether or not
# respondents raised concern.
# outcome = temp_outcomes[1]
# concern_means = get_group_means(df_control, concern_vars_wave1, outcome)
# concern_means$group = ifelse(concern_means$group == 1, 'Concerned', 'Not concerned')
# concern_means$group_name = str_replace(to_title(concern_means$group_name, '_wave1'), '^Concern ', '')
# p1 = gg_group_meanplot(concern_means)
# p1 = p1 + scale_colour_manual(values=tableau10_cb[c(6, 5)])
# legend = get_legend(p1)
# p1 = p1 + labs(x=NULL, y='', title=to_title(outcome, suffix), size=20) + guides(color=FALSE)
# # p1 = p1 + ylim(limits=c(1.0, 6.0))

# benefit_means = get_group_means(df_control, benefit_vars_1, outcome)

# regressions examining relationship between concerns and interest in clean meat.
covariates = paste0(c(demographics, 'ideology', 'ffq_total_sum_meat'), '_wave1')
covariates = str_replace(covariates, 'educ_wave1', 'as.factor(educ_wave1)')
concerns_coef_tables = get_coef_tables(df, temp_outcomes, c(concern_vars_chg_wave1_wave3, covariates))
term_regex = paste('(Intercept)', '^as.factor', paste0('^', covariates, collapse='|'), sep='|')
concern_effects = bind_rows(concerns_coef_tables, .id="outcome") %>% filter(!str_detect(term, term_regex))

ci = conf_interval(concern_effects$estimate, concern_effects$n, concern_effects$std.error, q=95)
concern_effects[,colnames(ci)[1] ] = ci[,1]
concern_effects[,colnames(ci)[2] ] = ci[,2]
ci = conf_interval(concern_effects$estimate, concern_effects$n, concern_effects$std.error, q=90)
concern_effects[,colnames(ci)[1] ] = ci[,1]
concern_effects[,colnames(ci)[2] ] = ci[,2]

# extracts specific results for paper.
data_for_paper[['x4']] = concern_effects %>% filter(outcome == paste('would_eat', suffix, sep='_'), term == paste('concern_unnatural', str_replace(suffix, '_std', ''), sep='_')) %>% select(estimate) %>% abs(.) %>% as.numeric(.)
data_for_paper[['x4_1']] = concern_effects %>% filter(outcome == paste('would_eat', suffix, sep='_'), term == paste('concern_unnatural', str_replace(suffix, '_std', ''), sep='_')) %>% mutate(p.value_str=pvalue_to_str(p.value)) %>% select(p.value_str) %>% as.character(.)

data_for_paper[['x5']] = concern_effects %>% filter(outcome == paste('interest_purchase', suffix, sep='_'), term == paste('concern_unnatural', str_replace(suffix, '_std', ''), sep='_')) %>% select(estimate) %>% abs(.) %>% as.numeric(.)
data_for_paper[['x5_1']] = concern_effects %>% filter(outcome == paste('interest_purchase', suffix, sep='_'), term == paste('concern_unnatural', str_replace(suffix, '_std', ''), sep='_')) %>% mutate(p.value_str=pvalue_to_str(p.value)) %>% select(p.value_str) %>% as.character(.)

# cleans up variable names for plotting.
concern_effects$term = str_replace(to_title(concern_effects$term, str_replace(suffix, '_std$', '')), '^Concern ', 'Concern: ')
concern_effects$outcome = to_title(concern_effects$outcome, suffix)

# plots the results.
p = ggplot(concern_effects, aes(y=estimate, x=outcome), color="black") + 
    geom_hline(yintercept=0, linetype='dashed', color='gray30') +
    scale_y_continuous(breaks=round(seq(-0.6, 0.6, by=0.2), 2), limits=c(-0.5, 0.5)) +
    geom_errorbar(aes(ymin=lower_ci90, ymax=upper_ci90), width=0, size=0.8, position=position_dodge(width=0.7)) +
    geom_errorbar(aes(ymin=lower_ci95, ymax=upper_ci95), width=0, size=0.3, position=position_dodge(width=0.7)) +
    labs(y='Std. effect of change in concern, waves 1-3', x=NULL) +
    geom_point(size=2.3, position=position_dodge(width=0.7)) + 
    # geom_line(size=1, position=position_dodge(width=0.1)) + 
    # facet_wrap(~color, ncol=3, scales='fixed') + 
    guides(color=guide_legend(title=NULL, reverse=FALSE)) +
    plot_theme +
    theme(strip.background = element_blank()) +
    facet_wrap(~term, ncol=1) +
    coord_flip()
fname = sprintf('barrier_concerns_%s.png', suffix)
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height+4, units="cm", dpi=dpi)


# examines whether "unnatural" and "unsafe" concerns subside over time.
completed_all_waves = rowSums(!is.na(df_control[,concern_vars_wave3])) > 0
means_temp = df_control %>% filter(completed_all_waves) %>% select(one_of(c(concern_vars_wave1, concern_vars_wave2, concern_vars_wave3, concern_vars_chg_wave1_wave3))) %>% summarise_each(funs(mean(., na.rm=TRUE))) %>% unlist()

pvalues_temp = data.frame(matrix(NA, nrow=len(concern_vars), ncol=2))
rownames(pvalues_temp) = concern_vars
colnames(pvalues_temp) = c('estimate', 'pvalue')
for (i in 1:len(concern_vars)) {
    test = t.test(x=df_control[completed_all_waves, concern_vars_wave3[i] ], y=df_control[completed_all_waves, concern_vars_wave1[i] ], paired=TRUE)
    pvalues_temp[i,] = c(test$estimate, test$p.value)
}

data_for_paper[['x8_1']] = sum(completed_all_waves)
data_for_paper[['x8_2']] = means_temp['concern_unnatural_wave1'] * 100
data_for_paper[['x8_3']] = means_temp['concern_unnatural_wave3'] * 100
data_for_paper[['x8_4']] = abs(data_for_paper[['x8_3']] - data_for_paper[['x8_2']])
data_for_paper[['x8_5']] = pvalue_to_str(pvalues_temp['concern_unnatural', 'pvalue'])

data_for_paper[['x9_1']] = means_temp['concern_unsafe_wave1'] * 100
data_for_paper[['x9_2']] = means_temp['concern_unsafe_wave3'] * 100
data_for_paper[['x9_3']] = abs(data_for_paper[['x9_2']] - data_for_paper[['x9_1']])
data_for_paper[['x9_4']] = pvalue_to_str(pvalues_temp['concern_unsafe', 'pvalue'])

data_for_paper[['x10_1']] = means_temp['concern_unhealthy_wave1'] * 100
data_for_paper[['x10_2']] = means_temp['concern_unhealthy_wave3'] * 100
data_for_paper[['x10_3']] = abs(data_for_paper[['x10_2']] - data_for_paper[['x10_1']])
data_for_paper[['x10_4']] = pvalue_to_str(pvalues_temp['concern_unhealthy', 'pvalue'])

data_for_paper[['x11_1']] = means_temp['concern_cost_wave1'] * 100
data_for_paper[['x11_2']] = means_temp['concern_cost_wave3'] * 100
data_for_paper[['x11_3']] = abs(data_for_paper[['x11_2']] - data_for_paper[['x11_1']])
data_for_paper[['x11_4']] = pvalue_to_str(pvalues_temp['concern_cost', 'pvalue'])

data_for_paper[['x12_1']] = means_temp['concern_taste_wave1'] * 100
data_for_paper[['x12_2']] = means_temp['concern_taste_wave3'] * 100
data_for_paper[['x12_3']] = abs(data_for_paper[['x12_2']] - data_for_paper[['x12_1']])
data_for_paper[['x12_4']] = pvalue_to_str(pvalues_temp['concern_taste', 'pvalue'])


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

# TODO: finish these analyses.

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

# TODO: finish these analyses.

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


# DISCRETE CHOICE ANLAYSIS
# ------------------------
# Estimates conditional logistic regression coefficients and willingness to 
# pay from discrete choice experiment. Resuls are estimated separately for
# each survey wave and treatment group.
# 
# Requires a dataset in which each row is a single alternative in a choice
# task completed by a single person. So if you have 100 respondents, 5 choices
# per respondent, and 3 alternatives per choice, then your dataset should have
# 100x5x3=1500 rows.
# 
# Example:
# head(df_dce, 10)
#    question alt choice      product cost             MTurkCode wave
# 1         1   1      0        clean    5 A06375872IMBX6SHLVH7V    1
# 2         1   2      0 conventional   20 A06375872IMBX6SHLVH7V    1
# 3         1   3      1   vegetarian   10 A06375872IMBX6SHLVH7V    1
# 4         2   1      0 conventional   20 A06375872IMBX6SHLVH7V    1
# 5         2   2      0   vegetarian   15 A06375872IMBX6SHLVH7V    1
# 6         2   3      1        clean    5 A06375872IMBX6SHLVH7V    1
# 7         3   1      0        clean   20 A06375872IMBX6SHLVH7V    1
# 8         3   2      1   vegetarian    5 A06375872IMBX6SHLVH7V    1
# 9         3   3      0 conventional   10 A06375872IMBX6SHLVH7V    1
# 10        4   1      1 conventional    5 A06375872IMBX6SHLVH7V    1
#    treat_article treat_social
# 1              2            1
# 2              2            1
# 3              2            1
# 4              2            1
# 5              2            1
# 6              2            1
# 7              2            1
# 8              2            1
# 9              2            1
# 10             2            1
#
# The group_by code below is doing more or less the following for each 
# combination of survey wave and treat_article value:
# df_temp = df_dce[df_dce$wave == 2,]
# df_temp = df_dce[df_dce2$treat_article == 0 ,]
# mod = clogit(choice ~ product + cost + strata(MTurkCode, question), data=df_temp)
# coefs = tidy(summary(mod)$conf.int)
# wtp = mwtp(mod, monetary.variables=c("cost"), confidence.level=0.90, method="delta")
# 
# note: for simple linear model, replace lines 3 and 4 with:
# mod = lm(choice ~ product + cost, data=df_temp)
# coefs = tidy(coeftest(mod0, cluster=temp_data$MTurkCode))

df_dce$product = factor(df_dce$product, levels=c('conventional', 'clean', 'vegetarian'))
# df_dce$cost = df_dce$cost / 5

# function: get_clogit_effects
# description: wrapper to clogit, returns dataframe of model coefficients (in 
# odds ratios, along with confidence intervals.
# 
# note: applying as.matrix() before returning is a hack for making 
# dce_effects_article %>% tidy(coefs) work as intended later on.
get_clogit_effects = function(data) {
    mod = clogit(choice ~ product + cost + strata(MTurkCode, question), data=data)
    coefs = summary(mod)$conf.int
    colnames(coefs) = str_replace_all(colnames(coefs), '[ \\(\\)-\\.]', '')
    colnames(coefs)[2] = paste0(colnames(coefs)[2], '_neg')  # 2nd column is exp(-coef), rather than exp(coef)
    return(as.matrix(coefs))
}

# function: get_wtp
# description: wrapper to mwtp, computing the marginal willingness to pay,
# with 90% and 95% confidence intervals.
get_wtp = function(data) {
    mod = clogit(choice ~ product + cost + strata(MTurkCode, question), data=data)
    wtp90 = mwtp(mod, monetary.variables=c("cost"), confidence.level=0.90, method="delta")
    wtp95 = mwtp(mod, monetary.variables=c("cost"), confidence.level=0.95, method="delta")
    wtp = cbind(wtp90$mwtp.table, wtp95$mwtp.table[,-1])
    colnames(wtp)[-1] = paste0('ci', colnames(wtp)[-1])
    colnames(wtp) = str_replace_all(colnames(wtp), '%', '')
    return(wtp)
}

# function: postclean_clogt
# description: cleans up the results from the use of get_clogit_effects and
# get_wtp after being applied in group_by.
postclean_clogit = function(data) {
    # renames '.rowname' column to 'term'
    data = rename(data, term=.rownames)
    # recodes variables and converts to factors.
    data$treat = to_title(data$treat)
    data$term = recode(data$term, 'productclean'='Clean meatballs', 'productvegetarian'='Vegetarian meatballs', 'cost'='$1 higher cost')
    data$term = factor(data$term, levels=c('Clean meatballs', 'Vegetarian meatballs', '$1 higher cost'))
    data$wave = paste0('Wave ', data$wave)
    return(data)
}

# marginal effects and wtp for article appeals.
dce_effects_article = df_dce %>%
    group_by(wave, treat_article) %>%
    do(n=len(unique(.$MTurkCode)),
       coefs=get_clogit_effects(.),
       wtp=get_wtp(.)
    )

# marginal effects and wtp for social information treatment.
dce_effects_social = df_dce %>%
    group_by(wave, treat_social) %>%
    do(n=len(unique(.$MTurkCode)),
       coefs=get_clogit_effects(.),
       wtp=get_wtp(.)
    )

# renames treatment variable.
dce_effects_article = rename(dce_effects_article, treat=treat_article)
dce_effects_social = rename(dce_effects_social, treat=treat_social)

# dce_effects_article$treat = factor(dce_effects_article$treat, rev(unique(dce_effects_article$treat)))
# dce_effects_social$treat = factor(dce_effects_social$treat, rev(unique(dce_effects_social$treat)))

# binds/expands rows together.
dce_effects_article_coefs = dce_effects_article %>% tidy("coefs")
dce_effects_article_wtp = dce_effects_article %>% tidy("wtp")
dce_effects_social_coefs = dce_effects_social %>% tidy("coefs")
dce_effects_social_wtp = dce_effects_social %>% tidy("wtp")

# extracts specific results for paper.
# appeal effects.
data_for_paper[["x13_0"]] = dce_effects_article_wtp %>% ungroup() %>% filter(wave==3, treat==appeal_names[1], .rownames=='productclean') %>% mutate(mwtp_abs=abs(MWTP)) %>% select(mwtp_abs) %>% as.numeric(.)
data_for_paper[["x13_1"]] = dce_effects_article_wtp %>% ungroup() %>% filter(wave==3, treat==appeal_names[2], .rownames=='productclean') %>% mutate(mwtp_abs=abs(MWTP)) %>% select(mwtp_abs) %>% as.numeric(.)
data_for_paper[["x13_2"]] = dce_effects_article_wtp %>% ungroup() %>% filter(wave==3, treat==appeal_names[3], .rownames=='productclean') %>% mutate(mwtp_abs=abs(MWTP)) %>% select(mwtp_abs) %>% as.numeric(.)
data_for_paper[["x13_3"]] = dce_effects_article_wtp %>% ungroup() %>% filter(wave==3, treat==appeal_names[4], .rownames=='productclean') %>% mutate(mwtp_abs=abs(MWTP)) %>% select(mwtp_abs) %>% as.numeric(.)

data_for_paper[["x13_0_0"]] = dce_effects_article_wtp %>% ungroup() %>% filter(wave==3, treat==appeal_names[1], .rownames=='productclean') %>% mutate(ci_abs=abs(ci2.5)) %>% select(ci_abs) %>% as.numeric(.)
data_for_paper[["x13_0_1"]] = dce_effects_article_wtp %>% ungroup() %>% filter(wave==3, treat==appeal_names[1], .rownames=='productclean') %>% mutate(ci_abs=abs(ci97.5)) %>% select(ci_abs) %>% as.numeric(.)
data_for_paper[["x13_1_0"]] = dce_effects_article_wtp %>% ungroup() %>% filter(wave==3, treat==appeal_names[2], .rownames=='productclean') %>% mutate(ci_abs=abs(ci2.5)) %>% select(ci_abs) %>% as.numeric(.)
data_for_paper[["x13_1_1"]] = dce_effects_article_wtp %>% ungroup() %>% filter(wave==3, treat==appeal_names[2], .rownames=='productclean') %>% mutate(ci_abs=abs(ci97.5)) %>% select(ci_abs) %>% as.numeric(.)
data_for_paper[["x13_2_0"]] = dce_effects_article_wtp %>% ungroup() %>% filter(wave==3, treat==appeal_names[3], .rownames=='productclean') %>% mutate(ci_abs=abs(ci2.5)) %>% select(ci_abs) %>% as.numeric(.)
data_for_paper[["x13_2_1"]] = dce_effects_article_wtp %>% ungroup() %>% filter(wave==3, treat==appeal_names[3], .rownames=='productclean') %>% mutate(ci_abs=abs(ci97.5)) %>% select(ci_abs) %>% as.numeric(.)
data_for_paper[["x13_3_0"]] = dce_effects_article_wtp %>% ungroup() %>% filter(wave==3, treat==appeal_names[4], .rownames=='productclean') %>% mutate(ci_abs=abs(ci2.5)) %>% select(ci_abs) %>% as.numeric(.)
data_for_paper[["x13_3_1"]] = dce_effects_article_wtp %>% ungroup() %>% filter(wave==3, treat==appeal_names[4], .rownames=='productclean') %>% mutate(ci_abs=abs(ci97.5)) %>% select(ci_abs) %>% as.numeric(.)

# social information effects.
data_for_paper[["x13_0_s2"]] = dce_effects_social_wtp %>% ungroup() %>% filter(wave==2, treat==social_names[1], .rownames=='productclean') %>% mutate(mwtp_abs=abs(MWTP)) %>% select(mwtp_abs) %>% as.numeric(.)
data_for_paper[["x13_1_s2"]] = dce_effects_social_wtp %>% ungroup() %>% filter(wave==2, treat==social_names[2], .rownames=='productclean') %>% mutate(mwtp_abs=abs(MWTP)) %>% select(mwtp_abs) %>% as.numeric(.)

data_for_paper[["x13_0_0_s2"]] = dce_effects_social_wtp %>% ungroup() %>% filter(wave==2, treat==social_names[1], .rownames=='productclean') %>% mutate(ci_abs=abs(ci2.5)) %>% select(ci_abs) %>% as.numeric(.)
data_for_paper[["x13_0_1_s2"]] = dce_effects_social_wtp %>% ungroup() %>% filter(wave==2, treat==social_names[1], .rownames=='productclean') %>% mutate(ci_abs=abs(ci97.5)) %>% select(ci_abs) %>% as.numeric(.)
data_for_paper[["x13_1_0_s2"]] = dce_effects_social_wtp %>% ungroup() %>% filter(wave==2, treat==social_names[2], .rownames=='productclean') %>% mutate(ci_abs=abs(ci2.5)) %>% select(ci_abs) %>% as.numeric(.)
data_for_paper[["x13_1_1_s2"]] = dce_effects_social_wtp %>% ungroup() %>% filter(wave==2, treat==social_names[2], .rownames=='productclean') %>% mutate(ci_abs=abs(ci97.5)) %>% select(ci_abs) %>% as.numeric(.)

data_for_paper[["x13_0_s3"]] = dce_effects_social_wtp %>% ungroup() %>% filter(wave==3, treat==social_names[1], .rownames=='productclean') %>% mutate(mwtp_abs=abs(MWTP)) %>% select(mwtp_abs) %>% as.numeric(.)
data_for_paper[["x13_1_s3"]] = dce_effects_social_wtp %>% ungroup() %>% filter(wave==3, treat==social_names[2], .rownames=='productclean') %>% mutate(mwtp_abs=abs(MWTP)) %>% select(mwtp_abs) %>% as.numeric(.)

data_for_paper[["x13_0_0_s3"]] = dce_effects_social_wtp %>% ungroup() %>% filter(wave==3, treat==social_names[1], .rownames=='productclean') %>% mutate(ci_abs=abs(ci2.5)) %>% select(ci_abs) %>% as.numeric(.)
data_for_paper[["x13_0_1_s3"]] = dce_effects_social_wtp %>% ungroup() %>% filter(wave==3, treat==social_names[1], .rownames=='productclean') %>% mutate(ci_abs=abs(ci97.5)) %>% select(ci_abs) %>% as.numeric(.)
data_for_paper[["x13_1_0_s3"]] = dce_effects_social_wtp %>% ungroup() %>% filter(wave==3, treat==social_names[2], .rownames=='productclean') %>% mutate(ci_abs=abs(ci2.5)) %>% select(ci_abs) %>% as.numeric(.)
data_for_paper[["x13_1_1_s3"]] = dce_effects_social_wtp %>% ungroup() %>% filter(wave==3, treat==social_names[2], .rownames=='productclean') %>% mutate(ci_abs=abs(ci97.5)) %>% select(ci_abs) %>% as.numeric(.)

# appeal effects on MWTP for vegetarian meatballs.
# data_for_paper[["x13_0_v"]] = dce_effects_article_wtp %>% ungroup() %>% filter(wave==3, treat==appeal_names[1], .rownames=='productvegetarian') %>% mutate(mwtp_abs=abs(MWTP)) %>% select(mwtp_abs) %>% as.numeric(.)
# data_for_paper[["x13_0_0_v"]] = dce_effects_article_wtp %>% ungroup() %>% filter(wave==3, treat==appeal_names[1], .rownames=='productvegetarian') %>% mutate(ci_abs=abs(ci2.5)) %>% select(ci_abs) %>% as.numeric(.)
# data_for_paper[["x13_0_1_v"]] = dce_effects_article_wtp %>% ungroup() %>% filter(wave==3, treat==appeal_names[1], .rownames=='productvegetarian') %>% mutate(ci_abs=abs(ci97.5)) %>% select(ci_abs) %>% as.numeric(.)

# minor cleaning for plotting.
dce_effects_article_coefs = postclean_clogit(dce_effects_article_coefs)
dce_effects_article_wtp = postclean_clogit(dce_effects_article_wtp)
dce_effects_social_coefs = postclean_clogit(dce_effects_social_coefs)
dce_effects_social_wtp = postclean_clogit(dce_effects_social_wtp)

# plots odds ratios.
# note: x and y are flipped.
p1 = ggplot(dce_effects_article_coefs[dce_effects_article_coefs$wave != "Wave 1",], aes(y=expcoef, x=treat, color=wave)) + 
    geom_hline(yintercept=1.0, linetype='dashed', color='gray30') +
    geom_errorbar(aes(ymin=lower95, ymax=upper95), width=0, size=0.8, position=position_dodge(width=0.7)) +
    labs(y='Odds ratio (vs. conventional meat)', x=NULL) +
    labs(title='Pro-clean meat appeals', size=22) +
    geom_point(size=2.3, position=position_dodge(width=0.7)) + 
    guides(color=guide_legend(title=NULL, reverse=FALSE)) +
    plot_theme +
    coord_flip() + 
    scale_colour_manual(values=c("gray70", "black")) + 
    scale_y_continuous(breaks=round(seq(0.2, 1.0, by=0.2), 1), limits=c(0.35, 1.0)) +
    facet_wrap(~term, scales='free_y', ncol=1, strip.position=c('top')) + 
    theme(strip.background = element_blank(), strip.text=element_text(size=12, face='bold'), panel.spacing=unit(c(0.2,0.2), 'cm'), axis.text.y=element_text(size=11), axis.text.x=element_text(size=11), plot.margin=unit(c(0.5,0,0.5,0), "cm")) +
    guides(color=FALSE)

p2 = ggplot(dce_effects_social_coefs[dce_effects_social_coefs$wave != "Wave 1",], aes(y=expcoef, x=treat, color=wave)) + 
    geom_hline(yintercept=1.0, linetype='dashed', color='gray30') +
    geom_errorbar(aes(ymin=lower95, ymax=upper95), width=0, size=0.8, position=position_dodge(width=0.7)) +
    labs(y='Odds ratio (vs. conventional meat)', x=NULL) +
    labs(title='Anti-clean meat social info.', size=22) +
    geom_point(size=2.3, position=position_dodge(width=0.7)) + 
    guides(color=guide_legend(title=NULL, reverse=FALSE)) +
    plot_theme +
    coord_flip() + 
    scale_colour_manual(values=c("gray70", "black")) + 
    scale_y_continuous(breaks=round(seq(0.2, 1.0, by=0.2), 1), limits=c(0.35, 1.0)) +
    facet_wrap(~term, scales='free_y', ncol=1, strip.position=c('top')) + 
    theme(strip.background = element_blank(), strip.text=element_text(size=12, face='bold'), panel.spacing=unit(c(0.2,0.2), 'cm'), axis.text.y=element_text(size=11), axis.text.x=element_text(size=11), plot.margin=unit(c(0.5,0,0.5,0), "cm")) +
    guides(color=guide_legend(title=NULL, size=3))

legend = get_legend(p2)
p2 = p2 + guides(color=FALSE)

fname = 'dce_effects_or.png'
lay = rbind(
    c(1,2),
    c(3,3)
)

ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), grid.arrange(p1, p2, legend, heights=c(7, 1), widths=c(6, 6), layout_matrix=lay), units="cm", dpi=dpi, height=height+1, width=width+14)


# plots wtp.
# note: x and y are flipped.
p3 = ggplot(dce_effects_article_wtp[dce_effects_article_wtp$wave != "Wave 1",], aes(y=MWTP, x=treat, color=wave)) + 
    geom_hline(yintercept=0, linetype='dashed', color='gray30') +
    geom_errorbar(aes(ymin=ci5, ymax=ci95), width=0, size=0.8, position=position_dodge(width=0.7)) +
    geom_errorbar(aes(ymin=ci2.5, ymax=ci97.5), width=0, size=0.3, position=position_dodge(width=0.7)) +
    labs(y='Marginal WTP ($)', x=NULL) +
    labs(title='Pro-clean meat appeals', size=22) +
    geom_point(size=2.3, position=position_dodge(width=0.7)) + 
    guides(color=guide_legend(title=NULL, reverse=FALSE)) +
    plot_theme +
    coord_flip() + 
    scale_colour_manual(values=c("gray70", "black")) + 
    scale_y_continuous(breaks=round(seq(-10, 0, by=2), 1), limits=c(-9, 0)) +
    # ylim(c(-0.5, 0.5)) +
    facet_wrap(~term, scales='free_y', ncol=1, strip.position=c('top')) + 
    theme(strip.background = element_blank(), strip.text=element_text(size=12, face='bold'), axis.text.y=element_text(size=11), axis.text.x=element_text(size=11), plot.margin=unit(c(0.5,0,0.5,0), "cm")) +
    guides(color=FALSE)

p4 = ggplot(dce_effects_social_wtp[dce_effects_social_wtp$wave != "Wave 1",], aes(y=MWTP, x=treat, color=wave)) + 
    geom_hline(yintercept=0, linetype='dashed', color='gray30') +
    geom_errorbar(aes(ymin=ci5, ymax=ci95), width=0, size=0.8, position=position_dodge(width=0.7)) +
    geom_errorbar(aes(ymin=ci2.5, ymax=ci97.5), width=0, size=0.3, position=position_dodge(width=0.7)) +
    labs(y='Marginal WTP ($)', x=NULL) +
    labs(title='Anti-clean meat social info.', size=22) +
    geom_point(size=2.3, position=position_dodge(width=0.7)) + 
    guides(color=guide_legend(title=NULL, reverse=FALSE)) +
    plot_theme +
    coord_flip() + 
    scale_colour_manual(values=c("gray70", "black")) + 
    scale_y_continuous(breaks=round(seq(-10, 0, by=2), 1), limits=c(-9, 0)) +
    # ylim(c(-0.5, 0.5)) +
    facet_wrap(~term, scales='free_y', ncol=1, strip.position=c('top')) + 
    theme(strip.background = element_blank(), strip.text=element_text(size=12, face='bold'), axis.text.y=element_text(size=11), axis.text.x=element_text(size=11), plot.margin=unit(c(0.5,0,0.5,0), "cm")) +
    guides(color=guide_legend(title=NULL, size=3))

legend = get_legend(p4)
p4 = p4 + guides(color=FALSE)

fname = 'dce_effects_wtp.png'
lay = rbind(
    c(1,2),
    c(3,3)
)

ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), grid.arrange(p3, p4, legend, heights=c(7, 1), widths=c(6, 6), layout_matrix=lay), units="cm", dpi=dpi, height=height, width=width+12)



# EFFECTS OF APPEALS AND SOCIAL INFORMATION ON INTEREST IN CLEAN MEAT
# -------------------------------------------------------------------
# this section examines the treatment effects of the pro-clean meat appeals
# and the anti-adoption social information.

# experimental cell means
# suffix = 'chg_wave1_wave3'
this_outcomes = c('would_eat_chg_wave1_wave3', 'interest_purchase_chg_wave1_wave3')
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
vars_to_use = c(interest_vars, concern_vars, benefit_vars)  # ease_vars, meat_attitude_vars

# constructs vector of outcomes.
suffix2 = 'chg_wave1_wave2'
suffix2_std = paste0(suffix2, '_std')
suffix3 = 'chg_wave1_wave3'
suffix3_std = paste0(suffix3, '_std')
temp_outcomes2 = sapply(vars_to_use, FUN=function(x) ifelse(paste(x, suffix2_std, sep='_') %in% colnames(df), paste(x, suffix2_std, sep='_'), paste(x, suffix2, sep='_')))
temp_outcomes3 = sapply(vars_to_use, FUN=function(x) ifelse(paste(x, suffix3_std, sep='_') %in% colnames(df), paste(x, suffix3_std, sep='_'), paste(x, suffix3, sep='_')))
temp_outcomes = c(temp_outcomes2, temp_outcomes3)
waves = c(rep('Wave 2', len(temp_outcomes2)), rep('Wave 3', len(temp_outcomes3)))
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

# extracts specific results for paper.
temp_effect = effects %>% ungroup() %>% filter(wave=='Wave 2', term==appeal_names[2], outcome=='concern_unnatural_chg_wave1_wave2') %>% mutate(estimate_abs=abs(estimate)) %>% select(estimate_abs, std.error, p.value) %>% as.numeric(.)
data_for_paper[["x14_0"]] = temp_effect[1] * 100  # estimate
data_for_paper[["x14_1"]] = temp_effect[2] * 100 # SE
data_for_paper[["x14_2"]] = pvalue_to_str(temp_effect[3])  # pvalue

# cleans up text for plotting.
temp_regex = paste(paste0('_', c(suffix2, suffix2_std), '$', collapse='|'), paste0('_', c(suffix3, suffix3_std), '$', collapse='|'), sep='|')
effects$outcome = str_replace(effects$outcome, temp_regex, '')
effects$outcome_group = to_title(ifelse(effects$outcome %in% interest_vars, 'interest',
    ifelse(effects$outcome %in% concern_vars, 'concern',
    ifelse(effects$outcome %in% benefit_vars, 'benefit', 'other'))))
effects$outcome_group = factor(effects$outcome_group)
effects$outcome = to_title(effects$outcome)
effects$outcome = str_replace(effects$outcome, '^Concern |^Benefit ', '')
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
        scale_y_continuous(breaks=round(seq(-0.6, 0.6, by=0.2), 1), limits=c(-0.5, 0.5)) +
        # ylim(c(-0.5, 0.5)) +
        facet_wrap(~outcome_group, scales='free_y', ncol=1, strip.position=c('left')) + 
        theme(strip.background = element_rect(fill="gray90", color="gray90"), strip.text.y = element_text(angle=180, face="bold", hjust=0), plot.margin=unit(c(0.1,0,0.1,0), "cm"), ) +
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
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), grid.arrange(grobs=grobs, layout_matrix=lay, heights=c(8,8,8,8,1), widths=c(6)), width=width, height=height+26, units="cm", dpi=dpi)


# INTERACTIONS BETWEEN TREATMENTS
# -------------------------------
# in this section, we examine whether any of the appeals were significantly
# more/less effective when preceded by the negative social information than
# when not.

suffix = 'chg_wave1_wave3'
suffix_std = paste0(suffix, '_std')
temp_outcomes = sapply(vars_to_use, FUN=function(x) ifelse(paste(x, suffix_std, sep='_') %in% colnames(df), paste(x, suffix_std, sep='_'), paste(x, suffix, sep='_')))

moderator = 'treat_social'

# constructs coefficient tables.
coef_tables = lapply(temp_outcomes, FUN=function(y) get_coef_tables_interact(df, y, treatments[1], moderator))
names(coef_tables) = temp_outcomes
coef_table = do.call(rbind, coef_tables)

# cleans up outcome text.
temp_regex = paste0('_', suffix, '\\.[0-9]+$|_', suffix_std, '\\.[0-9]+$')
coef_table$outcome = str_replace(rownames(coef_table), temp_regex, '')
rownames(coef_table) = NULL

# extracts specific results for paper.
temp_effect = coef_table %>% filter(treatment==appeal_names[2], moderator==social_names[1], outcome=='concern_unnatural') %>% mutate(estimate_abs=abs(estimate)) %>% select(estimate_abs, se, n) %>% as.numeric(.)
# # ttest_coef_diff(temp_effect$estimate[2], temp_effect$estimate[1], temp_effect$se[2], temp_effect$se[1], df=temp_effect$n[1])
data_for_paper[["x15_0"]] = temp_effect[1] * 100  # estimate
data_for_paper[["x15_1"]] = pvalue_to_str(get_pvalue(temp_effect[1] / temp_effect[2], temp_effect[3]-1))  # pvalue

temp_effect = coef_table %>% filter(treatment==appeal_names[2], moderator==social_names[2], outcome=='concern_unnatural') %>% mutate(estimate_abs=abs(estimate)) %>% select(estimate_abs, se, n) %>% as.numeric(.)
# # ttest_coef_diff(temp_effect$estimate[2], temp_effect$estimate[1], temp_effect$se[2], temp_effect$se[1], df=temp_effect$n[1])
data_for_paper[["x16_0"]] = temp_effect[1] * 100  # estimate
data_for_paper[["x16_1"]] = get_pvalue(temp_effect[1] / temp_effect[2], temp_effect[3]-1)  # pvalue

# cleans up text for plotting.
coef_table$outcome_group = to_title(ifelse(coef_table$outcome %in% interest_vars, 'interest',
    ifelse(coef_table$outcome %in% concern_vars, 'concern', 
    ifelse(coef_table$outcome %in% benefit_vars, 'benefit', 'other'))))
coef_table$outcome_group = factor(coef_table$outcome_group)
coef_table$outcome = to_title(coef_table$outcome)
coef_table$outcome = str_replace(coef_table$outcome, '^Concern |^Benefit ', '')
coef_table$outcome = str_replace(coef_table$outcome, 'Interest Purchase', 'Int. Purch.')

coef_table$treatment = paste0('Effects of "', to_title(coef_table$treatment), '"')
coef_table$moderator = to_title(coef_table$moderator)

plots = coef_table %>%
    group_by(treatment) %>%
    do(plot=gg_coefplot(., color="moderator") +
        scale_colour_manual(values=c("black", "gray70")) + 
        labs(title=.$treatment[1], y="", size=18) +
        scale_y_continuous(breaks=round(seq(-0.6, 0.6, by=0.2), 2), limits=c(-0.62, 0.62)) +
        # ylim(c(-0.5, 0.5)) +
        facet_wrap(~outcome_group, scales='free_y', ncol=1, strip.position=c('left')) + 
        theme(strip.background = element_rect(fill="gray90", color="gray90"), strip.text.y = element_text(size=10, angle=180, face="bold", hjust=0), plot.margin=unit(c(0.5,0,0.5,0), "cm"), ) +
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
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), grid.arrange(grobs=grobs, layout_matrix=lay, heights=c(8,8,8,1), widths=c(6)), width=width, height=height+22, units="cm", dpi=dpi)


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

suffix = 'chg_wave1_wave3_std'
temp_outcomes = paste(interest_vars, suffix, sep='_')

# FEEL
df[,'feel_wave1_cut'] = cut(df[,'feel_wave1'], breaks=c(1,4,5,8), include.lowest=TRUE, right=FALSE, labels=c('low', 'neutral', 'high'))
# tab(df[,'feel_wave1'], df[,'feel_wave1_cut'])
moderator1 = 'feel_wave1_cut'

coef_tables = lapply(temp_outcomes, FUN=function(y) get_coef_tables_interact(df, y, treatments[1], moderator1))
names(coef_tables) = temp_outcomes
coef_table = do.call(rbind, coef_tables)
coef_table$y = str_replace(rownames(coef_table), paste0('_', suffix, '\\.[0-9]+$'), '')
rownames(coef_table) = NULL
coef_table$moderator = factor(coef_table$moderator, levels=levels(df[,moderator1]))

coef_tables_social = lapply(temp_outcomes, FUN=function(y) get_coef_tables_interact(df, y, treatments[2], moderator1))
names(coef_tables_social) = temp_outcomes
coef_table_social = do.call(rbind, coef_tables_social)
coef_table_social$y = str_replace(rownames(coef_table_social), paste0('_', suffix, '\\.[0-9]+$'), '')
rownames(coef_table_social) = NULL
coef_table_social$moderator = factor(coef_table_social$moderator, levels=levels(df[,moderator1]))

coef_table1 = bind_rows(coef_table, coef_table_social)

# extracts specific results for paper.
# ...

# INTEREST_PURCHASE
df[,'interest_purchase_wave1_cut'] = cut(df[,'interest_purchase_wave1'], breaks=c(1,3,4,6), include.lowest=TRUE, right=FALSE, labels=c('low', 'neutral', 'high'))
# tab(df[,'interest_purchase_wave1'], df[,'interest_purchase_wave1_cut'])
moderator2 = 'interest_purchase_wave1_cut'

coef_tables = lapply(temp_outcomes, FUN=function(y) get_coef_tables_interact(df, y, treatments[1], moderator2))
names(coef_tables) = temp_outcomes
coef_table = do.call(rbind, coef_tables)
coef_table$y = str_replace(rownames(coef_table), paste0('_', suffix, '\\.[0-9]+$'), '')
rownames(coef_table) = NULL
coef_table$moderator = factor(coef_table$moderator, levels=levels(df[,moderator2]))

coef_tables_social = lapply(temp_outcomes, FUN=function(y) get_coef_tables_interact(df, y, treatments[2], moderator2))
names(coef_tables_social) = temp_outcomes
coef_table_social = do.call(rbind, coef_tables_social)
coef_table_social$y = str_replace(rownames(coef_table_social), paste0('_', suffix, '\\.[0-9]+$'), '')
rownames(coef_table_social) = NULL
coef_table_social$moderator = factor(coef_table_social$moderator, levels=levels(df[,moderator2]))

coef_table2 = bind_rows(coef_table, coef_table_social)


# WOULD_EAT
df[,'would_eat_wave1_cut'] = cut(df[,'would_eat_wave1'], breaks=c(1,3,4,6), include.lowest=TRUE, right=FALSE, labels=c('low', 'neutral', 'high'))
# tab(df[,'would_eat_wave1'], df[,'would_eat_wave1_cut'])
moderator3 = 'would_eat_wave1_cut'

coef_tables = lapply(temp_outcomes, FUN=function(y) get_coef_tables_interact(df, y, treatments[1], moderator3))
names(coef_tables) = temp_outcomes
coef_table = do.call(rbind, coef_tables)
coef_table$y = str_replace(rownames(coef_table), paste0('_', suffix, '\\.[0-9]+$'), '')
rownames(coef_table) = NULL
coef_table$moderator = factor(coef_table$moderator, levels=levels(df[,moderator3]))

coef_tables_social = lapply(temp_outcomes, FUN=function(y) get_coef_tables_interact(df, y, treatments[2], moderator3))
names(coef_tables_social) = temp_outcomes
coef_table_social = do.call(rbind, coef_tables_social)
coef_table_social$y = str_replace(rownames(coef_table_social), paste0('_', suffix, '\\.[0-9]+$'), '')
rownames(coef_table_social) = NULL
coef_table_social$moderator = factor(coef_table_social$moderator, levels=levels(df[,moderator3]))

coef_table3 = bind_rows(coef_table, coef_table_social)


# cleans up text for plotting.
coef_table1$y = to_title(coef_table1$y)
coef_table2$y = to_title(coef_table2$y)
coef_table3$y = to_title(coef_table3$y)
coef_table1$treatment = to_title(coef_table1$treatment)
coef_table2$treatment = to_title(coef_table2$treatment)
coef_table3$treatment = to_title(coef_table3$treatment)

# plots the results.
p1 = gg_subgroup_coefplot(coef_table1, x='estimate', y='moderator', color='y') + guides(color=guide_legend(reverse=TRUE, title='Outcome: ')) +
    facet_wrap(~treatment, ncol=1) + 
    labs(title=paste0('Effects by baseline\n"', to_title(str_replace(moderator1, '_wave1_cut$', '')), '"'), y='ATE', x='Baseline (wave 1) value')
p2 = gg_subgroup_coefplot(coef_table2, x='estimate', y='moderator', color='y') +
    facet_wrap(~treatment, ncol=1) + 
    labs(title=paste0('Effects by baseline\n"', to_title(str_replace(moderator2, '_wave1_cut$', '')), '"'), y='ATE', x=NULL)
p3 = gg_subgroup_coefplot(coef_table3, x='estimate', y='moderator', color='y') +
    facet_wrap(~treatment, ncol=1) + 
    labs(title=paste0('Effects by baseline\n"', to_title(str_replace(moderator3, '_wave1_cut$', '')), '"'), y='ATE', x=NULL)
legend = get_legend(p1)

add_this_plot_theme = function(p) p + theme(
    strip.background=element_blank(), 
    plot.title=element_text(size=12),
    strip.text.x = element_text(size=11)
  ) + 
  scale_y_continuous(breaks=round(seq(-0.8, 0.8, by=0.4), 2), limits=c(-0.8, 0.8)) +
  guides(color=FALSE)
# p = p + labs(title=to_title(y)) + guides(color=guide_legend(title=paste0(to_title(moderator),': '), reverse=FALSE))
p1 = add_this_plot_theme(p1)
p2 = add_this_plot_theme(p2) + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
p3 = add_this_plot_theme(p3) + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

fname = paste0('subgroup_effects_interest_', suffix, '.png')
lay = rbind(
    c(1,2,3),
    c(4,4,4)
)
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), grid.arrange(p1, p2, p3, legend, heights=c(9,1), widths=c(10,9,9), layout_matrix=lay), width=width+5, units="cm", dpi=dpi)


# HETEROGENEOUS EFFECTS BY BASELINE MEAT CONSUMPTION
# --------------------------------------------------
# Answer to the question: are effects concentrated among those who already eat
# very little meat?

suffix = 'chg_wave1_wave3_std'
temp_outcomes = paste(interest_vars, suffix, sep='_')

# df[,'ffq_total_sum_meat_cut'] = cut(df[,'ffq_total_sum_meat_1'], breaks=quantile(df[,'ffq_total_sum_meat_1'], c(0.0, 0.25, 0.5, 0.75, 1.0), na.rm=TRUE), include.lowest=TRUE, right=TRUE, labels=c('Bottom quartile', '2nd quartile', '3rd quartile', 'Top quartile'))
df[,'ffq_total_sum_meat_cut'] = cut(df[,'ffq_total_sum_meat_wave1'], breaks=quantile(df[,'ffq_total_sum_meat_wave1'], c(0.0, 0.33, 0.66, 1.0), na.rm=TRUE), include.lowest=TRUE, right=TRUE, labels=c('Bottom third', 'Middle third', 'Top third'))
df[,'ffq_total_sum_meat_cut'] = factor(df[,'ffq_total_sum_meat_cut'], levels=rev(levels(df[,'ffq_total_sum_meat_cut'])))
# quantile(df[,'ffq_total_sum_meat_wave1'], c(0.0, 0.25, 0.5, 0.75, 1.0), na.rm=TRUE)
# quantile(df[,'ffq_total_sum_meat_wave1'], c(0.0, 0.33, 0.66, 1.0), na.rm=TRUE)
# tab(df[,'ffq_total_sum_meat_wave1'], df[,'ffq_total_sum_meat_cut'])
# tab(df[,'ffq_total_sum_meat_cut'])
moderator = 'ffq_total_sum_meat_cut'

coef_tables = lapply(temp_outcomes, FUN=function(y) get_coef_tables_interact(df, y, treatments[1], moderator))
names(coef_tables) = temp_outcomes
coef_table = do.call(rbind, coef_tables)
coef_table$y = str_replace(rownames(coef_table), paste0('_', suffix, '\\.[0-9]+$'), '')
rownames(coef_table) = NULL
coef_table$moderator = factor(coef_table$moderator, levels=levels(df[,moderator]))

coef_tables_social = lapply(temp_outcomes, FUN=function(y) get_coef_tables_interact(df, y, treatments[2], moderator))
names(coef_tables_social) = temp_outcomes
coef_table_social = do.call(rbind, coef_tables_social)
coef_table_social$y = str_replace(rownames(coef_table_social), paste0('_', suffix, '\\.[0-9]+$'), '')
rownames(coef_table_social) = NULL
coef_table_social$moderator = factor(coef_table_social$moderator, levels=levels(df[,moderator]))

coef_table = bind_rows(coef_table, coef_table_social)

# cleans up text for plotting
coef_table$y = to_title(coef_table$y)
coef_table$treatment = paste0('Effects of "', to_title(coef_table$treatment), '"')

# plots the results.
p = gg_subgroup_coefplot(coef_table, x='estimate', y='moderator', color='y')
p = p + facet_wrap(~treatment, ncol=1) + 
    theme(strip.background = element_blank()) + 
    labs(x='Baseline servings of meat per week', y='ATE', title=NULL) + 
    guides(color=guide_legend(reverse=TRUE, title='Outcome: ')) + 
    scale_y_continuous(breaks=round(seq(-0.8, 0.8, by=0.4), 2), limits=c(-0.8, 0.8))
fname = paste0('subgroup_effects_meat_', suffix, '.png')
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width, height=height+5, units="cm", dpi=dpi)


# HETEROGENEOUS EFFECTS BY BASELINE CONCERN UNNATURAL
# ---------------------------------------------------
suffix = 'chg_wave1_wave3'
temp_outcomes = paste(interest_vars, suffix, sep='_')

moderator = 'concern_unnatural_wave1'
tab(df[,moderator], exclude=NULL)
df[, moderator] = factor(df[, moderator])

coef_tables = lapply(temp_outcomes, FUN=function(y) get_coef_tables_interact(df, y, treatments[1], moderator))
names(coef_tables) = temp_outcomes
coef_table = do.call(rbind, coef_tables)
coef_table$y = str_replace(rownames(coef_table), paste0('_', suffix, '\\.[0-9]+$'), '')
rownames(coef_table) = NULL
coef_table$moderator = factor(coef_table$moderator, levels=levels(df[,moderator]))

coef_tables_social = lapply(temp_outcomes, FUN=function(y) get_coef_tables_interact(df, y, treatments[2], moderator))
names(coef_tables_social) = temp_outcomes
coef_table_social = do.call(rbind, coef_tables_social)
coef_table_social$y = str_replace(rownames(coef_table_social), paste0('_', suffix, '\\.[0-9]+$'), '')
rownames(coef_table_social) = NULL
coef_table_social$moderator = factor(coef_table_social$moderator, levels=levels(df[,moderator]))

coef_table = bind_rows(coef_table, coef_table_social)

# cleans up text for plotting
coef_table$y = to_title(coef_table$y)
coef_table$moderator = recode(coef_table$moderator, '0'='Not concerned', '1'='Concerned')
coef_table$treatment = paste0('Effects of "', to_title(coef_table$treatment), '"')

p = gg_subgroup_coefplot(coef_table, x='estimate', y='moderator', color='y')
p = p + facet_wrap(~treatment, ncol=1) + 
    theme(strip.background = element_blank()) + 
    labs(x=sprintf('Baseline %s', to_title(str_replace(moderator, '_wave1', ''))), y='ATE', title=NULL) + 
    guides(color=guide_legend(reverse=TRUE, title='Outcome: ')) + 
    scale_y_continuous(breaks=round(seq(-0.8, 0.8, by=0.4), 2), limits=c(-0.8, 0.8))
fname = paste0('subgroup_effects_', moderator, '_', suffix, '.png')
ggsave(filename=paste(fig_output_dir, '/', fname, sep=''), p, width=width+3, height=height+5, units="cm", dpi=dpi)


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


# --------------------------------- #
# Function: gg_coefplot
# ---------------------------
# This function creates a coefficient plot showing treatment effects.
#
# --------------------------------- #
gg_coefplot <- function(data_to_plot, ...) {
    p <- ggplot(data_to_plot, aes_string(y="estimate", x="outcome", ...)) + 
        geom_hline(yintercept=0, linetype='dashed', color='gray30') +
        geom_errorbar(aes(ymin=lower_ci90, ymax=upper_ci90), width=0, size=0.8, position=position_dodge(width=0.7)) +
        geom_errorbar(aes(ymin=lower_ci95, ymax=upper_ci95), width=0, size=0.3, position=position_dodge(width=0.7)) +
        labs(y='ATE', x=NULL) +
        geom_point(size=2.3, position=position_dodge(width=0.7)) + 
        # geom_line(size=1, position=position_dodge(width=0.1)) + 
        # facet_wrap(~color, ncol=3, scales='fixed') + 
        guides(color=guide_legend(title=NULL, reverse=FALSE)) +
        plot_theme +
        coord_flip()
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
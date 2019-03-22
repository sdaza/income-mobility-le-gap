##############################d
# descriptive plots and tables
# author: sebastian daza
##############################

library(data.table)
library(ggplot2)
library(ggthemes)
library(sdazar)
library(texreg)
library(xtable)

# read data
df = readRDS('related_projects/health_inequality_project/data/le_cov_sel.rds')
ncounties = length(unique(df$county))

male = df[gender=='M' & income_q %in% c('Q1', 'Q4')]

savepdf('related_projects/health_inequality_project/output/plots/sc_male', 15, 12)
print(ggplot(male, aes(x=(relative_mob*-1)/100, y=le, group=income_q)) +
    geom_point(aes(color=income_q), alpha=0.3) +
    geom_smooth(aes(color=income_q), alpha=0.3,  se = FALSE) +
    theme_classic() +
    theme(text = element_text(size=14), legend.position = "top", legend.title=element_blank()) +
    labs(x='\nRelative income mobility', y='Life expectancy at age 40\n'))
dev.off()

female = df[gender=='F' & income_q %in% c('Q1', 'Q4')]

savepdf('related_projects/health_inequality_project/output/plots/sc_female', 15, 12)
print(ggplot(female, aes(x=(relative_mob*-1)/100, y=le, group=income_q)) +
    geom_point(aes(color=income_q), alpha=0.3) +
    geom_smooth(aes(color=income_q), alpha=0.3,  se = FALSE) +
    theme_classic() +
    theme(text = element_text(size=14), legend.position = "top", legend.title=element_blank()) +
    labs(x='\nRelative income mobility', y='Life expectancy at age 40\n'))
dev.off()

length(unique(df$county))

# descriptive table

df[ , quartile_relative_mob  := cut(z_relative_mob*-1,
                        breaks = quantile(z_relative_mob, probs = 0:4/4),
                        labels = 1:4, right = FALSE, include.lowest = TRUE)]

# life expectancy
total_le = df[income_q %in% c('Q1', 'Q4'),
              .(tm = mean(le), tsd = sd(le)), by = .(income_q, gender)][
              , variable := paste0(gender, '-', income_q)][
              , .(variable, tm, tsd)]


mq1_le =  df[income_q %in% c('Q1', 'Q4') & quartile_relative_mob == 1,
             .(q1m = mean(le), q1sd = sd(le)), by = .(income_q, gender)][
              , variable := paste0(gender, '-', income_q)][
              , .(variable, q1m, q1sd)]

mq4_le =  df[income_q %in% c('Q1', 'Q4') & quartile_relative_mob == 4,
             .(q4m = mean(le), q4sd = sd(le)), by = .(income_q, gender)][
              , variable := paste0(gender, '-', income_q)][
              , .(variable, q4m, q4sd)]

tab_le = merge(merge(total_le, mq1_le, on='variable'), mq4_le, on='variable')

setorder(tab_le, variable)


# covariates

df[, adjust_relative_mob := (relative_mob * -1)/100]

names(df)

vars = c('adjust_relative_mob', 'gini', 'income', 'population', 'pct_black',
         'pct_hispanic', 'segregation_income', 'unemployment',
         'uninsured', 'medicare_expenses')

cov_summary = function(x) list(mean = mean(x, na.rm=TRUE),
                               median = sd(x, na.rm=TRUE))

total_cov = df[income_q == 'Q1' & gender == 'M',
            lapply(.SD, cov_summary),
            .SDcols = vars]
total_cov = data.table(t(total_cov), keep.rownames=TRUE)
setnames(total_cov, names(total_cov), c('variable', 'tm', 'tsd'))

mq1_cov = df[income_q == 'Q1' & gender == 'M' & quartile_relative_mob == 1,
            lapply(.SD, cov_summary),
            .SDcols = vars]
mq1_cov = data.table(t(mq1_cov), keep.rownames=TRUE)
setnames(mq1_cov, names(mq1_cov), c('variable', 'q1m', 'q1sd'))

mq4_cov = df[income_q == 'Q1' & gender == 'M' & quartile_relative_mob == 4,
            lapply(.SD, cov_summary),
            .SDcols = vars]
mq4_cov = data.table(t(mq4_cov), keep.rownames=TRUE)
setnames(mq4_cov, names(mq4_cov), c('variable', 'q4m', 'q4sd'))

tab_cov = merge(merge(total_cov, mq1_cov, on='variable'), mq4_cov, on='variable')

tab_cov = tab_cov[vars]

# final table

tab = rbind(tab_le, tab_cov)

# rename variables to create latex table
nvars = c("Age 40 LE Poorest Income Quartile, Women",
          "Age 40 LE Richest Income Quartile, Women",
          "Age 40 LE Poorest Income Quartile, Men",
          "Age 40 LE Richest Income Quartile, Men",
          "Relative Income Mobility",
          "Gini Coefficient",
          "Average Household Income",
          "Population size",
           "Percent Afroamerican",
           "Percent Hispanic",
           # "Crime Rate",
           "Income Segregation",
           "Unemployment Rate",
           "Percent Uninsured",
           "Medicare Expenses")

tab[, variable := nvars]

nnames = c('Variable', 'Mean', 'SD', 'Mean', 'SD', 'Mean', 'SD')
setnames(tab, names(tab), nnames)

total_rows = nrow(tab)

# create latex table
addtorow = list()
addtorow$pos = list(-1, 0, 4, total_rows)
addtorow$command <- c("\\hline
\\addlinespace
& \\multicolumn{2}{c}{Full Sample} & \\multicolumn{2}{c}{Lowest Quartile IRM} & \\multicolumn{2}{c}{Highest Quartile IRM}  \\\\
Variable & \\multicolumn{1}{c}{Mean} & \\multicolumn{1}{c}{SD} & \\multicolumn{1}{c}{Mean} & \\multicolumn{1}{c}{SD} & \\multicolumn{1}{c}{Mean} & \\multicolumn{1}{c}{SD} \\\\
\\addlinespace
",
"\\addlinespace
\\multicolumn{7}{l}{\\textit{Outcome}} \\\\
\\addlinespace
",
"\\addlinespace
\\multicolumn{7}{l}{\\textit{Covariates}} \\\\
\\addlinespace
",
"\\addlinespace
\\hline
\\addlinespace
")


caption = paste0('Mean and standard deviation of outcome and covariates
                 \\newline by relative income mobility (IRM), N = ', ncounties, ' counties')
print(xtable(tab, caption=caption, label='descriptives'),
    caption.placement='top',
    hline.after=c(-1),
    size="scriptsize",
    table.placement='htp',
    add.to.row = addtorow,
    include.rownames=FALSE, include.colnames=FALSE,
    file='related_projects/health_inequality_project/output/tables/descriptive.tex')

# end



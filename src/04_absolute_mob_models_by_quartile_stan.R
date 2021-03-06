##############################
# stan models by quartile
# absolute mobility
# author: sebastian daza
##############################

# libraries
library(brms)
library(data.table)
library(ggplot2)
library(texreg)
library(stringr)

# functions
source('src/utils/extract_stan.R')
source('src/utils/check_convergence.R')

# load data
df = readRDS('data/le_cov_sel.rds')
ncounties = length(unique(df$county))

# auxiliry variables
df[, state := .GRP, statename]
df[, income_qr := .GRP, income_q]

# check order
table(df[, .(income_qr, income_q)])

female = df[gender=='F']
male = df[gender=='M']

# run models by gender and quartile

# baseline mode
# male
for (i in 1:4) {
    print(paste0(':::: model male quartile ', i))
    fit = brm(le ~ z_absolute_mob + z_gini + log_population + log_income +
              (1|state),
              data = male[income_qr==i],
              family = gaussian())
    check_convergence(fit)
    model_name = paste0('m1_', i)
    assign(model_name, fit)
}

# female
for (i in 1:4) {
    print(paste0(':::: model female quartile ', i))
    fit = brm(le ~ z_absolute_mob + z_gini + log_population + log_income +
              (1|state),
              data = female[income_qr==i],
              family = gaussian())
    check_convergence(fit)
    model_name = paste0('f1_', i)
    assign(model_name, fit)
}

# model with adjustments
# male
for (i in 1:4) {
    print(paste0(':::: model male adjusted quartile ', i))
    fit = brm(le ~ z_absolute_mob + z_gini + log_population + log_income +
              z_segregation_income + log_pct_black + log_pct_hispanic +
              log_unemployment + z_uninsured + z_medicare_expenses +
              (1|state),
              data = male[income_qr==i],
              family = gaussian())
    check_convergence(fit)
    model_name = paste0('m2_', i)
    assign(model_name, fit)
}

# female
for (i in 1:4) {
    print(paste0(':::: model female adjusted quartile ', i))
    fit = brm(le ~ z_absolute_mob + z_gini + log_population + log_income +
              z_segregation_income + log_pct_black + log_pct_hispanic +
              log_unemployment + z_uninsured + z_medicare_expenses +
              (1|state),
              data = female[income_qr==i],
              family = gaussian())
    check_convergence(fit)
    model_name = paste0('f2_', i)
    assign(model_name, fit)
}

# create table
for (i in 1:4) {
    cmodels <- c('Base Model', 'Base Model + Covariates', 'Base Model', 'Base Model + Covariates')
    models <- list(get(paste0('f1_', i)),
                   get(paste0('f2_', i)),
                   get(paste0('m1_', i)),
                   get(paste0('m2_', i))
                   )

    cnames <- list(z_absolute_mob = paste0('Q', i))

    # screenreg(models)
    t = texreg(models,
               include.dic = TRUE, include.waic = TRUE,
               ci.test = FALSE,
               float.pos = "htp",
               caption = "Life Expectancy (40) Models",
               booktabs = TRUE,
               use.packages = FALSE,
               dcolumn = TRUE,
               caption.above = TRUE,
               scalebox = 0.65,
               label = "inla_models",
               # sideways = TRUE,
               digits = 2,
               custom.model.names = cmodels,
               custom.coef.map = cnames,
               # groups = list("Random Effects" = c(4:5)),
               custom.note = "95\\% credibility intervals.")

    assign(paste0('tab_', i), t)
    remove(t)
}

heading = paste0('\\renewcommand{\\arraystretch}{1.2}\n
\\setlength{\\tabcolsep}{11pt}\n
\\begin{table}[htp]\n
\\begin{threeparttable}\n
\\caption{Estimates of association between life expectancy at age 40
  \\newline and absolute income mobility\\tnote{1} (N = ', ncounties, ' counties)}
  \\label{stan_absolute_mob}\n
\\centering\n
\\scriptsize\n
\\begin{tabular}{l D{.}{.}{5.11} D{.}{.}{5.11} D{.}{.}{5.11} D{.}{.}{5.11} }\n
\\hline\n
\\addlinespace\n
& \\multicolumn{2}{c}{Women} & \\multicolumn{2}{c}{Men} \\\\
Income Quartile & \\multicolumn{1}{c}{Base model\\tnote{2}} & \\multicolumn{1}{c}{Additional covariates\\tnote{3}}
& \\multicolumn{1}{c}{Base model} & \\multicolumn{1}{c}{Additional covariates} \\\\
\\addlinespace\n
\\hline')

heading =  gsub("\n\n", "\n", heading)

bottom = '\\addlinespace[5pt]\n
\\hline\n
\\end{tabular}\n
\\begin{tablenotes}[flushleft]\n
\\scriptsize\n
\\item [1] Four separated models (one per income quartile). Standardized coefficients and 95\\% credibility intervals in brackets.\n
\\item [2] Baseline models adjust for log county population size and log income.\n
\\item [3] Additional covariates models adjust for log county population size, log income, log \\% Black, log \\% Hispanic, log unemployment, z-score income segregation, z-score \\% uninsured, and z-score Medicare expenses.\n\\end{tablenotes}\n\\end{threeparttable}\n
\\end{table}'

bottom = gsub("\n\n", "\n", bottom)

sep = NA
for (i in 1:4) {
    sep[i] = "\n\\addlinespace\n"
}

tabs = list(tab_1, tab_2, tab_3, tab_4)

out = list()
for (i in 1:4) {
    out[[i]] = strsplit(tabs[[i]], '\\midrule')[[1]][2]
    out[[i]] = gsub('\n|\n\\\\', '', out[[i]])
}

# export table
cat(heading,
    sep[[1]], out[[1]],
    sep[[2]], out[[2]],
    sep[[3]], out[[3]],
    sep[[4]], out[[4]],
    bottom,
    file = 'output/tables/stan_absolute_mob_models.tex')

# end

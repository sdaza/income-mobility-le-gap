##############################
# INLA models by quartile
# relative mobility
# author: sebastian daza
##############################

# libraries
# This is INLA_18.07.12 built 2018-07-12 11:07:12 UTC.
library(INLA)
# https://github.com/julianfaraway/brinla
library(brinla)
library(data.table)
library(ggplot2)
library(texreg)
library(stringr)

# functions
source('related_projects/health_inequality_project/src/utils/extract_inla.R')
source('related_projects/health_inequality_project/src/utils/simulation_no_random_effects.R')
# source('related_projects/health_inequality_project/src/utils/simulation_no_random_effects.R')


# load data
df = readRDS('related_projects/health_inequality_project/data/le_cov_sel.rds')
ncounties = length(unique(df$county))

df[, state := .GRP, by = statename]
df[, cty := .GRP, by = county]
df[, income_qr := .GRP, by = income_q]

# check grouping variable
table(df[, .(income_qr, income_q)])

# reverse sign of relative mobility
df[, z_relative_mob := z_relative_mob * -1.0]

# auxiliry variables
df[, state_mob := state]
df[, state_gini := state]

female = df[gender=='F']
male = df[gender=='M']

# run models by gender and quartile

# baseline mode

# male

lmod = lm(le ~ z_relative_mob  + z_gini + log_population + log_income
          # + as.factor(income_qr)
          , male)

# pc prior
sdres <- sd(residuals(lmod))
pcprior <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))

# run models per income quartile
for (i in 1:4) {
    print(paste0('::::: model male quartile ', i))
    formula = le ~ z_relative_mob + z_gini + log_population + log_income +
       f(state, model = "iid", hyper = pcprior)
    model = inla(formula, family = "gaussian", data = male[income_qr==i],
          control.predictor=list(compute = TRUE),
          control.compute = list(config = TRUE, dic = TRUE,
                                 waic = TRUE, cpo = FALSE),
          control.inla = list(tolerance=1e-6, h=1e-3),
          verbose = FALSE)

    model_name = paste0('m1_', i)
    assign(model_name, model)
}

# check
bri.hyperpar.summary(m1_1)

# create data for prediction
# all values in their means except for constrast: income mobility

# # 4 quartiles for 2 contrast values
# nrep = 2
# relative_mob_pred_data = data.table(
#     z_relative_mob = c(0.0, 1.0),
#     z_gini = rep(0, nrep),
#     log_population = rep(0, nrep),
#     log_income = rep(0, nrep))

# # simulate values per quartile
# sim_male_m1 = data.table()

# for (i in 1:4) {
#     model_name = paste0('m1_', i)
#     s = simulate_pred_no_re(model=get(model_name),
#                                           data=relative_mob_pred_data,
#                                           contrast='z_relative_mob',
#                                           nsim = 2000)
#     d = s[, .(q = i, fd = diff(pred)), by = sim][, .(q, fd)]
#     sim_male_m1 = rbind(sim_male_m1, d)
# }

# saveRDS(sim_male_m1, file = 'related_projects/health_inequality_project/data/sim_male_m1.rds')

# female

lmod <- lm(le ~ z_relative_mob + z_gini + log_population + log_income
           # + as.factor(income_qr)
           , female)

sdres <- sd(residuals(lmod))
pcprior <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))

# run models per income quartile
for (i in 1:4) {
    print(paste0('::::: model female male quartile ', i))
    formula = le ~ z_relative_mob  + z_gini + log_population + log_income +
       f(state, model = "iid", hyper = pcprior)
    model = inla(formula, family = "gaussian", data = female[income_qr==i],
          control.predictor=list(compute = TRUE),
          control.compute = list(config = TRUE, dic = TRUE,
                                 waic = TRUE, cpo = FALSE),
          control.inla = list(tolerance=1e-6, h=1e-3),
          verbose = FALSE)

    model_name = paste0('f1_', i)
    assign(model_name, model)
}

# check
bri.hyperpar.summary(f1_1)

# # simulate per quartile
# sim_female_f1 = data.table()

# for (i in 1:4) {
#     model_name = paste0('f1_', i)
#     s = simulate_pred_no_re(model=get(model_name),
#                                           data=relative_mob_pred_data,
#                                           contrast='z_relative_mob',
#                                           nsim = 2000)
#     d = s[, .(q = i, fd = diff(pred)), by = sim][, .(q, fd)]
#     sim_female_f1 = rbind(sim_female_f1, d)
#     }

# saveRDS(sim_female_m1, file = 'related_projects/health_inequality_project/data/sim_female_f1.rds')

# adjusting for covariates

# male

# define PC prior
lmod <- lm(le ~ z_relative_mob  + z_gini + log_population + log_income +
          z_segregation_income + log_pct_black + log_pct_hispanic +
           log_unemployment +  z_uninsured + z_medicare_expenses
           # + as.factor(income_qr)
           , male)

sdres <- sd(residuals(lmod))
pcprior <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))

# models per quartile
for (i in 1:4) {
    print(paste0('::::: model male adjusted quartile ', i))
    formula = le ~ z_relative_mob + z_gini + log_population + log_income +
        z_segregation_income + log_pct_black + log_pct_hispanic +
        log_unemployment + z_uninsured + z_medicare_expenses +
        f(state, model = "iid", hyper = pcprior)
    model = inla(formula, family = "gaussian", data = male[income_qr==i],
          control.predictor=list(compute = TRUE),
          control.compute = list(config = TRUE, dic = TRUE,
                                 waic = TRUE, cpo = FALSE),
          control.inla = list(tolerance=1e-6, h=1e-3),
          verbose = FALSE)

    model_name = paste0('m2_', i)
    assign(model_name, model)

    }

# # create data for predictions
# nrep =  2 # 2 contrast values
# relative_mob_pred_data = data.table(
#     z_relative_mob       = c(0.0, 1.0),
#     z_gini               = rep(0, nrep),
#     log_population       = rep(0, nrep),
#     log_income           = rep(0, nrep),
#     log_crime_rate       = rep(0, nrep),
# #     log_poverty          = rep(0, nrep),
# #     log_mig_inflow       = rep(0, nrep),
# #     log_mig_outflow      = rep(0, nrep),
# #     log_foreign          = rep(0, nrep),
#     log_pct_black        = rep(0, nrep),
#     log_pct_hispanic     = rep(0, nrep),
# #     log_house_value      = rep(0, nrep),
# #     log_local_gov_exp    = rep(0, nrep),
#     log_unemployment     = rep(0, nrep),
#     z_segregation_income = rep(0, nrep),
# #     z_religion           = rep(0, nrep),
# #     z_labor_force        = rep(0, nrep),
# #     z_college            = rep(0, nrep),
# #     z_middle_class       = rep(0, nrep),
#     z_uninsured          = rep(0, nrep),
#     z_medicare_expenses  = rep(0, nrep))

# # simulate by quartile
# sim_male_m2 = data.table()

# for (i in 1:4) {
#     model_name = paste0('m2_', i)
#     s = simulate_pred_no_re(model=get(model_name),
#                                           data=relative_mob_pred_data,
#                                           contrast='z_relative_mob',
#                                           nsim = 2000)
#     d = s[, .(q = i, fd = diff(pred)), by = sim][, .(q, fd)]
#     sim_male_m2 = rbind(sim_male_m2, d)
#     }

# saveRDS(sim_male_m2, file = 'related_projects/health_inequality_project/data/sim_male_m2.rds')

# female

# define PC prior
lmod <- lm(le ~ z_relative_mob + z_gini + log_population + log_income +
      z_segregation_income + log_pct_black + log_pct_hispanic +
       log_unemployment +  z_uninsured + z_medicare_expenses
       # + as.factor(income_qr)
       , female)

# pc prior
sdres <- sd(residuals(lmod))
pcprior <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))

for (i in 1:4) {
    print(paste0('::::: model female adjusted quartile ', i))
    formula = le ~ z_relative_mob + z_gini + log_population + log_income +
        z_segregation_income + log_pct_black + log_pct_hispanic +
        log_unemployment + z_uninsured + z_medicare_expenses +
        f(state, model = "iid", hyper = pcprior)
    model = inla(formula, family = "gaussian", data = female[income_qr==i],
          control.predictor=list(compute = TRUE),
          control.compute = list(config = TRUE, dic = TRUE,
                                 waic = TRUE, cpo = FALSE),
          control.inla = list(tolerance=1e-6, h=1e-3),
          verbose = FALSE)

    model_name = paste0('f2_', i)
    assign(model_name, model)

    }

# # simulate per quartile
# sim_female_f2 = data.table()

# for (i in 1:4) {
#     model_name = paste0('f2_', i)
#     s = simulate_pred_no_re(model=get(model_name),
#                                           data=relative_mob_pred_data,
#                                           contrast='z_relative_mob',
#                                           nsim = 2000)
#     d = s[, .(q = i, fd = diff(pred)), by = sim][, .(q, fd)]
#     sim_female_f2 = rbind(sim_female_f2, d)
#     }

# saveRDS(sim_female_f2, file = 'related_projects/health_inequality_project/data/sim_female_f2.rds')


# create tables with results

# relative mobility

for (i in 1:4) {
    cmodels <- c('Base Model', 'Base Model + Covariates', 'Base Model', 'Base Model + Covariates')
    models <- list(get(paste0('f1_', i)),
                   get(paste0('f2_', i)),
                   get(paste0('m1_', i)),
                   get(paste0('m2_', i)))

    cnames <- list(z_relative_mob = paste0('Q', i))

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
\\setlength{\\tabcolsep}{11pt}
\\begin{table}[htp]\n
\\begin{threeparttable}\n
\\caption{Estimates of association between life expectancy at age 40
  \\newline and relative income mobility\\tnote{1} (N = ', ncounties, ' counties)}\\label{inla_models}\n
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
\\item [2] Baseline model adjusts for log population and log income.\n
\\item [3] Social indicators model adjusts for log population, log income, log crime rate, log \\% Black, log \\% Hispanic, log unemployment, z-score income segregation, z-score \\% uninsured, and z-score Medicare expenses.\n\\end{tablenotes}\n\\end{threeparttable}\n
\\end{table}'

bottom =  gsub("\n\n", "\n", bottom)

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
    file = 'related_projects/health_inequality_project/output/tables/relative_mob_inla_models.tex')

# supplementary table with covariates

cmodels <- c('Women Q1', 'Women Q4', 'Men Q1', 'Men Q4')
models = list(f2_1, f2_4, m2_1, m2_4)

cnames <- list(
               z_relative_mob = 'Relative income mobility (z)',
               z_gini = 'Gini (z)',
               log_population = 'Log population',
               log_income = 'Log household income',
               # log_crime_rate = 'Log crime rate',
               z_segregation_income = 'Income segregation (z)',
               log_pct_black = 'Log percent Afroamerican',
               log_pct_hispanic = 'Log percent Hispanic',
               log_unemployment = 'Log unemployment rate',
               z_uninsured = 'Percent uninsured (z)',
               z_medicare_expenses = 'Medicare expenses (z)',
               "SD for state" = "SD states",
               "SD for the Gaussian observations" = "SD observations")

texreg(models,
       include.dic = TRUE, include.waic = TRUE,
       ci.test = FALSE,
       float.pos = "htp",
       caption = paste0("Estimates of association between life expectancy at age 40
       \\newline and relative income mobility (N = ", ncounties, " counties)"),
       booktabs = TRUE,
       use.packages = FALSE,
       dcolumn = TRUE,
       caption.above = TRUE,
       scalebox = 0.65,
       label = "inla_models_cov",
       # sideways = TRUE,
       digits = 2,
       custom.model.names = cmodels,
       custom.coef.map = cnames,
       groups = list("\\addlinespace\n\\textit{Random Effects}" = c(11:12)),
       custom.note = "95\\% credibility intervals in brackets. z = standardized values. DIC = Deviance Information Criterion, WAIC = Widely Applicable Information Criterion.",
       file = 'related_projects/health_inequality_project/output/tables/relative_mob_inla_models_cov.tex')


# simulation of counter factual

# max_mob = max(c(male$z_relative_mob, female$z_relative_mob))

# # male

# vars = c("z_relative_mob","z_gini","log_population","log_income","log_crime_rate",
#          "z_segregation_income","log_pct_black","log_pct_hispanic",
#          "log_unemployment","z_uninsured","z_medicare_expenses")

# bottom_male = copy(male[income_qr==1, ..vars])
# top_male = copy(male[income_qr==4, ..vars])

# sim_male_bottom = simulate_pred_no_re(m2_1, nsim=2000, contrast='z_relative_mob', bottom_male)

# s = simulate_pred_no_re(m2_1,
# #                                           data=relative_mob_pred_data,
# #                                           contrast='z_relative_mob',
# #                                           nsim = 2000)

# names(tabs)

# # simulate values
# source('utils/simulation_random_intercept.R')

# sim_male_bottom = simulate_pred_no_re(m2_1, nsim=2000, bottom_male)
# end

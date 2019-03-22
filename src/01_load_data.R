###############################
# load chetty's data
# author: sebastian daza
###############################

library(data.table)
library(sdazar) # https://github.com/sdaza/sdazar
library(haven)

# read data covariates
cov = read_stata('related_projects/health_inequality_project/data/cty_full_covariates.dta')
cov = data.table(cov)

# rename columns
ovars = c('cty', 'county_name', 'cty_pop2000', 'statename', 'stateabbrv', 'pop_density',
          'gini99', 's_rank', 'e_rank_b', 'cs00_seg_inc', 'cs_race_theil_2000', 'hhinc00',
          'poor_share', 'frac_middleclass', 'mig_inflow', 'mig_outflow', 'cs_born_foreign',
          'rel_tot', 'crime_total', 'puninsured2010',  'cs_labforce', 'unemp_rate',
          'cs_frac_black', 'cs_frac_hisp',
          'median_house_value', 'cs_educ_ba', 'reimb_penroll_adj10', 'subcty_exp_pc')

nvars = c('county', 'county_name', 'population', 'statename', 'stateabbrv', 'density',
          'gini', 'relative_mob', 'absolute_mob', 'segregation_income', 'segregation_race',
          'income', 'poverty', 'middle_class', 'mig_inflow', 'mig_outflow', 'foreign',
          'religion', 'crime_rate', 'uninsured', 'labor_force', 'unemployment', 'pct_black',
          'pct_hispanic', 'house_value', 'college',
          'medicare_expenses', 'local_gov_exp')

setnames(cov, ovars, nvars)

total_population = sum(cov$population)

cov = cov[, ..nvars]

# missing data
m = countmis(cov)
m[m>0]

# impute missing by state
impute = function(x) {
     v = median(x, na.rm = TRUE)
     x[is.na(x)] = v
     return(x)
}

vars = names(m[m>0])
cov[, c(vars) := lapply(.SD, impute), by=statename, .SDcols=vars]

m = countmis(cov)
print(m)
m[m>0]

# table(cov[!is.na(crime_rate) , .(statename, county)], useNA='ifany')

# select complete cases
cov = cov[complete.cases(cov[, .(relative_mob, absolute_mob, income, gini)])]
length(unique(cov$county))

# le database
le = read_stata('related_projects/health_inequality_project/data/cty_leBY_gnd_hhincquartile.dta')
le = data.table(le)

le[, income_q := paste0('Q', hh_inc_q)]
setnames(le, c('cty', 'gnd', 'le_raceadj'), c('county', 'gender', 'le'))
le = le[, .(county, gender, income_q, le)]

# merge data bases
df = merge(le, cov, on='county')
dim(df)

# adjust le values (life expectancy at age 40)
df[, le := le - 40]
# center
# df[, le := le - mean(le)]

unique(df$county)

df[county==39051, .(income_q, gender, le, relative_mob)]

# transform variables
logtran = function(x, center=TRUE) {
     x = ifelse(x <= 0, log(0.1), log(x))
     if (center) {
          return(scale(x, center=TRUE, scale=FALSE))
     }
     else {
          return(x)
     }
}

ztran  = function(x) {
     m = mean(x, na.rm=TRUE)
     sm = sd(x, na.rm=TRUE)
     z = (x - m) / sm
     return(z)
}

# list of variables log (centered)
log_vars = c('population', 'poverty', 'mig_inflow',
         'mig_outflow', 'foreign', 'pct_black', 'pct_hispanic',
         'house_value', 'local_gov_exp', 'unemployment', 'income')

df[, paste0('log_', log_vars) := lapply(.SD, logtran), .SDcols=log_vars]

# z values
z_vars = c('gini', 'relative_mob', 'absolute_mob',
         'middle_class', 'segregation_income', 'religion',
         'labor_force', 'uninsured', 'medicare_expenses',
         'college')

df[, paste0('z_', z_vars) := lapply(.SD, ztran), .SDcols=z_vars]

# duplicates
anyDuplicated(df[, .(county, gender, income_q)])
length(unique(df$county))

list_variables = c(log_vars, z_vars, 'le')
df = df[complete.cases(df[, ..list_variables])]

# print output
print(paste0('Number of counties: ', length(unique(df$county))))
print(paste0('Number of states: ', length(unique(df$statename))))
print(paste0('Number of rows: ', nrow(df)))
print(paste0('Proportion population: ', round(sum(df[gender=='M' & income_q=='Q1', population])/total_population, 2)))

# save file
saveRDS(df, file='related_projects/health_inequality_project/data/le_cov_sel.rds')

# end

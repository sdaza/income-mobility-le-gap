#############################################
# reproduce results health inequality paper
# author: sebastian daza
#############################################

# load data
source('src/01_load_data.R')

# descriptive stats
source('src/02_descriptive.R')

# run relative mobility models by quartile
source('src/03_relative_mob_models_by_quartile_stan.R')

# run absolute mobility models by quartile
source('src/04_absolute_mob_models_by_quartile_stan.R')

# run relative mobility robust models by quartile
source('src/05_robust_relative_mob_models_by_quartile_stan.R')

# end

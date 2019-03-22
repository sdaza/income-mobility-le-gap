# btw grouups
first_difference_between_groups = function(data, contrast = 'z_gini', group = 'q', model = 'm1') {
    c = gtools::combinations(n = 4, r = 2, v = c(1:4), repeats.allowed = FALSE)
    t = list()
    for (i in 1:nrow(c)) {
        a = data[get(group) == c[i,1] & get(contrast) == 1, value] - data[get(group) == c[i,1] & get(contrast) == 0, value]
        b = data[get(group) == c[i,2] & get(contrast) == 1, value] - data[get(group) == c[i,2] & get(contrast) == 0, value]
        varname = paste0(c[i,1], '-', c[i,2]) 
        t[[i]] = data.table(type =contrast,  contrast = varname, model = model, values = (a - b))
    }
    return(rbindlist(t))
}
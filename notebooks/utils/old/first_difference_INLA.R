# first difference function
first_difference = function(simulated_data, value_variable, constrast_variable, 
                            simulation_index, group_variable) {
    output = data.table()
    gr = simulated_data[, unique(get(group_variable))]
    for (g in gr) { 
        diff = simulated_data[get(group_variable)==g, 
                     .(q=g, diff = diff(get(value_variable))), by=.(sim=get(simulation_index))]
        output = rbind(output, diff )
    }
    return(output)
}

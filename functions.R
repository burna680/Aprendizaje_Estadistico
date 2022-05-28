count_missing_values = function(data, column_names, zero_value, comparission){
  missing_data = c()
  for(name in column_names){
    missing_data[name] = length(which(comparission(data[name], zero_value)))
  }
  return(missing_data)
}


equals = function(x, y){
  return(x == y)
}
greater_than = function(x, y){
  return(x > y)
}

less_than = function(x, y){
  return(x < y)
}


adjust_data = function(data,name,comparisson,criteria){
  missing_values= which(comparisson(data[name], criteria))
  return(data[-missing_values,])
}
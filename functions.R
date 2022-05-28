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

stepwise = function(datainfunction){
  attach(datainfunction)
  vars = cbind(Glucose, Pregnancies, BloodPressure, SkinThickness, Insulin, DiabetesPedigreeFunction, Age)
  exhaustive<-regsubsets(datainfunction$BMI~vars,data = datainfunction, method = "exhaustive")
  
  par(mfrow=c(2,2))
  plot(summary(exhaustive)$rss,pch=20,xlab="Modelo", ylab= "RSS")
  plot(summary(exhaustive)$rsq,pch=20,xlab="Modelo", ylab= "R^2")
  plot(summary(exhaustive)$adjr2,pch=20,xlab="Modelo", ylab= "R^2 aj")
  plot(summary(exhaustive)$cp,pch=20,xlab="Modelo", ylab= "CP")
  abline(0,1)
  par(mfrow=c(1,1))
  
  return(exhaustive)
}
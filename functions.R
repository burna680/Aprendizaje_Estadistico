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

stepwise = function(data){
  vars = cbind(Glucose = data$Glucose, 
               Pregnancies = data$Pregnancies, 
               BloodPressure = data$BloodPressure, 
               SkinThickness = data$SkinThickness, 
               Insulin = data$Insulin, 
               DiabetesPedigreeFunction = data$DiabetesPedigreeFunction, 
               Age = data$Age, Outcome = data$Outcome)
  
  exhaustive<-regsubsets(data$BMI~vars,
                         data = data, 
                         method = "exhaustive")
  
  par(mfrow=c(2,1))
  plot(summary(exhaustive)$adjr2, pch=20, xlab="Modelo", ylab= "R^2 aj")
  plot(summary(exhaustive)$cp,ylim = c(0, 45), pch=20, xlab="Modelo", ylab= "CP")
  abline(0,1)
  par(mfrow=c(1,1))
  
  return(exhaustive)
}
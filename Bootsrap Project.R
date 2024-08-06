library(boot)
library(MASS)

data <- read.csv("~/Downloads/data/data_100531460.csv")
# bootstrap function for confidence intervals

bootstrap_ci <- function(data, formula, R = 1000) {
  n <- nrow(data)
  p <- length(all.vars(formula))-1
  coefs <- matrix(NA, nrow = R, ncol = p)
  for (i in 1:R) {
    boot_sample <- data[sample(n, replace = TRUE), ]
    fit <- rlm(formula, data = boot_sample)
    
    coefs[i, ] <- coef(fit)[-1]
  }
  
  # get theta hat
  
  whole_model <- rlm(formula, data = data)
  theta_hat <- coef(whole_model)[-1]
  
  # Compute confidence intervals
  
  conf_intervals <- apply(coefs, 2, function(x) quantile(x, c(0.025, 0.975)))
  
  # apply function from slides
  
  for(i in 1:p){
    conf_intervals[,i][1] = 2*theta_hat[i] - conf_intervals[,i][1]
    conf_intervals[,i][2] = 2*theta_hat[i] - conf_intervals[,i][2]
  }
  
  return(conf_intervals)
}


# 1

initial_model <- y ~ x1 + x2 + x3

initial_conf_intervals <- bootstrap_ci(data, initial_model)

# Print confidence intervals
print("95% Bootstrap Confidence Intervals for Initial model:")
print(conf_intervals)

# 2

# backward elimination function

backward_elimination = function(data, model_formula) {
  throw_away_threshold = 0.1
  
  while(TRUE) {
    current_model = rlm(model_formula, data = data[, drop = FALSE])
    coefs = coef(current_model)
    
    
    min_coef_index <- which.min(coefs)
    
    if(coefs[min_coef_index] < throw_away_threshold){
      print(min_coef_index)
      vars = all.vars(model_formula)[-1][-(min_coef_index-1)]
      model_formula = reformulate(vars, response = "y")
      print(model_formula)
    }else{
      print("Current model formula and values: ")
      print(model_formula)
      print(coefs)
      break
    }
    
  }
  
  return(model_formula)
}


# Perform backward elimination
selected_model <- backward_elimination(data, initial_model)

# Print the chosen regression model
print("Chosen Regression Model:")
print(selected_model)


# 3

conf_intervals <- bootstrap_ci(data, selected_model)

# Print confidence intervals
print("95% Bootstrap Confidence Intervals for selected model:")
print(conf_intervals)

# 4

formula <- y ~ x1 + x2 + x3

# Fit robust linear regression model
model <- rlm(formula, data = data)

# Define new data with x1 = 14, x2 = 14, x3 = 14
new_data <- data.frame(x1 = 14, x2 = 14, x3 = 14)

# Predict mean response for the new data
mean_response <- predict(model, newdata = new_data)

# Obtain confidence interval for the mean response
confidence_interval <- predict(model, newdata = new_data, interval = "confidence", level = 0.95)

# Print confidence interval
print("95% Confidence Interval on the Mean Response:")
print(confidence_interval)
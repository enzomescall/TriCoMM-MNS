library(tidyverse)
library(numDeriv)

data = read.csv("TriCoMM_DNC_data.csv")
data$Percent_GreenSpace = data$Percent_GreenSpace/100
data$PercentPopIncomeBelow2xPovertyLevel = data$PercentPopIncomeBelow2xPovertyLevel/100

G = function(C_max, percent_green) {
  pmax(C_max - percent_green * C_max/0.6, 0)
}

T = function(P, G) {
  x = pmax(P - G, 0)
  n = floor(pmax(0.2*x-40, 0)) # cost curve
  return(n) 
}

E = function(T, tree_size, area, percent_green) { # A represents the current % of area covered by greenery 
  pmin((T*tree_size)/area + percent_green, 0.9) # cap at 90% green (can be made modular)
}

K = function(B_0, B_1, E, K_current) {
  (B_0 + B_1 * E) - K_current
}

U = function(K, I) {
  K %*% I
}

mega_function = function(P, C_max, percent_green, tree_size, area, B_0, B_1, K_current, I) {
  G_value = G(C_max, percent_green)
  T_value = T(P, G_value)
  E_value = E(T_value, tree_size, area, percent_green)
  K_value = K(B_0, B_1, E_value, K_current)
  U_value = U(K_value, I)
  
  return(U_value)
}

ga_input = function(x) {
  C_max = 100 # cost of breaking in to concrete area
  B_0 = -0.02619984 # intercept given to % greenery to convert it into degrees Celsius (beta derived from regression)
  B_1 = 1.48795 # weight given to % greenery to convert it into degrees Celsius (beta derived from regression)
  tree_size = 30 # size of tree after planted
  area = 10000 # area of all sections
  mega_function(x, C_max, data$Percent_GreenSpace, tree_size, area, B_0, B_1, data$AvgReduxinNighttimeAnnualTemp_Celsius, data$PercentPopIncomeBelow2xPovertyLevel)  
}

random_ascent = function(fn, iterations = 10000, n = 193, budget = 100000, double_rate = 5, poverty = data$PercentPopIncomeBelow2xPovertyLevel) {
  a = rep(budget/(n*mean(poverty)), n) * poverty
  value = fn(a)
  converged = FALSE
  iter = 0
  
  print('starting iterations')
  while(converged == FALSE) {
    
    a_new = a
    
    for (i in 1:length(a)) {
      lucky_district = round(runif(1, 0, n))
      da = runif(1, 40, 100)
      
      if (a_new[i] > da) {
        a_new[i] = a_new[i] - da
        a_new[lucky_district] = a_new[lucky_district] + da
      }
      
      value_new = fn(a_new)
      
      if (value_new > value) {
        value = value_new
        a = a_new 
        
        da = runif(1, 80, 120)
        
        if (a_new[i] > da) {
          a_new[i] = a_new[i] - da
          a_new[lucky_district] = a_new[lucky_district] + da
        }
        
        value_new = fn(a_new)
        
        if (value_new > value) {
          value = value_new
          a = a_new 
        }
      }
    }
    
    iter = iter  + 1
    
    if(iter > iterations) {
      converged = TRUE
      print("Maximum iterations reached")
      print("Current utility is")
      print(value)
      return(a)
    }
  }
}

gradient_ascent = function(fn, rate, iterations, conv_threshold, n) {
  a = rep(250, n)
  value = fn(a)
  converged = FALSE
  i = 0
  
  while(converged == FALSE) {
    da = grad(fn, a)
    a_new = a + rate * da
    value_new = fn(a_new)
    if (abs(max(value) - max(value_new)) < conv_threshold) {
      converged = TRUE
      print("Optimal utility found")
      print(value_new)
      print("Iterations:")
      print(i)
      print("Returning vector of prices")
      return(a_new)
    }
    
    value = value_new
    a = a_new
    i = i  + 1
    
    if(i > iterations) {
      converged = TRUE
      print("Maximum iterations reached")
      print("Current utility is")
      print(value)
      return(a)
    }
  }
}

v = random_ascent(ga_input, 1000, nrow(data), 100000, 5)
v

which.min(v)

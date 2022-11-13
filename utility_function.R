library(tidyverse)
library(numDeriv)

data = read.csv("dataset_final.csv")
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

E = function(T, t, area, percent_green) { # A represents the current % of area covered by greenery 
  # assuming all trees are maple
  MSE_1 = 0.03091
  b_1 = 1.995
  A_1 = -0.205
  MSE_2 = 0.21332
  b_2 = 3.502
  A_2 = -0.365
    
  
  h_2 = exp(MSE_2/2 + A_2) * (log(t + 1))^b_2  
  r = 0.5*exp(MSE_1/2 + A_1) * (log(h_2 + 1))^b_1
  
  tree_area = T*pi*r^2
  percent_green_increase = tree_area/area
  
  percent_green + percent_green_inrease
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
  #a = rep(budget/(n*mean(poverty)), n) * poverty
  a = poor_budget
  value = fn(a)
  converged = FALSE
  iter = 0
  
  print('starting iterations')
  while(converged == FALSE) {
    for (i in 1:length(a)) {
      if (a[i] == 0) {
        next
      }
      
      a_new = a
      lucky_district = ceiling(runif(1, 0, n))
      
      da = a_new[i]
      a_new[i] = 0
      a_new[lucky_district] = a_new[lucky_district] + da
      
      value_new = fn(a_new)
      if (value_new > value) {
        value = value_new
        a = a_new 
        next
      }
      
      da = min(runif(1, 10, 50), a_new[i])
      
      a_new[i] = max(a_new[i] - da, 0)
      a_new[lucky_district] = a_new[lucky_district] + da
      
      value_new = fn(a_new)
      
      if (value_new > value) {
        value = value_new
        a = a_new 
        
        da = min(runif(1, 40, 100), a_new[i])
        
        a_new[i] = max(a_new[i] - da, 0)
        a_new[lucky_district] = a_new[lucky_district] + da
        
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

min(v)
which.min(v)

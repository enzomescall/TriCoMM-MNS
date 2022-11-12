library(tidyverse)

data = read.csv("TriCoMM_DNC_data.csv")
data$Percent_GreenSpace = data$Percent_GreenSpace/100
data$PercentPopIncomeBelow2xPovertyLevel = data$PercentPopIncomeBelow2xPovertyLevel/100

G = function(C_max, percent_green) {
  pmax(C_max - percent_green * C_max/60, 0)
}

T = function(P, G) {
  x = pmax(P - G, 0)
  n = floor(pmax(0.2*x-45, 0)) # cost curve
  return(n) 
}

E = function(T, tree_size, area, percent_green) { # A represents the current % of area covered by greenery 
  pmin((T*tree_size)/area + percent_green/100, 0.9) # cap at 90% green (can be made modular)
}

K = function(B_0, B_1, E) {
  B_0 + B_1 * E
}

U = function(K, I) {
  K %*% I
}

mega_function = function(P, C_max, percent_green, tree_size, area, B_0, B_1, I) {
  G_value = G(C_max, percent_green)
  print(G_value)
  T_value = T(P, G_value)
  print(T_value)
  E_value = E(T_value, tree_size, area, percent_green)
  print(E_value)
  K_value = K(B_0, B_1, E_value)
  print(K_value)
  U_value = U(K_value, I)
  
  return(U_value)
}


C_max = 100 # cost of breaking in to concrete area
B_0 = -0.2619984 # intercept given to % greenery to convert it into degrees Celsius (beta derived from regression)
B_1 = 1.48795 # weight given to % greenery to convert it into degrees Celsius (beta derived from regression)
tree_size = 30
area = 10000

subset = head(data, 2)

mega_function(100, C_max, subset$Percent_GreenSpace, tree_size, area, B_0, B_1, subset$PercentPopIncomeBelow2xPovertyLevel)

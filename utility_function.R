library(tidyverse)

data = read.csv("TriCoMM_DNC_data.csv")

G = function(C_max, percent_green) {
  pmax(C_max - percent_green * C_max/60, 0)
}

T = function(P, G) {
  x = pmax(P - G, 0)
  n = floor(pmax(0.2*x-45, 0)) # cost curve
  return(n) 
}

E = function(T, tree_size, area, percent_green) { # A represents the current % of area covered by greenery 
  pmin((T*tree_size)/area + percent_green, 90) # cap at 90% green (can be made modular)
}

K = function(B_0, B_1, E) {
  B_0 + B_1 * E
}

U = function(K, I) {
  K %*% I
}

C_max = 100 # cost of breaking in to concrete area
B_1 = 0.7 # weight given to % greenery to convert it into degrees Celsius (beta derived from regression)
B_0 = -0.5 # intercept given to % greenery to convert it into degrees Celsius (beta derived from regression)
n = nrow(data) # size of vectors

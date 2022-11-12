library(tidyverse)

data = read.csv("TriCoMM_DNC_data.csv")

C_max = 100 # cost of breaking in to concrete area
w = 0.7 # 

G = function(C_max, percent_green) {
  pmax(C_max - percent_green * C_max/60, 0)
}

T = function(P, G) {
  x = pmax(P - G, 0)
  n = pmax(0.2*x-45, 0) # cost curve
  return(n) 
}

G = G(100, data$Percent_GreenSpace)

T(10, 20)

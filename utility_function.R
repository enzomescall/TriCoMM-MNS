library(tidyverse)

data = read.csv("dataset_final.csv")

data$PercentPopIncomeBelow2xPovertyLevel = data$popTwicePovPct/100
data$availableGreen = (data$greenPct - data$forestsPct)/100
data$greenPct = data$greenPct/100

G = function(C_max, percent_green) {
  pmax(C_max - percent_green * C_max/0.6, 0)
}

T = function(P, G) {
  x = pmax(P - G, 0)
  n = floor(pmax((1/80)*x-(100/80), 0)) # cost curve
  return(n) 
}

K = function(B_1, T, t, current_green, area) {
  # assuming all trees are maple
  MSE_1 = 0.03490
  b_1 = 1.989
  A_1 = -0.471
  MSE_2 = 0.10769
  b_2 = 5.015
  A_2 = -2.717
  
  
  h_2 = exp(MSE_2/2 + A_2) * (log(t + 1))^b_2  
  r = 0.5*exp(MSE_1/2 + A_1) * (log(h_2 + 1))^b_1 # Erik's tree girth equation
  #print(paste(sum(T), "trees planted"))
  
  tree_area = T*pi*r^2/2.59e+6 # converting square meters to square miles
  percent_green = tree_area/area
  
  alpha = pmax(0.9-current_green, 0) # TO-DO elaborate on alpha
  E = pmin(percent_green, alpha)
  return(B_1 * E)
}

mega_function = function(P, C_max, percent_green, t, area, B_1, I, Pop) {
  G_value = G(C_max, percent_green)
  T_value = T(P, G_value)
  K_value = K(B_1, T_value, t, percent_green, area)
  U = K_value %*% (I*I*Pop)
  return(U)
}

ga_input = function(x) {
  C_max = 1000 # cost of breaking in to concrete area
  B_1 = 1.48795 # weight given to % greenery to convert it into degrees Celsius (beta derived from regression)
  t = 30 # size of tree after planted
  area = 10000 # area of all section
  mega_function(x,
                C_max = C_max,
                percent_green = data$greenPct,
                t = t,
                area = data$area.sqm.,
                B_1 = B_1,
                I = data$PercentPopIncomeBelow2xPovertyLevel,
                Pop = data$pop_2010)  
}

limited_greedy = function(fn, n, budget, step, a, max_investment_in_district) {
  while (budget > 0) {
    best_neighborhood = 0
    best_value = 0
    for (i in 1:n) {
      if (a[i] > max_investment_in_district) {
        next
      }
      a_new = a
      a_new[i] = a[i] + step
      new_value = fn(a_new)
      if(new_value > best_value) {
        best_value = new_value
        best_neighborhood = i
      } 
    } 
    a[best_neighborhood] = a[best_neighborhood] + step
    budget = budget - step
  }
  return(a)
}

v = limited_greedy(ga_input, nrow(data), 120000, 500, rep(0, 193), 20000)
plot(v, data$PercentPopIncomeBelow2xPovertyLevel)

which.max(v)

budget_levels = c((51:59) * 10000)
results = c()
for (budget in budget_levels) {
  v = greedy_while(ga_input, nrow(data), budget, 1000, rep(0, 193))
  results = c(results, ga_input(v))
}

plot(budget_levels, results)
abline(v = 450000)

toobind = data.frame(budget_levels, results)

plot_results = rbind(plot_results, toobind)



# distance analysis
# weight 1.4 = 2121.32



# utility analysis
# baseline 0.546339
# cmax 0.6 = 0.5473845
# cmax 0.8 = 0.5473845
# cmax 1.2 = 0.546339
# cmax 1.4 =  0.5452934


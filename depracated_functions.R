random_ascent = function(fn, iterations = 10000, n = 193, budget = 120000, poverty = data$PercentPopIncomeBelow2xPovertyLevel, initial_vector = rep(0, 193), lambda = 1) {
  #a = rep(budget/(n*mean(poverty)), n) * poverty
  a = initial_vector
  value = fn(a)
  converged = FALSE
  iter = 0
  
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
      
      da =min(lambda *runif(1, 10, 50), a_new[i])
      
      a_new[i] = max(a_new[i] - da, 0)
      a_new[lucky_district] = a_new[lucky_district] + da
      
      value_new = fn(a_new)
      
      if (value_new > value) {
        value = value_new
        a = a_new 
        
        da =min(lambda * runif(1, 40, 100), a_new[i])
        
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
      return(a)
    }
  }
}

greedy_while = function(fn, n, budget, step, a) {
  while (budget > 0) {
    best_neighborhood = 0
    best_value = 0
    for (i in 1:n) {
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

greedy_algo = function(fn, n, budget, step, a) {
  best_neighborhood = 0
  best_value = 0
  for (i in 1:n) {
    a_new = a
    a_new[i] = a[i] + step
    new_value = fn(a_new)
    if(new_value > best_value) {
      best_value = new_value
      best_neighborhood = i
    } 
  }
  
  a_best = a
  a_best[best_neighborhood] = a_best[best_neighborhood] + step
  
  if(budget - step < step) {
    return(a_best)
  }
  
  greedy_algo(fn, n, budget-step, step, a_best)
}

choose_best = function(initial_vector, n_vectors, fn, iterations, budget, n_rows, poverty, lambda) {
  best_vector = initial_vector
  best_value = fn(best_vector)
  for (i in 1:n_vectors) {
    temp_vector = random_ascent(fn, iterations, n_rows, budget, poverty, initial_vector, lambda)
    temp_value = fn(temp_vector)
    
    if (temp_value > best_value) {
      best_vector = temp_vector
      best_value = temp_value
      print(paste("Current best value:", best_value))
      print(paste("Iteration:", i))
    }
  }
  print(paste("Final value:", best_value))
  return(best_vector)
}
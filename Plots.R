library(tidyverse)
library(ggthemes)

data = read.csv("dataset_final.csv")

green_UHI = ggplot(data = data, aes(x = greenPct - forestsPct, y = avgTempReducNight, color = forestsPct)) +
  geom_point() + 
  labs(x = "% of green space without forests", y = "Avg Temp Reduction at Night",
       title = "Unforested Green Spaces x Urban Heat Index",
       subtitle = "Large green spaces without trees are a prime target for new tree planting") +
  theme(legend.position="none") + scale_colour_gradient(low = '#0E402D', high = '#6CAE75')

green_UHI

ggsave('unforestedGreenSpacesXUranHeatIndex.png', green_UHI, device = 'png')

multiple_budgets = ggplot(data = plot_results, aes(x = budget_levels/1000, y = results)) +
  geom_point() +
  labs(x = "Annual budget dedicated to reforestation", y = "Social benefit based on model",
       title = "Budget amount (thousands of USD) x Utility of project",
       subtitle = "After around $500,000 of investment, there are diminishing returns") + 
  geom_vline(color = 'red', xintercept = 5e+5/1000, linetype="dashed") +
  geom_smooth(method = 'loess', formula = "y~x")

multiple_budgets

ggsave('budgetIncreaseComparison.png', green_UHI, device = 'png')
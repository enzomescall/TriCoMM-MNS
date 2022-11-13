library(tidyverse)
library(ggthemes)

data = read.csv("dataset_final.csv")

green_UHI = ggplot(data = data, aes(x = greenPct - forestsPct, y = avgTempReducNight, color = forestsPct)) +
  geom_point() + 
  labs(x = "% of green space without forests", y = "Avg Temp Reduction at Night",
       title = "Unforested Green Spaces x Urban Heat Index",
       subtitle = "Large green spaces without trees are a prime target for new tree planting") +
  theme(legend.position="none") + scale_colour_gradient(low = '#0E402D', high = '#6CAE75')

ggsave('unforestedGreenSpacesXUranHeatIndex.png', green_UHI, device = 'png')

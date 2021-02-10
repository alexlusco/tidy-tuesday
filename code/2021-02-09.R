library(tidyverse)
library(scales)
library(ggthemes)

df <- tidytuesdayR::tt_load('2021-02-09') 

retire <- df$retirement

retire %>%
  ggplot(aes(x = year, y = retirement, group = race, colour = race)) +
  geom_smooth(size = 1.5, se = FALSE) +
  theme_fivethirtyeight() +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) +
  scale_y_continuous(labels = dollar_format(1)) +
  theme(legend.title = element_blank()) +
  labs(title = "Average family liquid retirement savings in the United States, 1989-2016",
       subtitle = "Dollar amounts on Y-axis normalized to 2016 dollars",
       caption = "Data source: Urban Institute/TidyTuesday")

ggsave("figures/2021-02-09.png", width = 17, height = 8)






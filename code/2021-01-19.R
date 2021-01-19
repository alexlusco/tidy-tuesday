library(rKenyaCensus)
library(tidyverse)
library(janitor)
library(ggthemr)

ggthemr("lilac")

df <- V4_T2.34

df <- df %>%
  clean_names() %>% 
  filter(admin_area == "SubCounty") %>%
  select(county, searched_online_male_perc, searched_online_female_perc) %>%
  group_by(county) %>%
  summarize(mean_male = mean(searched_online_male_perc, na.rm = TRUE),
            mean_female = mean(searched_online_female_perc, na.rm = TRUE)) %>%
  mutate(diff = mean_male - mean_female)

df %>%
  mutate(county = fct_reorder(county, diff)) %>%
  ggplot(aes(x =  county, y = diff)) +
  geom_point(size = 2) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dotted", size = 1, colour = "red") +
  theme(panel.grid.major.y = element_blank()) +
  scale_y_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2)) +
  labs(x = "",
       y = "Male-Female Difference (%)",
       title = "Mean male-female percentage difference in online search and purchasing behaviour in Kenya, by county",
       subtitle = "Red line indicates zero difference, data include all Kenyans aged 15+",
       caption = "Data source: rKenyaCensus/tidytuesday") +
  annotate("text", x = "MARSABIT", y = -1.8, label = "In Tharaka-Nithi, Kenya, 2.9% of females aged\n15 years and older search and purchased\ngoods online, as opposed to 2.3% of males", hjust = 0) +
  annotate("text", x = "MANDERA", y = -4.9, label = "In Embu, Kenya, 7.4% of females aged\n15 years and older searched and purchased\ngoods online, as opposed to 2.7% of males", hjust = 0) +
  annotate("text", x = "HOMA BAY", y = 0.05, label = "On average, the percentage of males far \noutnumbers females in terms of their use of \nthe internet to search and purchase goods\nacross Kenyan counties", hjust = 0) 

ggsave("figures/2021-01-19.png", width = 17, height = 8)



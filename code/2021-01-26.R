library(tidyverse)
library(scales)
library(Hmisc)
library(ggthemes)

plastics <- tidytuesdayR::tt_load('2021-01-26')

plastics <- plastics$plastics
  
plastics %>%
  mutate(country = tolower(country)) %>%
  filter(country %nin% c("empty")) %>%
  mutate(country = case_when(
    country == "united kingdom of great britain & northern ireland" ~ "united kingdom",
    country == "taiwan_ republic of china (roc)" ~ "taiwan",
    country == "cote d_ivoire" ~ "cote d'ivoire",
    TRUE ~ as.character(country)
  )) %>%
  filter(!is.na(volunteers)) %>%
  select(country, volunteers) %>%
  group_by(country) %>%
  summarise(volunteers = max(volunteers)) %>%
  arrange(desc(country)) %>%
  mutate(country = fct_reorder(country, volunteers)) %>%
  ggplot(aes(x = volunteers, y = country)) +
  scale_x_log10(labels = comma) +
  theme_wsj() +
  geom_text(aes(label = country)) +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 13),
        plot.caption = element_text(size = 12)) +
  labs(y = "",
       x = "Number of volunteers (log scale)",
       title = "Number of people that volunteered for Break Free From Plastic's Brand Audits in 2020, by country",
       caption = "Data source: Break Free From Plastic/Tidytuesday")
  
ggsave("figures/2021-01-26.png", width = 15, height = 8)

  
  
  










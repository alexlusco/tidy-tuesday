library(tidytuesdayR)
library(tidyverse)
library(countrycode)
library(ggrepel)
library(ggpmisc)
library(ggthemr)

tt <- tt_load("2021-01-05")

tcpdata <- tt$transit_cost

cpidata <- read_csv(here::here("outside_data/cpi_index_2019.csv")) %>% select(Country, ISO3, `CPI score 2019`, Rank)

tcpdata <- tcpdata %>%
  filter(!is.na(country))

tcpdata$country <- countrycode(tcpdata$country, "iso2c", "iso3c")

tcpcpi <- tcpdata %>%
  inner_join(cpidata, by = c("country" = "ISO3"))

tcpcpi <- tcpcpi %>%
  filter(!is.na(cost_km_millions),
         !is.na(Country)) %>%
  group_by(Country) %>%
  summarize(av_cost_per_km = mean(cost_km_millions),
            cpi_score = max(`CPI score 2019`),
            cpi_rank = max(Rank))

ggthemr("pale")

my_formula <- y ~ x

tcpcpi %>%
  ggplot(aes(x = cpi_score, y = av_cost_per_km, label = Country)) +
  geom_point(aes(size = av_cost_per_km, alpha = 0.5)) +
  geom_smooth(method = "lm", se = FALSE, formula = my_formula) +
  stat_poly_eq(formula = my_formula, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
  geom_text_repel() +
  labs(title = "Average cost of transit lines by global corruption index",
       subtitle = "What is the relationship between average cost of transit lines and global corruption ranking?",
       x = "Corruption Perceptions Index (2019)",
       y = "Average cost/KM in millions (USD)",
       caption = "Data: Transit Cost Project, Transparency International") +
  expand_limits(x = c(0, 100), y = 0) +
  theme(legend.position = "")

ggsave(here::here("figures/2021-01-05.png"), width = 12, height = 7)
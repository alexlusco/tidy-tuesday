library(tidyverse)
library(gganimate)
library(janitor)

hcbu <- tidytuesdayR::tt_load('2021-02-02') 

hcbu <- hcbu$bach_students %>% 
  clean_names() %>%
  rename(year = total) %>%
  filter(year >= 2003)

hbcu <- hbcu %>%
  select(year, white1, black1, hispanic, asian_pacific_islander_asian, asian_pacific_islander_pacific_islander, american_indian_alaska_native, two_or_more_race) %>%
  reshape2::melt(id.vars = c("year"), measure.vars = c("white1", 
                                                      "black1", 
                                                      "hispanic", 
                                                      "asian_pacific_islander_asian",
                                                      "asian_pacific_islander_pacific_islander",
                                                      "american_indian_alaska_native",
                                                      "two_or_more_race"))
hbcu <- hbcu %>%
  mutate(variable = case_when(
    variable == "white1" ~ "white",
    variable == "black1" ~"black",
    variable == "asian_pacific_islander_asian" ~ "asian",
    variable == "asian_pacific_islander_pacific_islander" ~ "pacific islander",
    variable == "american_indian_alaska_native" ~ "native",
    variable == "two_or_more_race" ~ "multiracial",
    TRUE ~ as.character(variable)
  )) %>%
  mutate(year = lubridate::ymd(year, truncated = 2L)) %>%
  mutate(variable = fct_reorder(variable, value)) %>%
  mutate(value = as.numeric(round(value, 1)))

p <- hbcu %>% 
  ggplot(aes(x = value, y = variable, fill = variable)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste(as.character(value), "%", sep = "")), hjust = -.5) +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(plot.title = element_text(size = 15)) +
  theme(plot.subtitle = element_text(size = 15)) +
  labs(title = "Racial Composition of Graduates of Historically Black Colleges in the US, 2003-2016",
    subtitle = "Year: {frame_time}",
       x = "",
       y = "") +
  transition_time(year)

animate(p, nframes = 50, fps = 1, end_pause = 20, height = 800, width = 800)

anim_save("/Users/alexluscombe/Dropbox/Git_Repos/tidytuesday/hbcu.gif")

  
  
  










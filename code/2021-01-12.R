library(tidyverse)
library(tidytuesdayR)
library(tidytext)

tt <- tt_load("2021-01-12")

floor_halfcent <- function(value){ return(value - value %% 50) }

artworkdf <- tt$artwork %>%
  select(title, year) %>%
  filter(title != "[title not known]",
         !is.na(year)) %>%
  mutate(halfcent = floor_halfcent(year)) %>%
  mutate(halfcent = as.character(halfcent)) %>%
  mutate(halfcent = format(halfcent, format = "%Y")) %>%
  select(-year) %>%
  view()

custom_stop_words <- tibble(word = c("unknown", "untitle", "untitled", "title", "blank", "ii", "el"))

art_tokens <- artworkdf %>%
  mutate(title = str_trim(title),
         title = str_squish(title)) %>%
  mutate(title = str_remove_all(title, "[[:punct:]]")) %>%
  mutate(title = str_remove_all(title, "[0-9]+")) %>%
  unnest_tokens(word, title, token = "words", to_lower = TRUE) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) %>%
  count(word, halfcent, sort = TRUE)

art_tokens %>%
  filter(n > 1) %>%
  bind_tf_idf(word, halfcent, n) %>%
  arrange(desc(tf_idf)) %>%
  group_by(halfcent) %>%
  slice_max(tf_idf, n = 4) %>%
  ungroup() %>%
  mutate(halfcent = paste(halfcent, "s", sep = "")) %>%
  ggplot(aes(y = reorder(word, +tf_idf), x = tf_idf, fill = halfcent)) +
  scale_fill_viridis_d() +
  theme_minimal() +
  geom_col() +
  facet_wrap(~halfcent, scales = "free", ncol = 5) +
  theme(legend.position = "") +
  labs(x = "tf-idf",
       y = "", 
       title = "Important words in Tate collection artwork titles, by half-century painted",
       subtitle = "1500s-2000s",
       caption = "Data source: Tate Art Museum/TidyTuesday")

ggsave("figures/2021-01-12.png", width = 15, height = 7)
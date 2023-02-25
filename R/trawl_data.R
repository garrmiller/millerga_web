library(dplyr)
library(tidyr)

dd_raw <- read.csv("./projects/trawl_fish_datasd.csv")

str(dd_raw)

with(dd_raw, table(species_name))

dd <- dd_raw %>%
  mutate(across(date_sample, ~as.Date(.,
                                      format = '%m/%d/%y')))
dd_raw %>%
  group_by(species_name) %>%
  dplyr::summarise(n = n()) %>%
  arrange(desc(n))

dd %>%
  group_by(date_sample, species_name) %>%
  dplyr::summarise(count = sum(abundance)) %>%
  ggplot(aes(x = date_sample,
             color = species_name,
             y = count)) +
  geom_point()


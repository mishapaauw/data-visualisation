library(tidyverse)

penguins <- read.csv('data/penguins.csv')

head(penguins)

penguins_not2007 <- penguins %>% filter(year != 2007)

penguins %>% filter(species %in% c("Gentoo", "Chinstrap"))

penguins %>% select(species, island, flipper_length_mm)

penguins %>% 
  filter(year == 2007) %>% 
  select(species, island)

penguins_kg <- penguins %>% 
                mutate(body_mass_kg = body_mass_g / 1000)

penguins_test <- penguins %>% 
  mutate(species_island = paste(species, island, sep = "-"))

penguins_summary <- penguins %>% 
  group_by(species) %>% 
  summarize(mean_bodymass = mean(body_mass_g, na.rm = TRUE),
            se = sd(body_mass_g, na.rm = TRUE) / sqrt(n()))

penguins_summary %>% 
  ggplot(aes(x = species, y = mean_bodymass)) +
  geom_col() + 
  geom_errorbar(aes(ymin = mean_bodymass - se,
                    ymax = mean_bodymass + se), width = 0.2)


p <- penguins %>% 
        ggplot(aes(x = species, y= body_mass_g)) +
        geom_violin() +
        geom_jitter(width = 0.2, alpha = 0.5)

ggsave('body_mass_plot.png', p, 
       width = 8, height = 8,
       units = "cm")

library(ggimage)

library(grid)
library(png)

bg <- rasterGrob(
  readPNG("data/ijsbeer.png"),
  width = unit(1, "npc"),
  height = unit(1, "npc"),
  interpolate = TRUE
)

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_length_mm / body_mass_g)) +
  annotation_custom(bg) +
  geom_point() +
  geom_image(aes(color = island, image = "data/penguin.png"), size = 0.07, alpha = 0.8) + 
  geom_image(aes(image = "data/penguin.png"), size = 0.05, alpha = 0.6)+ 
  theme_void()
    
  


penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_length_mm / body_mass_g)) +
  
  geom_point()
  


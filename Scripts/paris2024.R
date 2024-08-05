#---------------------------------------------------------------------------#
# Nom : paris2024.R                                       			            #
# Description : Visualization of paris 2024 olympic games                   #
# Auteur : Pietro Violo                                                     #
# Date : 24 Jul 2024                                                        #
# Modifications :                                                           #
#---------------------------------------------------------------------------#

options(scipen=999)

# Library
library(tidyverse)

#---------------------------------------------------------------------------#
# Data wrangling                                                            #
#---------------------------------------------------------------------------#
rm(list = ls(all = TRUE))

paris2024 <- read.csv("./Data/athletes.csv") %>% 
  mutate(height = ifelse(height == 0, NA, height)) %>% 
  filter(!is.na(height)) %>% 
  mutate(disciplines = str_remove_all(disciplines, "\\[|\\'|\\]")) %>% 
  mutate(disciplines = case_when(disciplines %in% c("3x3 Basketball, Basketball", "3x3 Basketball") ~ "Basketball",
                                 disciplines %in% c("Beach Volleyball", "Volleyball") ~ "Volleyball",
                                 TRUE ~ disciplines)) %>% 
  filter(disciplines != "Judo")
#---------------------------------------------------------------------------#
# Visualization.                                                            #
#---------------------------------------------------------------------------#

paris2024_height_order <- paris2024 %>% group_by(disciplines) %>% 
  summarise(median_height = median(height)) %>% arrange(median_height)

library(ggridges)

paris2024 <- paris2024 %>% 
  mutate(disciplines = factor(disciplines, levels = paris2024_height_order$disciplines))

medians <- paris2024 %>%
  group_by(disciplines, gender) %>%
  summarize(median_height = median(height, na.rm = TRUE))

p <- ggplot(paris2024, aes(x = height, y = disciplines, fill = gender)) +
  geom_density_ridges(alpha = 0.7, color = "white") +
  labs(title = "Height Distribution by Discipline and Gender",
       x = "Height (cm)",
       y = "Discipline") +
  theme_ridges() +
  theme(legend.position = "right")+
  theme(legend.position = "right", panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_fill_manual(values = c("#cd1127", "#002654"))


ggsave("./Outputs/height_distribution_ridgeline.png", plot = p, bg = "transparent", width = 2000, height = 3500, units = "px")


# Min max height plot

height_stats <- paris2024 %>%
  group_by(disciplines, gender) %>%
  summarize(min_height = min(height, na.rm = TRUE),
            max_height = max(height, na.rm = TRUE))

# Create the plot
p2 <- ggplot(height_stats, aes(y = disciplines, fill = gender)) +
  geom_point(aes(x = min_height), color = "#cd1127", size = 3, position = position_dodge(width = 0.7)) +
  geom_point(aes(x = max_height), color = "#002654", size = 3, position = position_dodge(width = 0.7)) +
  geom_text(aes(x = min_height, label = min_height), color = "#cd1127", size = 3, hjust = -0.5, position = position_dodge(width = 0.7)) +
  geom_text(aes(x = max_height, label = max_height), color = "#002654", size = 3, hjust = 1.5, position = position_dodge(width = 0.7)) +
  labs(title = "Minimum and Maximum Height by Gender for Each Discipline, Paris 2024",
       x = "Height (cm)",
       y = "Discipline",
       caption = "Blue points and labels represent minimum heights, red points and labels represent maximum heights") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "right")

ggsave("./Outputs/minmax.png", plot = p2, bg = "transparent", width = 2000, height = 3500, units = "px")


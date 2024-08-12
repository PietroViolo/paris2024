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
# Height ridges.                                                            #
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


#---------------------------------------------------------------------------#
# Age beeswarm.                                                             #
#---------------------------------------------------------------------------#
rm(list = ls())

library(ggbeeswarm)
library(ggthemes)

paris2024 <- read.csv("./Data/athletes.csv") %>%
  mutate(disciplines = str_remove_all(disciplines, "\\[|\\'|\\]")) %>% 
  mutate(disciplines = case_when(
    disciplines %in% c("3x3 Basketball, Basketball", "3x3 Basketball") ~ "Basketball",
    disciplines %in% c("Beach Volleyball", "Volleyball") ~ "Volleyball",
    disciplines %in% c("Cycling Road, Cycling Track", "Cycling Road, Cycling Mountain Bike", "Cycling Road, Triathlon", 
                       "Cycling Track", "Cycling Mountain Bike", "Cycling Road") ~ "Cycling",
    TRUE ~ disciplines
  )) %>% 
  filter(!is.na(birth_date)) %>% 
  mutate(age_in_days = difftime(Sys.Date(), as.Date(birth_date), units = "days"),
         age_in_years = as.numeric(age_in_days) / 365.25)

# Age group
age_summ <- paris2024 %>% group_by(disciplines, gender) %>% 
  summarise(mean_age = round(mean(age_in_years), 1),
            n = n()) %>% 
  arrange(mean_age)

# extract age order

age_summ %>% filter(gender == "Female") %>% pull(disciplines) %>% unique()

order <- age_summ %>% pull(disciplines) %>% unique()

paris2024 <- paris2024 %>% mutate(disciplines = factor(disciplines, levels = order))

paris2024 <- left_join(paris2024, age_summ)

library(showtext)

# Add the Loew-Heavy font
font_add("Loew-Heavy", regular = "/Users/pietroviolo/Library/Fonts/Loew-Heavy.otf")
showtext_auto()


# Create the plot
plot <- ggplot(paris2024, aes(x = age_in_years, y = disciplines, color = gender)) +
  geom_quasirandom(size = 1.4, alpha = 0.7) +  # Smaller points with transparency
  theme_minimal(base_size = 12) +              # Minimal theme for a cleaner look
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),  # Transparent background
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.position = "none",
    text = element_text(family = "Loew-Heavy"),  # Set all text elements to use Loew-Heavy font
    axis.title = element_text(size = 40),        # Increase axis titles
    axis.text = element_text(size = 30),         # Increase axis text
    plot.title = element_text(size = 34, hjust = 0.5),  # Increase plot title and center it
    plot.subtitle = element_text(size = 30)
  ) +
  labs(x = "Age in Years", y = "Discipline", title = "Age Distribution of 11,110 Olympians Across 42 Disciplines by Gender, Paris 2024") +  # Smaller points with transparency
  scale_x_continuous(limits = c(0, 75), breaks = seq(0, 70, by = 10)) +
  scale_color_manual(values = c("#cd1127", "#002654")) +
  geom_text(data = paris2024 %>% select(disciplines, gender, mean_age) %>% distinct() %>% filter(gender == "Male"),
            aes(x = 72, y = disciplines, label = mean_age),
            size = 5, hjust = 0, family = "Loew-Heavy")+
  geom_text(data = paris2024 %>% select(disciplines, gender, mean_age) %>% distinct() %>% filter(gender == "Female"),
            aes(x = 71, y = disciplines, label = mean_age),
            size = 5, hjust = 0, family = "Loew-Heavy")


# Save the plot as a PNG with a transparent background
ggsave("./Outputs/paris2024_age_distribution.png", plot = plot, bg = "transparent", width = 10, height = 15, dpi = 300)




# beeswarm(age_in_years ~ disciplines, 
#          data=paris2024, 
#          col=sample(colors(), 42), 
#          pch=19, 
#          method="swarm",
#          cex=0.5,
#          priority = "descending") 

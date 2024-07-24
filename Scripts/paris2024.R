#---------------------------------------------------------------------------#
# Nom : circular_migration.R                               			            #
# Description : Circular migration                                          #
# Auteur : Pietro Violo                                                     #
# Date : 3 Jul 2024                                                         #
# Modifications :                                                           #
#---------------------------------------------------------------------------#

options(scipen=999)

# Library
library(tidyverse)
library(circlize)

#---------------------------------------------------------------------------#
# Data wrangling                                                            #
#---------------------------------------------------------------------------#
rm(list = ls(all = TRUE))

# Import data
x2 <- read_dta("./Data/circularplot_data18l.dta") %>% 
  mutate(Year = 2018)
x3 <- read_dta("./Data/circularplot_data23l.dta") %>% 
  mutate(Year = 2023)

x1 <- read_dta("./Data/circularplot_data14l.dta") %>% 
  mutate(Year = 2014) %>% 
  rename(UPM = UPM_DIS) %>% 
  select(colnames(x2))


# Combine tables
df <- rbind(x1,x2,x3)

x2 <- x2 %>% 
  select(Q_Mig_STATEUSA_ARRIVED,Q_Mig_STATEUSA_LAST, Q_Mig_SEX ,ENT, Year) 

rm(x1,x2,x3)


mexican_states <- c("01" = "Aguascalientes", "02" = "Baja California", "03" = "Baja California Sur",
                    "04" = "Campeche", "05" = "Coahuila de Zaragoza", "06" = "Colima", "07" = "Chiapas",
                    "08" = "Chihuahua", "09" = "Mexico City", "10" = "Durango", "11" = "Guanajuato",
                    "12" = "Guerrero", "13" = "Hidalgo", "14" = "Jalisco", "15" = "State of Mexico",
                    "16" = "Michoacán de Ocampo", "17" = "Morelos", "18" = "Nayarit", "19" = "Nuevo León",
                    "20" = "Oaxaca", "21" = "Puebla", "22" = "Querétaro", "23" = "Quintana Roo",
                    "24" = "San Luis Potosi", "25" = "Sinaloa", "26" = "Sonora", "27" = "Tabasco",
                    "28" = "Tamaulipas", "29" = "Tlaxcala", "30" = "Veracruz de Ignacio de la Llave",
                    "31" = "Yucatan", "32" = "Zacatecas")



mexican_states <- c(
  "01" = "Ags",        # Aguascalientes
  "02" = "BC",         # Baja California
  "03" = "BCS",        # Baja California Sur
  "04" = "Camp",       # Campeche
  "05" = "Coah",       # Coahuila de Zaragoza
  "06" = "Col",        # Colima
  "07" = "Chis",       # Chiapas
  "08" = "Chih",       # Chihuahua
  "09" = "CDMX",       # Mexico City
  "10" = "Dgo",        # Durango
  "11" = "Gto",        # Guanajuato
  "12" = "Gro",        # Guerrero
  "13" = "Hgo",        # Hidalgo
  "14" = "Jal",        # Jalisco
  "15" = "EdoMex",     # State of Mexico
  "16" = "Mich",       # Michoacán de Ocampo
  "17" = "Mor",        # Morelos
  "18" = "Nay",        # Nayarit
  "19" = "NL",         # Nuevo León
  "20" = "Oax",        # Oaxaca
  "21" = "Pue",        # Puebla
  "22" = "Qro",        # Querétaro
  "23" = "QRoo",       # Quintana Roo
  "24" = "SLP",        # San Luis Potosi
  "25" = "Sin",        # Sinaloa
  "26" = "Son",        # Sonora
  "27" = "Tab",        # Tabasco
  "28" = "Tamps",      # Tamaulipas
  "29" = "Tlax",       # Tlaxcala
  "30" = "Ver",        # Veracruz de Ignacio de la Llave
  "31" = "Yuc",        # Yucatan
  "32" = "Zac"         # Zacatecas
)


# For states in the US
us_states <- c("1" = "California", "2" = "Texas", "3" = "Florida", "4" = "Arizona", 
               "5" = "New York", "6" = "Illinois", "7" = "Other State", "8" = "Other Country",
               "9" = "Not specified")

us_states <- c(
  "1" = "CA",          # California
  "2" = "TX",          # Texas
  "3" = "FL",          # Florida
  "4" = "AZ",          # Arizona
  "5" = "NY",          # New York
  "6" = "IL",          # Illinois
  "7" = "O-State", # Other State
  "8" = "O-Country", # Other Country
  "9" = "Not specified" # Not specified
)


countries <- c("1" = "United States of America", 
               "2" = "Mexico", 
               "3" = "Other country", 
               "9" = "Not specified")


# Recode states 
# Q_Mig_YEARLEFT = YEAR OF THE LAST MIGRATION
# Q_Mig_STATEUSA_LAST
# Q_Mig_RESIDENCE = COUNTRY OF RESIDENCE CURRENTLY
# Q_Mig_YEARRETURN = YEAR OF RETURN TO MEXICO
# Q_Mig_STATEUSA_ARRIVED = US state of migration / 
# Q_Mig_STATELIVED = State of residence in Mexico when the migrant left last time
# COND_RESID = Return in the state of Mexico



df <- df %>% mutate(ENT = recode(ENT, !!!mexican_states),
                    Q_Mig_STATEUSA_ARRIVED = recode(Q_Mig_STATEUSA_ARRIVED, !!!us_states),
                    Q_Mig_STATEUSA_LAST = recode(Q_Mig_STATEUSA_LAST, !!!us_states),
                    Q_Mig_RESIDENCE = recode(Q_Mig_RESIDENCE, !!!countries))


df_select <- df  %>% 
  select(LLAVE_PER, Q_Mig_STATEUSA_ARRIVED,Q_Mig_STATEUSA_LAST, Q_Mig_SEX ,ENT, Year,
         Q_Mig_RESIDENCE, Q_Mig_YEARRETURN, Q_Mig_YEARLEFT)

# There are only six people that come back as a follow up
df_select %>% group_by(LLAVE_PER) %>% summarise(n = n()) %>% arrange(desc(n))




#---------------------------------------------------------------------------#
# Create viz                                                                #
#---------------------------------------------------------------------------#

x1 <- df_select %>% filter(Q_Mig_RESIDENCE == "Mexico") %>% 
  rename(Destination = ENT,
         Origin = Q_Mig_STATEUSA_LAST) %>% 
  select(Year, Origin, Destination)


x2 <- df_select %>% filter(Q_Mig_RESIDENCE == "United States of America") %>% 
rename(Destination = Q_Mig_STATEUSA_ARRIVED,
         Origin = ENT) %>% 
  select(Year, Origin, Destination)

# 2044 + 2977 = 5021, we're missing 279 individuals....

# Origin-destination matrix

origin_dest <- rbind(x1, x2)
rm(x1,x2)

origin_dest %>% pull(Origin) %>% unique()


# Count by Year etc.

flux_counts <- origin_dest %>% 
  group_by(Year, Origin, Destination) %>% 
  summarise(Flux = n())

# Faudrait savoir ...
df %>% group_by(Year) %>% 
  summarise(n = n())
# Il y avait 665 répondants en 2014, 1809 en 2018 et 2856 en 2023.
# Or, ce n'est pas représentatif du nombre de migrations... 
# Faudrait penser à standardiser

# Remove ""

flux_counts <- flux_counts %>% filter(Origin != "" & Destination != "",
                                      Origin != "Not specified" & Destination != "Not specified")

#---------------------------------------------------------------------------#
# All states combined                                                       #
#---------------------------------------------------------------------------#

df_flux <- flux_counts %>% group_by(Origin, Destination) %>% 
  summarise(Flux = sum(Flux))

# Define the order of sectors
# There are 9 that aren't mexican

sector_order <- c(
  "Ags", "BC", "BCS", "CDMX", "Camp", "Chih", "Chis", "Coah", "Col", "Dgo", 
  "EdoMex", "Gro", "Gto", "Hgo", "Jal", "Mich", "Mor", "NL", "Nay", "Oax", 
  "Pue", "QRoo", "Qro", "SLP", "Sin", "Son", "Tab", "Tamps", "Tlax", "Ver", 
  "Yuc", "Zac", "O-Country", "O-State","TX", "CA", "AZ", "FL", "IL", "NY")


# Define the colors
library(grDevices)

# Define the mexican colors for the gradient
colors <- c("#386641", "#ffffff", "#e71d36")

# Create a function to generate the color gradient
color_gradient <- colorRampPalette(colors)

# Generate a vector of 32 colors
color_vector <- color_gradient(32)
color_vector <- rev(color_vector)

# Define the colors for the gradient
colors <- c("navy", "lightblue")

# Create a function to generate the color gradient
color_gradient_usa <- colorRampPalette(colors)

# Generate a vector of 9 colors
color_vector_usa <- color_gradient_usa(8)

color_vector <- c(color_vector, color_vector_usa)


# Ensure gap_degree has the same length as sector_order
gap_degree <- rep(1, length(sector_order))
gap_degree[length(sector_order)] <- 50  # Larger gap before Texas
gap_degree[length(sector_order)-8] <- 50


png(filename = "./Outputs/mig_plot.png", width = 3000, height = 3000, res = 300)

par(mar = c(5, 5, 5, 5))

circos.clear()
circos.par(gap.after=gap_degree)

chordDiagram(df_flux,
             grid.col = color_vector,
             directional = 1,
             order = sector_order,
             direction.type = c("arrows", "diffHeight"), 
             #annotationTrack = "grid",
             diffHeight  = -0.04, 
             annotationTrackHeight = c(0.05, 0.05),
             link.arr.type = "big.arrow", 
             link.largest.ontop = FALSE)

# circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
#   circos.text(CELL_META$xcenter, CELL_META$ylim[1], 
#               CELL_META$sector.index, 
#               facing = "clockwise", 
#               niceFacing = TRUE, 
#               adj = c(-0.5, 0.5))
# }, bg.border = NA)


title("Migration Chord Diagram Between Mexico and the United States")

dev.off()


#---------------------------------------------------------------------------#
# By States                                                                 #
#---------------------------------------------------------------------------#


# Texas, for example
df_tex <- flux_counts %>% filter(Year == 2018,
                                 Origin == "TX"| Destination == "TX") %>% 
  ungroup() %>% 
  select(-Year)

unique_sectors <- c(df_tex %>% pull(Origin) %>% unique(),
                    df_tex %>% pull(Destination) %>% unique) %>% 
  unique()

# colors

# Load the necessary package
library(grDevices)

# Define the colors for the gradient
colors <- c("#386641", "#ffffff", "#e71d36")

# Create a function to generate the color gradient
color_gradient <- colorRampPalette(colors)

# Generate a vector of 32 colors
color_vector <- color_gradient(length(unique_sectors)-1)

color_vector <- rev(color_vector)


color_vector[32] <- "navy"


# Print the color vector
print(color_vector)


sector_order_state <- sector_order[sector_order %in% unique_sectors]

circos.clear()

# Ensure gap_degree has the same length as sector_order
gap_degree <- rep(1, length(sector_order_state))
gap_degree[length(sector_order_state)] <- 50  # Larger gap before Texas
gap_degree[length(sector_order_state)-1] <- 50


png(filename = "./Outputs/Texas_mig.png", width = 3000, height = 3000, res = 300)

par(mar = c(5, 5, 5, 5))

circos.clear()
circos.par(gap.after=gap_degree)

chordDiagram(df_tex,
             grid.col = color_vector,
             directional = 1,
             order = sector_order_state,
             direction.type = c("arrows", "diffHeight"), 
             #annotationTrack = "grid",
             diffHeight  = -0.04, 
             annotationTrackHeight = c(0.05, 0.05),
             link.arr.type = "big.arrow", 
             link.largest.ontop = FALSE)

# circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
#   circos.text(CELL_META$xcenter, CELL_META$ylim[1], 
#               CELL_META$sector.index, 
#               facing = "clockwise", 
#               niceFacing = TRUE, 
#               adj = c(-1.5, 0.5))
# }, bg.border = NA)


title("Migration Chord Diagram Between Mexico and Texas")

dev.off()


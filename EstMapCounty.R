

library(pals)
library(raster)
library(sf)
library(spdep)
library(tidyverse)



setwd("/data/scripts/Ling/Proj1Maps/")

# 1: Import and join data 
county_layer <- read_sf("EstPers/Shapefiles/countySHP")
parish_layer <- read_sf("EstPers/Shapefiles/municipalitySHP/")

# Import parish-level variable
my_data <- read_csv("/data/scripts/Ling/Proj2PESH/inc Russian/B5_county_incRussian.csv")

my_data = rename(my_data, MNIMI = currentCounty)

currentCounty = my_data %>% group_by(MNIMI) %>% 
  summarise_at(c("N_T","E_T","O_T","A_T","C_T"), mean)

my_data$MNIMI <- as.character(my_data$MNIMI)

county <- left_join(county_layer, currentCounty, "MNIMI")


# count parish frequency

MNIMI_frequency <- my_data %>%
  count(MNIMI, name = "sample")

county$MNIMI <- as.character(county$MNIMI)
county <- left_join(county, MNIMI_frequency, "MNIMI")

library(dplyr)
library(psych)
# check the range of the differences

# pick parishes sample size >50
disaggr_layer <- county %>%
  filter(sample >= 50)

combined_summary <- bind_rows(
  describe(disaggr_layer$O_T),
  describe(disaggr_layer$N_T),
  describe(disaggr_layer$E_T),
  describe(disaggr_layer$A_T),
  describe(disaggr_layer$C_T)
) %>% mutate(Trait = c("Openness", "Neuroticism", "Extraversion", "Agreeableness", "Conscientiousness")) %>% 
  dplyr::select(Trait, everything()) %>% 
  select(c("Trait", "n", "mean", "sd", "min", "max", "range", "se")) %>%
  `colnames<-`(c("Trait", "n", "M", "SD", "Min", "Max", "Range", "SE"))

nice_table(combined_summary, title = "Descriptive Statistics: Personality Trait T-scores at County Level", note = "T-scores were standardized according to the mean and SD of Harju County.")






# generate a color coding scheme

grouping <- seq(43, 54, by = 1)

colors <- brewer.ylorrd(11)



# 2: Conventional mapping


# to draw the map, rerun the codes below


# Create a new column with the intervals

county$group <- findInterval(county$N_T, grouping) 
county$colors <- colors[county$group]



# produce the traditional geographic map:replace ggtile


ggplot() +
  geom_sf(data = county,
          aes(fill = factor(group)),
          color = county$colors) +  # Use 'factor(group)' for discrete colors
  geom_sf(data = parish_layer,
          fill = NA,
          color = "#bbbbbb",
          size = 0.3) +
  geom_sf(data = county_layer,
          fill = NA) +
    scale_fill_manual(values = colors,breaks = 1:(length(grouping) - 1),  # Use 1 to n-1 as breaks
                    labels = paste0(grouping[-length(grouping)], " to ", grouping[-1]), 
                    guide = guide_legend("T-scores")) +  # Use 'guide_legend' to display a legend
  
  ggtitle("Neuroticism") +
  theme_bw()





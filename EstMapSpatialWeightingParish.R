library(pals)
library(raster)
library(sf)
library(spdep)
library(tidyverse)
library(ggplot2)


setwd("/data/scripts/Ling/Proj1Maps/")

# 1: Import and join data 
county_layer <- read_sf("EstPers/Shapefiles/countySHP")
parish_layer <- read_sf("EstPers/Shapefiles/municipalitySHP/")


# Import parish-level variables

my_data <- read_csv("/data/scripts/Ling/Proj2PESH/inc Russian/municipality_PESH_B5.csv")

my_data = rename(my_data, ONIMI = currentParishEHAK) %>% 
  filter(sample_size >= 50)

parish <- left_join(parish_layer, my_data, "ONIMI")
parish_df <- parish %>% as.data.frame()

library(dplyr)
library(psych)
library(rempsyc)

## check the min, max and range of T-scores before grouping

combined_summary <- bind_rows(
  describe(my_data$N_T),
  describe(my_data$E_T),
  describe(my_data$O_T),
  describe(my_data$A_T),
  describe(my_data$C_T)) %>% 
  mutate(Trait = c("Neuroticism", "Extraversion", "Openness", "Agreeableness", "Conscientiousness")) %>% 
  select(Trait, everything()) %>% 
  select(c("Trait", "n", "mean", "sd", "min", "max", "range", "se")) %>%
  `colnames<-`(c("Trait", "n", "Mean", "SD", "Min", "Max", "Range", "SE"))

DesMunicipality = nice_table(combined_summary, 
           title = "Descriptive Statistics: Personality Trait T-scores at Municipality Level", 
           note = c("T-scores were standardized according to the mean and SD of Tallinn City.",
                    "Five municipalities were excluded (n < 50)."))
DesMunicipality
print(DesMunicipality, preview = "docx")


## print regional personality trait scores and n of each region

municipality_PT_N = read_csv("/data/scripts/Ling/Proj2PESH/inc Russian/B5_municipality_incRussian.csv") %>%
  rename(Municipality = currentParishEHAK) %>%
  group_by(Municipality)%>% 
  select(Municipality, O_T,N_T,E_T, A_T, C_T) %>%
  summarize(`n` = n(),
            Openness = mean(O_T),
            sdOpenness = sd(O_T),
            Neuroticism = mean(N_T),
            sdNeuroticism = mean(N_T),
            Extraversion = mean(E_T),
            sdExtraversion = sd(E_T),
            Agreeableness = mean(A_T),
            sdAgreeableness = sd(A_T),
            Conscientiousness = mean(C_T),
            sdConscientiousness = sd(C_T)) %>%
  arrange(desc(n))%>%
  mutate(Municipality = gsub("(.*) vald", "\\1 rural municipality", Municipality)) %>%
  mutate(Municipality = gsub("(.*) linn", "\\1 city", Municipality))

MunicipalityTable = nice_table(municipality_PT_N,title = "Sample Sizes and Mean Trait T-scores of Municipalities")

MunicipalityTable

setwd("/data/scripts/Ling/Proj2PESH/Outputs/")
write_csv(municipality_PT_N, "DesMuni.csv")
print(MunicipalityTable, preview = "xlsx")


# generate a color coding scheme

grouping <- seq(43, 54, by = 1)

colors <- brewer.ylorrd(11)


# 2: Conventional mapping


# to draw the map, rerun the codes below

# ****Replace other trait****

# Create a new column with the intervals

parish$group <- findInterval(parish$A_T, grouping)
parish$colors <- colors[parish$group]


# produce the traditional geographic map

ggplot() +
  geom_sf(data = parish,
          aes(fill = factor(group)),
          color = parish$colors) +  # Use 'factor(group)' for discrete colors
  geom_sf(data = parish_layer,
          fill = NA,
          color = "#bbbbbb",
          size = 0.3) +
  geom_sf(data = county_layer,
          fill = NA) +
  #geom_label(data = parish, aes(fill = parish$ONIMI), color = "red", size = 3) +
  # Define the manual fill scale and add the legend
  scale_fill_manual(values = colors,breaks = 1:(length(grouping)-1),  # Use 1 to n-1 as breaks
                    labels = paste0(grouping[-length(grouping)], " to ", grouping[-1]), 
                    guide = guide_legend("T-scores")) +  # Use 'guide_legend' to display a legend
  
  ggtitle("Agreeableness") +
  theme_bw()




# 3: Mapping based on distance weights

my_data <- read_csv("/data/scripts/Ling/Proj2PESH/inc Russian/municipality_PESH_B5.csv")

my_data = rename(my_data, ONIMI = currentParishEHAK) 

parish <- left_join(parish_layer, my_data, "ONIMI")


## Create raster layer:1 * 1 kilometers
raster_size <- 1000
raster_layer <- raster(ext = extent(parish_layer), res = c(raster_size, raster_size))


# to draw the map, rerun the codes below

## ****Replace other trait****Create layer of data points for distance-based weighting
db_layer <- parish %>%
  filter(!is.na(A_T)) %>% 
  as_Spatial()

## define a function to calculate the Euclidean distance between all geographic coordinates stored for the rasters and the sample of all measurement locations
eucl_dist <- function(p, q) {
  
  a <- outer(p[, 1], q[, 1], "-")^2
  b <- outer(p[, 2], q[, 2], "-")^2
  sqrt(a + b)
  
}

## apply this function to calculate the geographic distances
gdistmat <- eucl_dist(coordinates(raster_layer), coordinates(db_layer))
gdistmat <- gdistmat / 1000 # Transform meters to kilometers

## Calculating spatial weights using a log logistic distance decay function
spatial_weights <- function(d, r, s) {
  
  (1/(1+((d/r)^s)))
  
}


## log logistic distance decay function:we defined r = 10 kilometers and s = 7 (not sure ab r and s in Estonia)
r <- 10
s <- 4
gweights <- spatial_weights(gdistmat, r = r, s = s)

## Run the distance-based smoothing:takes three input parameters `w` (the spatial weights), `v` (the variable that is to be interpolated), and `n` (the size of the sample)
db_fun <- function(w, v, n) {
  
  x <- rowSums(t(apply(w, 1, function(x) x * v * n)), na.rm = TRUE)
  x_weight <- rowSums(t(apply(w, 1, function(x) x * n)), na.rm = TRUE)
  x <- x / x_weight
  x
  
}

## ****Replace other trait****apply it to our data
db_values <- db_fun(w = gweights, v = db_layer$A_T, n = db_layer$sample_size)

##store the interpolated values in the raster layer
db_raster_layer <- setValues(raster_layer, db_values)


## Map computed values
##unitl now, the raster layer is still a rectangle. Hence, it does not cover the contiguous Estonia one on one. 
##To change this, we crop the raster layer to the exact shape of the contiguous Estonia

db_raster_layer <- mask(db_raster_layer, parish)


## To map the raster layer with `ggplot`, we need to change the raster to an object of class data frame

db_raster_layer <- as.data.frame(db_raster_layer, xy = TRUE)
db_raster_layer <- na.omit(db_raster_layer)


## Equipped with the smoothed values for each grid cell in the raster layer, we can now group the values  using the same  coding scheme that we used for the disaggregated map.


db_raster_layer$group <- findInterval(db_raster_layer$layer, grouping)
db_raster_layer$colors <- colors[db_raster_layer$group]



 ggplot() +
  geom_raster(data = db_raster_layer, 
              aes(x = x, y = y), 
              fill = db_raster_layer$colors,
              color = db_raster_layer$colors) +
  geom_sf(data = county_layer,
          fill = NA) +
  labs(x = "",
       y = "") +
  ggtitle("Agreeableness") +
  theme_bw()

 
                





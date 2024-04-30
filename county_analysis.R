

library(tidyverse)


setwd("/data/scripts/Ling/Proj2PESH/")


# basic analysis

county_PESH <- read.csv("/data/scripts/Ling/Proj2PESH/Aggregated Data/county_PESH.csv")


B5 <- read.csv("/data/scripts/Ling/Proj2PESH/inc Russian/B5_county_incRussian.csv") %>%
  mutate(Place.of.residence = gsub("(.*) maakond", "\\1 County", currentCounty)) %>% 
  filter(!is.na(Place.of.residence)) %>% 
  select(Place.of.residence, gender, age, N_T, O_T, C_T, A_T, E_T) %>%
  group_by(Place.of.residence) %>%
  mutate(sample_size = n())%>%
  summarize(n = mean(sample_size),
            gender = mean(gender),
            age = mean(age),
            Openness = mean(O_T),
            Neuroticism = mean(N_T),
            Extraversion = mean(E_T),
            Agreeableness = mean(A_T),
            Conscientiousness = mean(C_T)) %>%
  arrange(desc(n))
  

county_PESH_B5 <- county_PESH %>% 
  left_join(B5, "Place.of.residence") %>%
  select(-mean_poverty_rate, -female_rate) %>%
  select(12:14, everything()) %>%
  select(4, everything())

setwd("/data/scripts/Ling/Proj2PESH/inc Russian/")

write_csv(county_PESH_B5, "county_PESH_B5.csv")

#county_PESH <- county_PESH %>% mutate_at(c("N_T","E_T","O_T","A_T","C_T"), ~ residuals(lm(. ~ gender + age)))

#correlation_PESH <- county_PESH %>% select(-1) %>% cor()
county_PESH_B5 = read_csv("county_PESH_B5.csv")


library(psych)
x = county_PESH_B5[, c(2:16,18,19)]
y = county_PESH_B5[20:24]

spearman <- corr.test(x,y, method="spearman", adjust="fdr")

spearman$r
spearman$p
spearman$p.adj

r = round(spearman$r,2) %>% as.data.frame %>% rownames_to_column("PESH Indicators")
c
library(openxlsx)

prediction_table <- read_csv("Aggregated Data/Est_PESH_Prediction Table_2.csv") %>% 
  setNames(.[1, ]) %>%
  slice(-1)


r_p <- p.adj %>% left_join(r, "PESH Indicators") %>%
  left_join(prediction_table, "PESH Indicators") %>% mutate_at(vars(Openness:Neuroticism),as.numeric)

prediction_table_results = write.xlsx(r_p, "prediction_table_results.xlsx")


## correlation table
x.control = control_county_PESH_B5[2:16]
y.control = control_county_PESH_B5[20:24]

spearman_control <- corr.test(x.control,y.control, method="spearman", adjust="fdr")

r_control = round(spearman_control$r,2)%>% as.data.frame %>% rownames_to_column("PESH Indicators")
p.adj_control = round(spearman_control$p.adj,2) %>% as.data.frame %>% rownames_to_column("PESH Indicators")

r_p.adj_control = r_control %>% left_join(p.adj_control, "PESH Indicators")%>%
  left_join(prediction_table, "PESH Indicators") %>% mutate_at(vars(Openness:Neuroticism),as.numeric)

## spatial autocorrelation

county_layer <- read_sf("/data/scripts/Ling/Proj1Maps/EstPers/Shapefiles/countySHP") %>%
  mutate(Place.of.residence = gsub("(.*) maakond", "\\1 County", MNIMI))
parish_layer <- read_sf("/data/scripts/Ling/Proj1Maps/EstPers/Shapefiles/municipalitySHP")


# Import county-level variables

my_data <- read_csv("county_PESH_B5.csv")

county <- left_join(county_layer, my_data, "Place.of.residence")

nb<-poly2nb(county, queen=T)

nbw <-nb2listw(nb, style="W", zero.policy = TRUE)

moran.plot(as.numeric(scale(county$Openness)), listw=nbw,
           zero.policy = TRUE,
           xlab="Standardized Openness", 
           ylab="Neighbors Standardized Openness",
           main=c("moran.mc(parishmoran.mc(parish", "in Estonia") )

# all five not significant
moran.test(county$Openness ,listw = nbw, zero.policy = TRUE,na.action = na.omit)
moran.test(county$Neuroticism,listw = nbw, zero.policy = TRUE,na.action = na.omit)
moran.test(county$Extraversion ,listw = nbw, zero.policy = TRUE,na.action = na.omit)
moran.test(county$Agreeableness,listw = nbw, zero.policy = TRUE,na.action = na.omit)
moran.test(county$Conscientiousness,listw = nbw, zero.policy = TRUE,na.action = na.omit)
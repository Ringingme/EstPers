library(tidyverse)


setwd("/data/scripts/Ling/Proj2PESH/")

municipality_PESH = read.csv("Aggregated Data/municipality_PESH.csv")

library(stringi)

municipality_B5 = read.csv("/data/scripts/Ling/Proj2PESH/inc Russian/B5_municipality_incRussian.csv")



# then we need to exclude the municipalities with sample size smaller than 50

aggregated_municipality_B5 = municipality_B5 %>% 
  group_by(currentParishEHAK) %>%
  mutate(sample_size = n())

## calculate the Means of each municipality and replace Estonian with English
aggregated_municipality_B5 = aggregated_municipality_B5 %>% 
  group_by(currentParishEHAK) %>%
  summarize_at(c("N_T", "E_T", "O_T", "C_T", "A_T", "age", "gender", "sample_size"), mean) %>%
  mutate(Place.of.residence = gsub("(.*) vald", "\\1 rural municipality", currentParishEHAK)) %>%
  mutate(Place.of.residence = gsub("(.*) linn", "\\1 city", Place.of.residence))


municipality_PESH_B5 = full_join(municipality_PESH, aggregated_municipality_B5, "Place.of.residence")

municipality_PESH_B5 = replace(municipality_PESH_B5, is.na(municipality_PESH_B5), 0)

municipality_PESH_B5$income = as.numeric(municipality_PESH_B5$income)

setwd("/data/scripts/Ling/Proj2PESH/inc Russian/")
write_csv(municipality_PESH_B5,"municipality_PESH_B5.csv")

# analysis

# if there's significant differences between municipalities?
library(DT)

# ANOVA test between traits and municipality regions

anova_openness <- aov(O_T ~ Place.of.residence, data = municipality_PESH_B5)
anova_conscientiousness <- aov(C_T ~ currentParishEHAK, data = municipality_B5)
anova_extraversion <- aov(E_T ~ currentParishEHAK, data = municipality_B5)
anova_agreeableness <- aov(A_T ~ currentParishEHAK, data = municipality_B5)
anova_neuroticism <- aov(N_T ~ currentParishEHAK, data = municipality_B5)


## Etas table
eta = function(x) Anova(x, type = 3)$Sum[-1] / sum(Anova(x, type = 3)$Sum[-1])

countyEtas = as.list(c("N_T","E_T","O_T","A_T","C_T")) %>%
  map(~ eta(aov(municipality_B5 %>% pull(.x) ~ currentCounty, municipality_B5))) %>%
  bind_cols %>% `names<-`(c("Neuroticism","Extraversion","Openness","Agreeableness","Conscientiousness")) %>%
  round(4) %>% 
  mutate(variables = c("municipality", "residuals")) %>%
  mutate(across(Neuroticism:Conscientiousness, ~ . * 100)) %>%
  select(variables, everything())

municipalityEtas = as.list(c("N_T","E_T","O_T","A_T","C_T")) %>%
  map(~ eta(aov(municipality_B5 %>% pull(.x) ~ currentParishEHAK, municipality_B5))) %>%
  bind_cols %>% `names<-`(c("Neuroticism","Extraversion","Openness","Agreeableness","Conscientiousness")) %>%
  round(4) %>% 
  mutate(variables = c("municipality", "residuals")) %>%
  mutate(across(Neuroticism:Conscientiousness, ~ . * 100)) %>%
  select(variables, everything())


nice_table(municipalityEtas, title = "Eta Squared (municipality)(%)", note = "η2 = 1% indicates a small effect. η2 = 6% indicates a medium effect. η2 = 14% indicates a large effect.") 

## correlation table
library(psych)

municipality_PESH_B5 = read_csv("/data/scripts/Ling/Proj2PESH/inc Russian/municipality_PESH_B5.csv") %>% 
  filter(sample_size>=50)

### Bivariate table

Bivariate_PESH_B5 = municipality_PESH_B5 %>% select(2:15,17,24,25,19:23) %>% cor()

### without controlling any variables
x = municipality_PESH_B5[,c(2:15,17,24,25)]
y = municipality_PESH_B5[19:23]

spearman <- corr.test(x,y, method="spearman", adjust="fdr")

r = round(spearman$r,2) %>% as.data.frame %>% rownames_to_column("PESH Indicators")
p.adj = round(spearman$p.adj,2) %>% as.data.frame %>% rownames_to_column("PESH Indicators")
p = round(spearman$p,3) %>% as.data.frame %>% rownames_to_column("PESH Indicators")


# load the prediction table
prediction_table <- read_csv("/data/scripts/Ling/Proj2PESH/Aggregated Data/Est_PESH_Prediction Table_2.csv") %>% 
  setNames(.[1, ]) %>%
  slice(-1)

library(openxlsx)

r_p <- p.adj %>% left_join(r, "PESH Indicators") %>%
  left_join(prediction_table, "PESH Indicators") %>% 
  mutate_at(vars(Openness:Neuroticism),as.numeric)
  
  
prediction_table_results = write.xlsx(r_p, "prediction_table_results.xlsx")
             
# agreement between predictions and results across five domains
agreement_O = cor.test(r_p$Openness, r_p$O_T.y, method = "spearman", exact = F)
agreement_N = cor.test(r_p$Neuroticism, r_p$N_T.y, method = "spearman", exact = F)
agreement_C = cor.test(r_p$Contientiousness, r_p$C_T.y, method = "spearman", exact = F)
agreement_E = cor.test(r_p$Extraversion, r_p$E_T.y, method = "spearman", exact = F)
agreement_A = cor.test(r_p$Agreeableness, r_p$A_T.y, method = "spearman", exact = F)

agreement_O_r = agreement_O$estimate
agreement_N_r = agreement_N$estimate
agreement_C_r = agreement_C$estimate
agreement_E_r = agreement_E$estimate
agreement_A_r = agreement_A$estimate

agreement_O_p = agreement_O$p.value
agreement_N_p = agreement_N$p.value
agreement_C_p = agreement_C$p.value
agreement_E_p = agreement_E$p.value
agreement_A_p = agreement_A$p.value


agreement_across_traits = read.csv("/data/scripts/Ling/Proj2PESH/Aggregated Data/prediction_table_results_muni_all.csv") 

agreement_across_traits = cor.test(agreement_across_traits$Prediction, agreement_across_traits$r, method ="spearman", exact = F)
agreement_across_traits$p.value
agreement_across_traits$estimate

# now we try controlling gender and age

control_municipality_PESH_B5 = municipality_PESH_B5 %>% mutate_at(c(2:15,17,19:23), ~ residuals(lm(. ~ gender + age , municipality_PESH_B5)))

## if gender and age were controlled, the correlation should be 0
cor(control_municipality_PESH_B5$O_T, control_municipality_PESH_B5$age)

## correlation table
x.control = control_municipality_PESH_B5[,c(2:15,17)]
y.control = control_municipality_PESH_B5[19:23]

spearman_control <- corr.test(x.control,y.control, method="spearman", adjust="fdr")

r_control_muni = round(spearman_control$r,2)%>% as.data.frame %>% rownames_to_column("PESH Indicators")
p.adj_control = round(spearman_control$p.adj,2) %>% as.data.frame %>% rownames_to_column("PESH Indicators")

r_p.adj_control = r_control_muni %>% left_join(p.adj_control, "PESH Indicators")%>%
  left_join(prediction_table, "PESH Indicators") %>% mutate_at(vars(Openness:Neuroticism),as.numeric)


write.xlsx(r_p.adj_control, "r_p.adj_control.xlsx")## when N = 74, r needs to be greater than .229 to make t statistics greater than 1.96 and non-directional < .05

# agreement after adjusting gender and age
control_agreement_O = cor.test(r_p.adj_control$Openness, r_p.adj_control$O_T.x,method = "spearman", exact = F)
control_agreement_N = cor.test(r_p.adj_control$Neuroticism, r_p.adj_control$N_T.x,method = "spearman", exact = F)
control_agreement_C = cor.test(r_p.adj_control$Contientiousness, r_p.adj_control$C_T.x,method = "spearman", exact = F)
control_agreement_E = cor.test(r_p.adj_control$Extraversion, r_p.adj_control$E_T.x,method = "spearman", exact = F)
control_agreement_A = cor.test(r_p.adj_control$Agreeableness, r_p.adj_control$A_T.x,method = "spearman", exact = F)

agreement_O_r_control = control_agreement_O$estimate
agreement_N_r_control = control_agreement_N$estimate
agreement_C_r_control = control_agreement_C$estimate
agreement_E_r_control = control_agreement_E$estimate
agreement_A_r_control = control_agreement_A$estimate

agreement_O_p_control = control_agreement_O$p.value
agreement_N_p_control = control_agreement_N$p.value
agreement_C_p_control = control_agreement_C$p.value
agreement_E_p_control = control_agreement_E$p.value
agreement_A_p_control = control_agreement_A$p.value


control_agreement_across_traits = read.csv("r_p.adj_control_muni_control.csv")

control_agreement_across_traits = cor.test(control_agreement_across_traits$Prediction, control_agreement_across_traits$r, method = "spearman", exact = F)
control_agreement_across_traits$estimate
control_agreement_across_traits$p.value

setwd("/data/scripts/Ling/Proj2PESH/Aggregated Data")
cm = read.csv("county_muni_all_control.csv") 

h0 = read.csv("prediction_table_results_muni_all.csv") %>% filter(!(PESH.Indicators %in% c("gender","age"))) %>%
  bind_cols(cm, "PESH.Indicators") %>% mutate(mean = ((muni+county)/2))

cor.test(h0$Prediction,h0$mean,method = "spearman", exact = F)

B5_ave_agreement = r_control_county %>% left_join(r_control_muni, "PESH Indicators") %>%
  left_join(prediction_table, "PESH Indicators") %>%
  mutate(O = ((Openness.x + O_T)/2),
         C =((Conscientiousness + C_T)/2),
         E = ((Extraversion.x + E_T)/2),
         A = ((Agreeableness.x +A_T)/2),
         N = ((Neuroticism.x + N_T)/2)
  ) %>%
  mutate_at(vars(12:16), as.numeric)

cor.test(B5_ave_agreement$O, B5_ave_agreement$Openness.y, method = "spearman", exact = F)
cor.test(B5_ave_agreement$C, B5_ave_agreement$Contientiousness, method = "spearman", exact = F)
cor.test(B5_ave_agreement$E, B5_ave_agreement$Extraversion.y, method = "spearman", exact = F)
cor.test(B5_ave_agreement$A, B5_ave_agreement$Agreeableness.y, method = "spearman", exact = F)
cor.test(B5_ave_agreement$N, B5_ave_agreement$Neuroticism.y, method = "spearman", exact = F)

test$estimate
# Spatial lag model
library(sf)
library(tidycensus)
library(corrr)
library(tmap)
library(spdep)
library(tigris)
library(rmapshaper)
library(flextable)
library(car)
library(spatialreg)
library(stargazer)

## spatial autocorrelation

setwd("/data/scripts/Ling/Proj1Maps/EstPers/")
county_layer <- read_sf("Shapefiles/countySHP")
parish_layer <- read_sf("Shapefiles/municipalitySHP")

# Import municipality-level variables

my_data <- read_csv("municipality_PESH_B5.csv")

my_data = rename(my_data, ONIMI = currentParishEHAK)

parish <- left_join(parish_layer, my_data, "ONIMI") %>% filter(sample_size > 50)

nb<-poly2nb(parish, queen = T)

nbw <-nb2listw(nb, style = "W", zero.policy = TRUE)



moran.plot(as.numeric(scale(parish$C_T)), listw=nbw,
           zero.policy = TRUE,
           xlab="Standardized Openness", 
           ylab="Neighbors Standardized Openness",
           main=c("moran.mc(parishmoran.mc(parish", "in Estonia") )


moran.test(parish$E_T,listw = nbw, zero.policy = TRUE,na.action = na.omit)
moran.test(parish$O_T,listw = nbw, zero.policy = TRUE,na.action = na.omit)
moran.test(parish$C_T,listw = nbw, zero.policy = TRUE,na.action = na.omit) # not sig
moran.test(parish$A_T,listw = nbw, zero.policy = TRUE,na.action = na.omit)
moran.test(parish$N_T,listw = nbw, zero.policy = TRUE,na.action = na.omit) # not sig

## example: testing model fit

multivariate <- lm(enterprise_birth_rate ~ O_T + gender + age, 
                   data = parish)

res.multivariate <- resid(multivariate) #check residuals

moran.test(res.multivariate,listw = nbw, zero.policy = TRUE,na.action = na.omit) # moran's test on residual

lm.LMtests(multivariate, listw = nbw, test = "all") #Lagrange Multiplier




# example:fit spatial lag model
model_example <- lagsarlm(far_right_rate ~ O_T + gender + age, 
                          data = parish, 
                          listw = nbw, zero.policy = TRUE)

# Print summary of the model
summary(model_example)$LR1$p.value # extract p value of rho
model_example$coefficients["O_T"]
summary(model_example)$Coef["O_T",4] # extract p value of coefficient

model_example2 <- errorsarlm(far_right_rate ~ O_T + gender + age, 
                        data = parish, 
                        listw = nbw, zero.policy = TRUE)
summary(model_example2)$LR1$p.value # extract p value of rho
model_example2$coefficients["O_T"]
summary(model_example2)$Coef["O_T",4] # extract p value of coefficient
summary(model_example2)$coefficients

# generate correlation table for traits and indicators
indicators = c("far_right_rate", "right_leaning_rate" ,"left_leaning_rate",        
"enterprise_birth_rate","enterprise_death_rate","income","unemployment_rate","marital_rate" ,
"foreign_born_rate","religious_ppl_rate","crime_rate","daily_activity_limit_rate",
"long_term_illness_rate","university_rate","population_density")

traits = c("E_T","O_T","A_T", "N_T", "C_T")

# scale indicators and trait
parish = parish %>%
  mutate_at(c(8:30), funs(c(scale(.))))

# checking residuals for all OLS models
# Initialize a data frame to store results
results <- data.frame(
  Personality = character(),
  Indicator = character(),
  Morans_I = numeric(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)


# Loop through each combination of personality traits and indicators
for (trait in traits) {
  for (indicator in indicators) {
    
    # Fit the linear model
    formula <- as.formula(paste(indicator, "~", trait, "+ gender + age"))
    model <- lm(formula, data = parish)
    
    # Extract residuals
    residuals <- model$residuals
    
    # Perform Moran's I test
    morans_test <- moran.test(residuals, listw = nbw, zero.policy = TRUE,na.action = na.omit)
    
    # Extract Moran's I statistic and p-value
    morans_I <- morans_test$estimate[1]
    p_value <- morans_test$p.value
    
    # Store the results
    results <- rbind(results, data.frame(
      Personality = trait,
      Indicator = indicator,
      Morans_I = morans_I,
      P_value = p_value
    ))
  }
}

# Print the results
print(results)


## lagrange test (LM)

# Initialize LM_results before the loop
LM_results <- data.frame(
  Trait = character(),
  Indicator = character(),
  LMlag_statistic = numeric(),
  LMlag_p_value = numeric(),
  LMerr_statistic = numeric(),
  LMerr_p_value = numeric(),
  bigger_test = character(),
  stringsAsFactors = FALSE
)

for (trait in traits) {
  for (indicator in indicators) {
    formular <- as.formula(paste(indicator, "~", trait, "+gender+age"))
    model <- lm(formular, data = parish)
    
    lm_test <- lm.RStests(model, listw = nbw, test = "all")
    
    LMlag_statistic <- round(lm_test$RSlag$statistic,3)
    LMlag_p_value <- round(lm_test$RSlag$p.value,3)
    LMerr_statistic <- round(lm_test$RSerr$statistic,3)
    LMerr_p_value <- round(lm_test$RSerr$p.value,3)
    
    # Corrected condition for bigger_test
    bigger_test <- ifelse(LMlag_statistic > LMerr_statistic, "LMlag", "LMerr")
    
    # Append results to LM_results with bigger_test column
    LM_results <- rbind(LM_results, data.frame(
      Trait = trait,
      Indicator = indicator,
      LMlag_statistic = LMlag_statistic,
      LMlag_p_value = LMlag_p_value,
      LMerr_statistic = LMerr_statistic,
      LMerr_p_value = LMerr_p_value,
      bigger_test = bigger_test
    ))
  }
}


# Print the results
print(LM_results)

write_csv(LM_results, "lagrangeTest.csv")




## OLS model

### example

# example:Fit OLS model
OLS_model_example <- lm(far_right_rate ~ O_T + gender + age, 
                        data = parish)


summary(OLS_model_example)$coefficients

# loop through all the OLS regression models
# Initialize the data frame to store results
lm_result <- data.frame(
  Trait = character(),
  Indicator = character(),
  Coefficient = numeric(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through traits and indicators
for (trait in traits) {
  for (indicator in indicators) {
    
    # Create the formula for the linear model
    formula <- as.formula(paste(indicator, "~", trait, "+gender+age"))
    
    # Fit the linear model
    model <- lm(formula, data = parish)
    
    # Extract coefficients and p-values
    coefficients <- summary(model)$coefficients
    
    # Extract the coefficient and p-value for the 'trait'
    coefficient_value <- round(coefficients[trait, "Estimate"], 3)
    p_value <- round(coefficients[trait, "Pr(>|t|)"], 3)
    
    # Append the results to the data frame
    lm_result <- rbind(lm_result, data.frame(
      Trait = trait,
      Indicator = indicator,
      Coefficient = coefficient_value,
      P_value = p_value
    ))
  }
}


print(lm_result)

write_csv(lm_result,"lm_result.csv")


## spatial lag model

lag_autoc <- data.frame(
  Trait = character(),
  Indicator = character(),
  coefficient = numeric(),
  p_value = numeric(),
  rho = numeric(),
  rho_p = numeric(),
  stringsAsFactors = FALSE
)
# Loop through each personality trait
for (trait in traits) {
  # Loop through each indicator
  for (indicator in indicators) {
    
    # Create the formula dynamically
    formula <- as.formula(paste(indicator, "~", trait, "+ gender + age"))

    # Fit the spatial lag model
    model <- lagsarlm(
      formula,
      data = parish,
      listw = nbw,
      zero.policy = TRUE 
    )
    
    # Extract coefficients and p-value
    coefficients <- model$coefficients[trait]
    p_value <- summary(model)$Coef[trait,4]
    rho <- model$rho
    rho_p <- c(summary(model)$LR1$p.value) # extract p value of rho

    lag_autoc <- rbind(lag_autoc, c(trait, indicator, coefficients, p_value, rho, rho_p))
    
  }
}

lag_result = lag_autoc %>% `names<-`(c("traits", "indicators", "coef", "coef_p", "rho", "rho_p")) %>%
  mutate_at(3:6, as.numeric) %>% mutate_at(3:6, round,3)

write.csv(lag_result, "lag_result.csv")


# level of autocorrelation in spaital lag model

OLS_autoc <- data.frame(
  Trait = character(),
  Indicator = character(),
  coefficient = numeric(),
  p_value = numeric(),
  rho = numeric(),
  rho_p = numeric(),
  stringsAsFactors = FALSE
)
# Loop through each personality trait
for (trait in traits) {
  # Loop through each indicator
  for (indicator in indicators) {
    
    # Create the formula dynamically
    formula <- as.formula(paste(indicator, "~", trait, "+ gender + age"))
    
    # Fit the spatial lag model
    model <- lagsarlm(
      formula,
      data = parish,
      listw = nbw,
      zero.policy = TRUE 
    )
    
    # Extract coefficients and p-value
    coefficients <- model$coefficients[trait]
    p_value <- summary(model)$Coef[trait,4]
    rho <- model$rho
    rho_p <- c(summary(model)$LR1$p.value) # extract p value of rho
    
    lag_autoc <- rbind(lag_autoc, c(trait, indicator, coefficients, p_value, rho, rho_p))
    
  }
}

lag_result = lag_autoc %>% `names<-`(c("traits", "indicators", "coef", "coef_p", "rho", "rho_p")) %>%
  mutate_at(3:6, as.numeric) %>% mutate_at(3:6, round,3) %>%
  rename('PESH Indicators' = indicators) %>% select(1:3) %>%
  pivot_wider(names_from = traits, values_from = coef)%>%
  left_join(prediction_table, "PESH Indicators") %>% 
  mutate_at(vars(Openness:Neuroticism),as.numeric)

write.xlsx(lag_result, "lag_result.xlsx")


lag_agreement_O = cor.test(lag_result$Openness, lag_result$O_T,method = "spearman", exact = F)
lag_agreement_N = cor.test(lag_result$Neuroticism, lag_result$N_T,method = "spearman", exact = F)
lag_agreement_C = cor.test(lag_result$Contientiousness,lag_result$C_T,method = "spearman", exact = F)
lag_agreement_E = cor.test(lag_result$Extraversion,lag_result$E_T,method = "spearman", exact = F)
lag_agreement_A = cor.test(lag_result$Agreeableness,lag_result$A_T,method = "spearman", exact = F)

lag_O_r = lag_agreement_O$estimate
lag_N_r = lag_agreement_N$estimate
lag_C_r = lag_agreement_C$estimate
lag_E_r = lag_agreement_E$estimate
lag_A_r = lag_agreement_A$estimate

lag_O_p = lag_agreement_O$p.value
lag_N_p = lag_agreement_N$p.value
lag_C_p = lag_agreement_C$p.value
lag_E_p = lag_agreement_E$p.value
lag_A_p = lag_agreement_A$p.value


agreement_across_lag = read.csv("/data/scripts/Ling/Proj2PESH/lag_result_prediction.csv") 

lag_agreement_across_traits = cor.test(agreement_across_lag$Prediction, agreement_across_lag$Lag.coef, method = "spearman", exact = F)
lag_agreement_across_traits$estimate
lag_agreement_across_traits$p.value


#spatial error model

# Load necessary library (if not installed, you can install it first using install.packages("spatialreg"))
library(spatialreg)

# Initialize the data frame to store results
err_result <- data.frame(
  Trait = character(),
  Indicator = character(),
  Coefficient = numeric(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through traits and indicators
for (trait in traits) {
  for (indicator in indicators) {
    
    # Create the formula for the spatial error model
    formula <- as.formula(paste(indicator, "~", trait, "+gender+age"))
    
    # Fit the spatial error model using errorsarlm (replace this with lagsarlm if needed)
    model <- errorsarlm(formula, data = parish, listw = nbw, zero.policy = TRUE)  # `nbw` is the spatial weights matrix
    
    coefficients <- round(model$coefficients[trait],3)
    p_value <- round(summary(model)$Coef[trait,4],3)

    # Append the results to the data frame
    err_result <- rbind(err_result, data.frame(
      Trait = trait,
      Indicator = indicator,
      Coefficient = coefficients,
      P_value = p_value
    ))
  }
}

write_csv(err_result, "err_result.csv")


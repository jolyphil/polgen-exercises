`# Import and clean EVS data
# ==============================================================================

library(dplyr) # Data wrangling
library(haven) # Import stata files

# Unzip data --------------------------------------------------------------

evs_raw <- unzip("data-raw/ZA7503_v2-0-0.dta.zip", 
                  files = "ZA7503_v2-0-0.dta",
                  exdir = tempdir()) |> 
  read_dta()

evs <- evs_raw |>
  mutate(wave = as.numeric(S002EVS),
         year = as.numeric(S020),
         yearborn = case_when(X002 < 0 ~ NA_real_,
                              TRUE ~ as.numeric(X002)), # Important: EVS 2017: People born before 1937 recoded into value '1937 and before'
         gen = case_when(yearborn %in% 1940:1959 ~ "1940-1959",
                         yearborn %in% 1960:1979 ~ "1960-1979",
                         yearborn %in% 1980:1999 ~ "1980-1999",
                         yearborn < 1940 ~ "Before 1940"),
         gen = factor(gen, levels = c("Before 1940", 
                                      "1940-1959", 
                                      "1960-1979", 
                                      "1980-1999")),
         age = year - yearborn,
         retired = case_when(X028 == 4 ~ 1,
                             X028 %in% c(1:3,5:8) ~ 0),
         student = case_when(X028 == 6 ~ 1,
                             X028 %in% c(1:5,7:8) ~ 0),
         postmat1 = case_when(E003 %in% c(2, 4) ~ 1,
                              E003 %in% c(1, 3) ~ -1),
         postmat2 = case_when(E004 %in% c(2, 4) ~ 1,
                              E004 %in% c(1, 3) ~ -1),
         postmat = postmat1 + postmat2,
         postmat = case_when(postmat < 0  ~ "Materialist",
                             postmat == 0 ~ "Mixed",
                             postmat > 0  ~ "Postmaterialist"),
         postmat = factor(postmat),
         weight = as.numeric(S017),
         weight_eq = as.numeric(S018)
  ) |>
  rename(country = S009) |> 
  group_by(country) |> 
  mutate(n_waves = length(unique(S002EVS))) |> 
  ungroup() |> 
  filter(n_waves == 5) |> 
  select(country, 
         wave, 
         year, 
         n_waves, 
         yearborn, 
         gen, 
         age, 
         retired, 
         student,
         postmat,
         weight,    # Normal weight
         weight_eq) # Equilibrated weight-100

saveRDS(evs, file = "data/evs.rds")

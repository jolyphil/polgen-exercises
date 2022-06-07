# Import and clean EVS data
# ==============================================================================

library(dplyr) # Data wrangling
library(haven) # Import stata files

# Data source: EVS Trend File 1981-2017, ZA7503, v.2.0.0 (2021-07-07), doi:10.4232/1.13736.
evs_raw <- read_dta("data-raw/ZA7503_v2-0-0.dta")

evs <- evs_raw |>
  filter(S009 == "DE" & S002EVS >= 2) |> # Germany, after 1989
  mutate(homo = case_when(F118 %in% 1:10 ~ as.numeric(F118),
                          TRUE ~ NA_real_),
         year = S020,
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
                             X028 %in% c(1:5,7:8) ~ 0)) |> 
  select(year, yearborn, gen, age, homo, retired, student)

saveRDS(evs, file = "data/evs.rds")

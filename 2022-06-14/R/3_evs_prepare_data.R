`# Import and clean EVS data
# ==============================================================================

library(dplyr) # Data wrangling
library(haven) # Import stata files

# Load functions ----------------------------------------------------------

recode_polpart_evs <- function(var){
  newvar <- as.character(NA)
  newvar[var == 1] <- "Yes"
  newvar[var == 2] <- "No"
  newvar[var == 3] <- "No"
  newvar <- factor(newvar)
  newvar
}


# Unzip data --------------------------------------------------------------

evs_raw <- unzip("data-raw/ZA7503_v2-0-0.dta.zip", 
                  files = "ZA7503_v2-0-0.dta",
                  exdir = tempdir()) |> 
  read_dta()

evs <- evs_raw |>
  mutate(
    across(.cols = c(E025,
                     E026,
                     E027),
           .fns = recode_polpart_evs)
  ) |>
  mutate(protest = case_when(E025 == "Yes" | E026 == "Yes" | E027 == "Yes" ~ 1,
                             E025 == "No" & E026 == "No" & E027 == "No" ~ 0),
         wave = as.numeric(S002EVS),
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
                             X028 %in% c(1:5,7:8) ~ 0)) |>
  rename(country = S009) |> 
  group_by(country) |> 
  mutate(n_waves = length(unique(S002EVS))) |> 
  ungroup() |> 
  filter(n_waves == 5) |> 
  select(country, wave, year, n_waves, yearborn, gen, age, protest, retired, student)

saveRDS(evs, file = "data/evs.rds")
`
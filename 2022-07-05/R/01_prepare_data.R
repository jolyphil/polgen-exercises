library(dplyr)
library(haven)

gles_raw <- read_dta("data-raw/ZA7702_v1-0-0.dta")

gles <- gles_raw |> 
  filter(sample == 8) |> # Keep post-election survey only
  filter(!is.na(n_w_ipfges)) |> 
  mutate(vote = case_when(q114ba == 1 ~   "CDU/CSU",
                          q114ba == 4 ~   "SPD",
                          q114ba == 5 ~   "FDP",
                          q114ba == 6 ~   "GRUENE",
                          q114ba == 7 ~   "DIE LINKE",
                          q114ba == 322 ~ "AfD",
                          q114ba == 801 ~ "andere Partei"),
         vote = factor(vote, levels = c("CDU/CSU", 
                                        "SPD", 
                                        "FDP", 
                                        "GRUENE", 
                                        "DIE LINKE", 
                                        "AfD", 
                                        "andere Partei")),
         vote_afd = case_when(vote == "AfD" ~ 1,
                              !is.na(vote) ~ 0), 
         edu_high = if_else(d8j == 1 | d8j1 == 1 | d8j2 == 1 | d8j3 == 1, 
                     "University degree",
                     "No university degree"),
         edu_high = factor(edu_high), 
         income = if_else(d63 > 0, d63, NA_real_),
         income = case_when(income <= quantile(income, na.rm = TRUE)[2] ~ "Q1",
                            income > quantile(income, na.rm = TRUE)[2] & income <= quantile(income, na.rm = TRUE)[3]~ "Q2",
                            income > quantile(income, na.rm = TRUE)[3] & income <= quantile(income, na.rm = TRUE)[4]~ "Q3",
                            income > quantile(income, na.rm = TRUE)[4] ~ "Q4"),
         income = factor(income),
         residence = case_when(wum6 == 1 ~ "Big city",
                               wum6 == 2 ~ "Outskirts of a big city",
                               wum6 == 3 ~ "Medium or small town",
                               wum6 %in% c(4, 5) ~ "Rural"),
         residence = factor(residence, levels = c("Big city",
                                                  "Outskirts of a big city",
                                                  "Medium or small town",
                                                  "Rural")),
         east = if_else(ostwest == 0, "East", "West"),
         east = factor(east, levels = c("West", "East")),
         yearborn = case_when(d2a == "1931 oder frueher" ~ 1931,
                              d2a == ~ "-99 keine Angabe" ~ NA_real_,
                              TRUE ~ as.numeric(d2a)),
         cohort = floor(yearborn / 5) * 5,
         cohort = factor(cohort),
         age = 2021 - yearborn,
         age_group = case_when(age < 30 ~ "16-29",
                               age >= 30 & age < 55 ~ "30-54",
                               age >= 55 ~ "55+"),
         age_group = factor(age_group)
         ) |>
  filter(yearborn >= 1935 & yearborn <= 2003) |> 
  select(vote,
         vote_afd,
         edu_high,
         income,
         residence,
         east,
         yearborn,
         cohort,
         age,
         age_group,
         vpoint,
         n_w_ipfges)

saveRDS(gles, file = "data/gles.rds")

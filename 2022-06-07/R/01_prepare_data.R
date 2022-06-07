library(dplyr)
library(haven)

gles_raw <- read_dta("data-raw/ZA7702_v1-0-0.dta")

gles <- gles_raw |> 
  mutate(party_id = case_when(q75a %in% c(1,2,3) ~ "CDU/CSU",
                              q75a == 4 ~ "SPD",
                              q75a == 322 ~ "AfD",
                              q75a == 5 ~ "FDP",
                              q75a == 7 ~ "DIE LINKE",
                              q75a == 6 ~ "GRÜNE",
                              q75a == 801 ~ "Other party",
                              q75a == 808 ~ "No party"),
         party_id = factor(party_id, levels = c("CDU/CSU",
                                                "SPD",
                                                "AfD",
                                                "FDP",
                                                "DIE LINKE",
                                                "GRÜNE",
                                                "Other party",
                                                "No party")),
         party_id2 = case_when(party_id == "No party" ~ "No party id.",
                               !is.na(party_id) ~ "Party id."),
         yearborn = case_when(d2a == "1931 oder frueher" ~ 1931,
                              d2a == ~ "-99 keine Angabe" ~ NA_real_,
                              TRUE ~ as.numeric(d2a)),
         age = 2021 - yearborn,
         age_group = case_when(age < 30 ~ "16-29",
                               age >= 30 & age < 55 ~ "30-54",
                               age >= 55 ~ "55+"),
         age_group = factor(age_group), 
         survey = case_when(sample == 7 ~ "pre",
                            sample == 8 ~ "post"),
         survey = factor(survey, levels = c("pre", "post"))) |> 
  select(survey,
         age,
         age_group,
         party_id,
         party_id2)

saveRDS(gles, file = "data/gles.rds")

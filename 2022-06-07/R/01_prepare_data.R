library(dplyr)
library(haven)
library(ggplot2)

gles_raw <- read_dta("data-raw/ZA7702_v1-0-0.dta")

gles <- gles_raw |> 
  mutate(party_id = case_when(q75a %in% c(1,2,3) ~ "CDU/CSU",
                              q75a == 4 ~ "SPD",
                              q75a == 322 ~ "AfD",
                              q75a == 5 ~ "FDP",
                              q75a == 7 ~ "DIE LINKE",
                              q75a == 6 ~ "GRÜNE",
                              q75a == 801 ~ "Andere Partei",
                              q75a == 808 ~ "keiner Partei"),
         party_id = factor(party_id, levels = c("CDU/CSU",
                                                "SPD",
                                                "AfD",
                                                "FDP",
                                                "DIE LINKE",
                                                "GRÜNE",
                                                "Andere Partei",
                                                "keiner Partei")),
         yearborn = case_when(d2a == "1931 oder frueher" ~ 1931,
                              d2a == ~ "-99 keine Angabe" ~ NA_real_,
                              TRUE ~ as.numeric(d2a)))

gles |> 
  filter(yearborn > 1990 & !is.na(party_id)) |> 
  group_by(party_id) |> 
  count() |> 
  ungroup() |> 
  mutate(pct = n / sum(n) * 100) |> 
  ggplot(aes(x = pct, y = party_id)) +
  geom_col()

gles |> 
  filter(yearborn < 1990 & !is.na(party_id)) |> 
  group_by(party_id) |> 
  count() |> 
  ggplot(aes(x = n, y = party_id)) +
  geom_col()

table(gles_young$party_id)

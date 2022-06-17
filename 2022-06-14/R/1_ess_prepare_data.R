library(dplyr)
library(haven)

# Load functions ----------------------------------------------------------

recode_polpart <- function(var){
  newvar <- as.character(NA)
  newvar[var == 1] <- "Yes"
  newvar[var == 2] <- "No"
  newvar <- factor(newvar)
  newvar
}


# Unzip data --------------------------------------------------------------

ess9_raw <- unzip("data-raw/ESS9e03_1.dta.zip", 
              files = "ESS9e03_1.dta",
              exdir = tempdir()) |> 
  read_dta()

# Political participation battery in the ESS ------------------------------
# vote     Voted last national election
# contplt  Contacted politician or government official last 12 months
# wrkprty  Worked in political party or action group last 12 months
# wrkorg   Worked in another organisation or association last 12 months
# badge    Worn or displayed campaign badge/sticker last 12 months
# sgnptit  Signed petition last 12 months
# pbldmn   Taken part in lawful public demonstration last 12 months
# bctprd   Boycotted certain products last 12 months
# pstplonl Posted or shared anything about politics online last 12 months

# Other variables ---------------------------------------------------------
# gndr     Gender
# agea     Age of respondent, calculated
# hinctnta Household's total net income, all sources

# Coding of 'region' ------------------------------------------------------
# DE1	Baden-Württemberg
# DE2	Bayern
# DE3	Berlin
# DE4	Brandenburg
# DE5	Bremen
# DE6	Hamburg
# DE7	Hessen
# DE8	Mecklenburg-Vorpommern
# DE9	Niedersachsen
# DEA	Nordrhein-Westfalen
# DEB	Rheinland-Pfalz
# DEC	Saarland
# DED	Sachsen
# DEE	Sachsen-Anhalt
# DEF	Schleswig-Holstein
# DEG	Thüringen

ess9_de <- ess9_raw  |> 
  mutate(cntry = as.character(cntry)) |> 
  filter(cntry == "DE") |> 
  mutate(
    across(.cols = c(contplt,
                     wrkprty,
                     wrkorg,
                     badge,
                     sgnptit,
                     pbldmn,
                     bctprd,
                     pstplonl),
           .fns = recode_polpart)
  ) |>
  mutate(vote = case_when(vote == 1 ~ "Yes",
                          vote == 2 ~ "No",
                          vote == 3 ~ "Not eligible"),
         vote = factor(vote, levels = c("No", "Yes", "Not eligible")),
         gender = case_when(gndr == 1 ~ "Male",
                          gndr == 2 ~ "Female"),
         gender = factor(gndr, levels = c("Male", "Female")),
         age = as.numeric(agea),
         age_group = case_when(age < 30 ~ "16-29",
                               age >= 30 & age < 55 ~ "30-54",
                               age >= 55 ~ "55+"),
         age_group = factor(age_group),
         pol_inter = case_when(polintr == 1 ~ "Very interested",
                               polintr == 2 ~ "Quite interested",
                               polintr == 3 ~ "Hardly interested",
                               polintr == 4 ~ "Not at all interested"),
         pol_inter = factor(pol_inter, levels = c("Not at all interested",
                                                  "Hardly interested",
                                                  "Quite interested",
                                                  "Very interested")),
         close_party = case_when(clsprty == 1 ~ "Yes",
                                 clsprty == 2 ~ "No"),
         close_party = factor(close_party, levels = c("No", "Yes")),
         trust_parl = as.numeric(trstprl),
         trust_legal = as.numeric(trstlgl),
         trust_parties = as.numeric(trstprt),
         lrscale = as.numeric(lrscale),
         region_de = if_else(
           region %in% c("DE3", "DE4", "DE8", "DED", "DEE", "DEG"), 
           "East", 
           "West"),
         region_de = factor(region_de, levels = c("West", "East")),
         hinctnta = as.numeric(hinctnta),
         edu = case_when(eisced %in% c(1:2) ~ "Low", 
                         eisced %in% c(3:4) ~ "Middle",
                         eisced %in% c(5:7) ~ "High"),
         edu = factor(edu, levels = c("Low", "Middle", "High"))
  ) |>
  rename(contact = contplt,
         work_party = wrkprty,
         work_org = wrkorg,
         petition = sgnptit,
         demo = pbldmn,
         boycott = bctprd,
         online_part = pstplonl, 
         netuse = netustm,
         income = hinctnta) |>
  select(
    vote,
    contact,
    work_party,
    work_org,
    badge,
    petition,
    demo,
    boycott,
    online_part,
    lrscale,
    pol_inter,
    close_party,
    trust_parl,
    trust_legal,
    trust_parties,
    gender,
    age,
    age_group,
    income,
    edu,
    region_de
  )


# Add labels --------------------------------------------------------------

attr(ess9_de$vote, "label") <- "Voted last national election"
attr(ess9_de$contact, "label") <- "Contacted politician or government official, last 12 months"
attr(ess9_de$work_party, "label") <- "Worked in political party or action group, last 12 months"
attr(ess9_de$work_org, "label") <- "Worked in another organisation or association, last 12 months"
attr(ess9_de$badge, "label") <- "Worn or displayed campaign badge/sticker, last 12 months"
attr(ess9_de$petition, "label") <- "Signed petition, last 12 months"
attr(ess9_de$demo, "label") <- "Taken part in lawful public demonstration, last 12 months"
attr(ess9_de$boycott, "label") <- "Boycotted certain products, last 12 months"
attr(ess9_de$online_part, "label") <- "Posted or shared anything about politics online, last 12 months"
attr(ess9_de$pol_inter, "label") <- "How interested in politics"
attr(ess9_de$close_party, "label") <- "Feel closer to a particular party than all other parties"
attr(ess9_de$trust_parl, "label") <- "Trust in country's parliament"
attr(ess9_de$trust_legal, "label") <- "Trust in the legal system"
attr(ess9_de$trust_parties, "label") <- "Trust in political parties"
attr(ess9_de$lrscale, "label") <- "Placement on left right scale"
attr(ess9_de$gender, "label") <- "Gender"
attr(ess9_de$age, "label") <- "Age"
attr(ess9_de$age_group, "label") <- "Age Group"
attr(ess9_de$income, "label") <- "Household's total net income, all sources"
attr(ess9_de$edu, "label") <- "Highest level of education"
attr(ess9_de$region_de, "label") <- "Western/Eastern Germany"

# Save data --------------------------------------------------------------------

saveRDS(ess9_de, file = "data/ess9_de.rds")

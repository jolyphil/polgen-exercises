# ******************************************************************************
# Task:    Import ESS data and create tidy dataset
# ******************************************************************************

source("R/eastsoc.R")
source("R/load_packages.R")

p <- c("countrycode", # Harmonize country codes
       "dplyr", # Used for data wrangling
       "essurvey", # Download main ESS datafiles
       "haven", # Use labelled vectors
       "purrr", # Functional programming
       "RCurl", # Download file from Internet
       "readr", # Import CSV data
       "stringr" # String manipulation
       )

load_packages(p)

# ______________________________________________________________________________
# Unzip ESS files and convert dta to rds ====

# Functions --------------------------------------------------------------------

save_ess_rds <- function(ess_zip_file) {
  filename <- str_extract(ess_zip_file, "ESS[:digit:]") %>%
    paste0(".rds") %>%
    file.path("data", "temp", .)
  paste0("Saving ", filename) %>%
    message()
  unzip(ess_zip_file, 
        files = find_dta(ess_zip_file),
        exdir = tempdir()) %>%
    read_dta(encoding = 'latin1') %>%
    filter(cntry == "DE") %>%
    saveRDS(filename)
}

find_dta <- function(esszipfile){
  unzip(esszipfile, list = TRUE) %>% 
    filter(str_detect(Name, ".dta")) %>%
    select(Name) %>%
    as.character()
}

# Extract list of zip files ----------------------------------------------------

ess_zip_files <- file.path("data-raw") %>% 
  list.files() %>%
  .[(str_detect(., "ESS[:digit:]e0[:digit:]_[:digit:].stata.zip"))] %>%
  paste0("data-raw/", .)

# Unzip dta files and save to rds ----------------------------------------------

walk(ess_zip_files, save_ess_rds)

# ______________________________________________________________________________
# Append datasets ====

for (i in 1:8) {
  
  # _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  # Import rds file ----
  
  round_i <- file.path("data", "temp", paste0("ESS", i, ".rds")) %>% 
   readRDS() %>%
   recode_missings()
  
  # _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  # year, harmonize variable name ----
 
 if (i < 3) { # inwyr, "year of interview" --> only in ESS1 and ESS2
   round_i <- round_i %>%
     group_by(cntry) %>%
     mutate(yearsurvey = inwyr) %>%
     ungroup
   
 } else { # inwyys, "Start of interview, year" --> in essround > 2
   round_i <- round_i %>%
     group_by(cntry) %>%
     mutate(yearsurvey = inwyys) %>%
     ungroup
 } 
  
 # _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
 # Add old education var (in ESS 1 to 4) if missing
  
 if (any(names(round_i) == "edulvla") == F) { 
   round_i <- round_i %>%
     mutate(edulvla = NA)
 }

  # _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
  # Add old region var (used in ESS 1 to 4) if missing
  
  if (any(names(round_i) == "regionde") == F) { 
    round_i <- round_i %>%
      mutate(regionde = NA_real_)
  }
  
  # _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
  # Add new region var (used in ESS 5 to 8) if missing
  
  if (any(names(round_i) == "region") == F) { 
    round_i <- round_i %>%
      mutate(region = NA_character_)
  }
  
  # _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  # Merge with German country-specific data ----
  
  round_i <- paste0("ESS", i, "csDE.rds") %>% # get file name
    file.path("data", "temp", .) %>%
    readRDS() %>%
    left_join(round_i, ., by = c("cntry", "idno"))
  
  
  # _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  # Select variables ----
  
  round_i <- round_i %>%
    dplyr::select(essround, 
                  idno, 
                  cntry, 
                  yearsurvey,
                  dweight,
                  
                  stfdem,
                  gincdif,
                  imwbcnt,
                  
                  wrkprty,
                  wrkorg,
                  sgnptit,
                  pbldmn,
                  bctprd,
                  yrbrn,
                  gndr,
                  eisced,
                  edulvla,
                  mnactic,
                  mbtru,
                  domicil,
                  brncntr,
                  intewde,
                  wherebefore1990,
                  yrmovedwest,
                  yrmovedeast,
                  region,
                  regionde)
 
 if (i == 1) {
   ess <- round_i
 } else {
   ess <- rbind(ess, round_i)
 }
 
}

# Remove temporary dataset
rm(round_i, i)

# ______________________________________________________________________________
# Recode and rename variables ====

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Country | cntry --> country_iso3c ----

ess <- ess %>%
  dplyr::mutate(country = case_when(
    cntry == "DE" & intewde == 1 ~ "DEE",  # East Germany
    cntry == "DE" & intewde == 2 ~ "DEW")) # West Germany

attr(ess$country, "label") <- "Country code, ISO3C"

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Country-wave | country, essround --> countrywave ----

ess <- ess %>%
 mutate(countrywave = paste0(country, essround))

attr(ess$countrywave, "label") <- "Country-wave"

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Year | yearsurvey --> year ----

ess <- ess %>%
  group_by(country, countrywave) %>%
  mutate(year = round(mean(yearsurvey, na.rm = T))) %>%
  ungroup()

attr(ess$year, "label") <- "Year"

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# 2-year period | essround --> period ----

ess <- ess %>%
  mutate(period = 2002 + (essround - 1) * 2)

attr(ess$period, "label") <- "2-year period"

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Design weight | dweight --> dweight ----

# Note: set mean to 1 in order to recalibrate 

ess <- ess %>%
  group_by(country, essround) %>%
  summarize(weight_mean = mean(dweight)) %>%
  left_join(ess, ., by = c("country", "essround")) %>%
  mutate(dweight = dweight / weight_mean) %>%
  select(-weight_mean)

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Worked for party or group | wrkprty, wrkorg --> partygroup ----

ess <- ess %>%
  mutate(partygroup = case_when(wrkprty == 1 | wrkorg == 1 ~ 1,
                                wrkprty == 2 & wrkorg == 2 ~ 0),
         partygroup = labelled(partygroup,
                               c("Not done" = 0, "Has done" = 1),
                               label = "Worked for a party or a group, last 12 months"))

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Protest | sgnptit, bctprd, pbldmn --> petition, boycott, demonstration ----

recode_action_var <- function(var, label) {
  var_label <- c("Not done" = 0, "Has done" = 1)
  r <- case_when(var == 1 ~ 1,
                 var == 2 ~ 0)
  r <- labelled(r, var_label, label = label)
  r
}

ess <- ess %>%
  mutate(petition = recode_action_var(sgnptit, label = "Signed a petition, last 12 months"),
         boycott = recode_action_var(bctprd, label = "Boycotted certain products, last 12 months"),
         demonstration = recode_action_var(pbldmn, label = "Taken part in lawful public demonstration, last 12 months"))

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Gender (woman) | gndr --> female ----

ess <- ess %>%
  mutate(female = case_when(gndr == 1 ~ 0,
                            gndr == 2 ~ 1,),
         female = labelled(female,
                           c("Male" = 0, "Female" = 1),
                           label = "Gender"))

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Year of birth | yrbrn --> yrbrn ----  

attr(ess$yrbrn, "label") <- "Year of birth"

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# 5-year cohort | yrbrn --> cohort ----  

ess <- ess %>%
  mutate(cohort = floor(yrbrn / 5) * 5)

attr(ess$cohort, "label") <- "5-year cohort"

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Country-cohort | country, yrbrn --> yrbrn ----  

ess <- ess %>%
  mutate(country_yrbrn = ifelse(!is.na(yrbrn), 
                                paste0(country, yrbrn), 
                                NA_character_))

attr(ess$country_yrbrn, "label") <- "Country-cohort"


# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Age | yrbrn, yearsurvey --> age ----  

ess <- ess %>%
  mutate(age = (yearsurvey - yrbrn))

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Age (10 years) | yrbrn, yearsurvey --> age10 ----  

ess <- ess %>%
  mutate(age10 = (yearsurvey - yrbrn)/10)

attr(ess$age10, "label") <- "Age (10 years), calculated"

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Highest level of education (3 categories) | eisced --> edu ----

ess <- ess %>%
  mutate(edu = case_when(eisced %in% c(1:2) ~ 1, 
                         eisced %in% c(3:4) ~ 2,
                         eisced %in% c(5:7) ~ 3,
                         eisced == 0 & edulvla %in% c(1:2) ~ 1,
                         eisced == 0 & edulvla %in% c(3:4) ~ 2,
                         eisced == 0 & edulvla == 5 ~ 3),
         edu = labelled(edu, 
                        labels = c("Lower" = 1, 
                                   "Medium" = 2,
                                   "Higher" = 3),
                        label = "Highest level of education, 3 categories")) 

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Unemployed | mnactic --> unemp ----

ess <- ess %>%
  mutate(unemp = case_when(mnactic %in% c(1,2,5,6,7,8,9) ~ 0,
                           mnactic %in% c(3,4) ~ 1),
         unemp = labelled(unemp,
                          labels = c("Other" = 0,
                                     "Unemployed" = 1),
                          label = "Unemployed"))

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Native | brncntr --> native ----

ess <- ess %>%
  mutate(native = case_when(brncntr == 2 ~ 0,
                            brncntr == 1 ~ 1),
         native = case_when(country == "DEW" & wherebefore1990 != 2 ~ 0,
                            country == "DEE" & wherebefore1990 != 1 ~ 0,
                            TRUE ~ native),
         native = labelled(native, 
                           labels = c("Not native" = 0,
                                      "Native" = 1),
                           label = "Born in country"))

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Member of trade union (currently or previously) | mbtru --> union ----

ess <- ess %>%
  mutate(union = case_when(mbtru == 3 ~ 0,
                           mbtru %in% c(1,2) ~ 1),
         union = labelled(union, 
                          labels = c("Not member" = 0, "Member" = 1),
                          label = "Member of trade union, currently or previously"))

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Size of town | domicil --> city ----

ess <- ess %>%
  mutate(city = case_when(domicil == 1 ~ 5,
                          domicil == 2 ~ 4,
                          domicil == 3 ~ 3,
                          domicil == 4 ~ 2,
                          domicil == 5 ~ 1),
         city = labelled(city, 
                         labels = c("Home in countryside" = 1,
                                    "Country village" = 2,
                                    "Town or small city" = 3,
                                    "Outskirts of big city" = 4, 
                                    "Big city" = 5),
                         label = "Size of town"))

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Social class | Import Oesch data (Oesch 2006) --> class5 ----

ess <- "https://raw.githubusercontent.com/jolyphil/" %>% # download class data
  paste0("oesch-class/master/data/oesch_class_ess_1-8.csv") %>% # from GitHub
  read_csv() %>%
  mutate(essround = as.numeric(essround), # convert variable class to allow join
         idno = as.numeric(idno)) %>%
  dplyr::select(essround, cntry, idno, class5) %>%
  left_join(ess, ., by = c("essround", "cntry", "idno"))

ess <- ess %>%
  mutate(class5 = case_when(class5 == 1 ~ 5,
                            class5 == 2 ~ 4,
                            class5 == 3 ~ 3,
                            class5 == 4 ~ 2,
                            class5 == 5 ~ 1),
         class5 = labelled(class5, 
                           labels = c("Unskilled workers" = 1,
                                      "Skilled workers" = 2,
                                      "Small business owners" = 3,
                                      "Lower service class" = 4,
                                      "Higher service class" = 5),
                           label = "Social class"))

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Socialized in Eastern Germany | ... --> eastsoc ----

ess <- ess %>%
  eastsoc %>%
  mutate(eastsoc = labelled(eastsoc,
                           c("West German" = 0, "East German" = 1),
                           label = "East German"))

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Bundesland, Germany (chap. 6) | region, regionde --> land_de ----

ess <- ess %>%
  mutate(land_de = case_when(regionde == 8  | region == "DE1" ~ 1,  # Baden-Wuerttemberg
                             regionde == 9  | region == "DE2" ~ 2,  # Bayern
                             regionde == 11 | region == "DE3" ~ 3,  # Berlin
                             regionde == 12 | region == "DE4" ~ 4,  # Brandenburg
                             regionde == 4  | region == "DE5" ~ 5,  # Bremen
                             regionde == 2  | region == "DE6" ~ 6,  # Hamburg
                             regionde == 6  | region == "DE7" ~ 7,  # Hessen
                             regionde == 13 | region == "DE8" ~ 8,  # Mecklenburg-Vorpommern
                             regionde == 3  | region == "DE9" ~ 9,  # Niedersachsen
                             regionde == 5  | region == "DEA" ~ 10, # Nordrhein-Westfalen
                             regionde == 7  | region == "DEB" ~ 11, # Rheinland-Pfalz
                             regionde == 10 | region == "DEC" ~ 12, # Saarland
                             regionde == 14 | region == "DED" ~ 13, # Sachsen
                             regionde == 15 | region == "DEE" ~ 14, # Sachsen-Anhalt
                             regionde == 1  | region == "DEF" ~ 15, # Schleswig-Holstein
                             regionde == 16 | region == "DEG" ~ 16), # Thueringen
         land_de = labelled(land_de,
                            labels = c("Baden-Wuerttemberg" = 1,
                                       "Bayern" = 2,
                                       "Berlin" = 3,
                                       "Brandenburg" = 4,
                                       "Bremen" = 5,
                                       "Hamburg" = 6,
                                       "Hessen" = 7,
                                       "Mecklenburg-Vorpommern" = 8,
                                       "Niedersachsen" = 9,
                                       "Nordrhein-Westfalen" = 10,
                                       "Rheinland-Pfalz" = 11,
                                       "Saarland" = 12,
                                       "Sachsen" = 13,
                                       "Sachsen-Anhalt" = 14,
                                       "Schleswig-Holstein" = 15,
                                       "Thueringen" = 16),
                            label = "Bundesland, DE")) 

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Filter impossible values in East/West Germany

ess <- ess %>%
  mutate(land_de_east = land_de %in% c(4, 8, 13, 14, 16),
         land_de_west = land_de %in% c(1, 2, 5, 6, 7, 9, 10, 11, 12, 15)) %>%
  filter((land_de_east & intewde == 1) | 
         (land_de_west & intewde == 2) | 
         (land_de == 3) |
         is.na(land_de))

# ______________________________________________________________________________
# Clean and save main dataset ====

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Select variables for final dataset ----

ess <- ess %>% 
  dplyr::select(essround,
         idno,
         country,
         countrywave,
         year,
         period,
         dweight,
         
         stfdem,
         gincdif,
         imwbcnt,
         
         demonstration,
         petition,
         boycott,
         female,
         age10,
         yrbrn,
         cohort,
         country_yrbrn,
         edu,
         unemp,
         partygroup,
         union,
         native,
         city,
         class5,
         eastsoc,
         land_de)

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Save dataset ----

saveRDS(ess, file = file.path("data", "ess.rds")) 


# ______________________________________________________________________________
# Clean environment

rm(eastsoc,
   ess,
   ess_zip_files,
   find_dta,
   load_one_package,
   load_packages,
   p,
   recode_action_var,
   save_ess_rds)

gc()

# ******************************************************************************
# Task:    Extract ESS country-specific data for Germany
# ******************************************************************************

source("R/load_packages.R")

p <- c("dplyr", # Used for data wrangling
       "foreign", # Converts SPSS files to R objects
       "purrr", # Functional programming
)

load_packages(p)

# ______________________________________________________________________________
# Define functions ====

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

extract_data <- function(filename) {
  
  filepath <- file.path("data-raw", filename)
                           
  df <- read.spss(filepath, use.value.labels = F, to.data.frame = T) %>%
    as_tibble()
  
  df
}

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

recode_miss_one <- function(x) {
  
  miss_codes <- c(7, 8, 6666, 7777, 8888)
  
  if (x %in% miss_codes) {
    x <- NA_real_
  }
    
  x
}

recode_miss <- function(x) {
  
  map_dbl(x, recode_miss_one)

}

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

clean_data <- function(df) {
  
  # Rename variable names to lowercase
  names(df) <- names(df) %>% 
    tolower()
  
  # Rename variables so that everything is clearer and harmonized across ESS
  # rounds
  if (any(names(df) == "splow5de")) { # Names for ESS 1, 2, 3, 4, 6, 7, 8
    df <- df %>% 
      mutate(
        wherebefore1990 = splow2de,
        yrmovedwest = splow4de,
        yrmovedeast = splow5de
      )
  } else if (any(names(df) == "n3")) { # Names for ESS 5
    df <- df  %>% 
      mutate(
        wherebefore1990 = n3,
        yrmovedwest = n5a_1,
        yrmovedeast = n5b_1
      )
  } else {
    print("Unknown data")
    break
  }
  
  df <- df %>%
    mutate(
      wherebefore1990 = recode_miss(wherebefore1990),
      yrmovedwest = recode_miss(yrmovedwest),
      yrmovedeast = recode_miss(yrmovedeast),
    ) %>%
    select(cntry,
           idno,
           wherebefore1990,
           yrmovedwest,
           yrmovedeast)
  
  df
}

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

save_data <- function(df, filename) {
  rootname <- str_sub(filename, end = -5)
  rdsfilepath <- file.path("data", "temp", paste0(rootname, ".rds"))
  
  saveRDS(df, file = rdsfilepath)
}

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

save_clean_csdata <- function(filename){
  
  extract_data(filename) %>%
    clean_data %>%
    save_data(filename)
  
}

# ______________________________________________________________________________
# Find country-specific datafiles (SPSS) ====

filenames <- file.path("data-raw") %>% 
  list.files() %>%
  .[(str_detect(., "ESS[:digit:]csDE.(por|sav)"))]

# ______________________________________________________________________________
# Execute functions on each dataset ====

map(filenames, save_clean_csdata)

# ______________________________________________________________________________
# Clear ====

rm("clean_data",
   "extract_data",
   "filenames",
   "load_one_package",
   "load_packages",
   "p",
   "recode_miss",
   "recode_miss_one",
   "save_clean_csdata",
   "save_data")

gc()

eastsoc <- function(essdata, agemin = 15, agemax = 25) {
  
  # (1) Define the maximum number of years of early political socialization
  #    (by default: 11)
  rangemax <- agemax - agemin + 1
  
  # (2) Generate new variables
  essdata <- essdata %>%
    mutate(
      # (2.1) Respondent interviewed in Eastern Germany? yes (1) / no (0)
      eastintv = case_when(
        intewde == 1 ~ 1,
        intewde == 2 ~ 0),

      # (2.2) Lived in East Germany before 1990? yes (1) / no (0)
      eastbefore1990 = case_when(
       # Lived in East Germany / East Berlin
       wherebefore1990 == 1 ~ 1,
       # Lived in West Germany / West Berlin
       wherebefore1990 == 2 ~ 0),

      # (2.3) Age when moved to East Germany
      agemovedeast = case_when(
       yrmovedeast <= yearsurvey & (yrmovedeast - yrbrn) > 0 
       ~ yrmovedeast -  as.numeric(yrbrn)),
      
      # (2.4) Age when moved to West Germany
      agemovedwest = case_when(
       yrmovedwest <= yearsurvey & (yrmovedwest - yrbrn) > 0 
       ~ yrmovedwest -  as.numeric(yrbrn)),
       
      # (2.5) Total years of early political socialization [0-rangemax]
      soctotyears = case_when(
      age > agemax ~ rangemax,
       age >= agemin & age <= agemax ~ as.numeric(age) - agemin,
       !is.na(age) ~ 0),
       
      # (2.6) Years of political socialization in Eastern Germany [0-rangemax]
      socyearseast = case_when(
      # Case 1: Lived in the GDR before 1990, still living in Eastern Germany
      age > agemax & eastintv == 1 & eastbefore1990 == 1 ~ rangemax,
      age >= agemin & age <= agemax & eastintv == 1 & eastbefore1990 == 1 
      ~ as.numeric(age) - agemin,
      
      # Case 2: Lived in the GDR before 1990, moved to Western Germany 
      agemovedwest < agemin ~ 0,
      agemovedwest >= agemin & agemovedwest <= agemax ~ agemovedwest - agemin,
      agemovedwest > agemax ~ rangemax,
      
      # Case 3: Lived in West Germany before 1990, still living in West Germ.
      eastintv == 0 & eastbefore1990 == 0 ~ 0,
      
      # Case 4: Lived in West Germany before 1990, moved to Eastern Germany
      agemovedeast < agemin  & age > agemax ~ rangemax,
      agemovedeast < agemin  & age >= agemin & age <= agemax ~ as.numeric(age) - agemin,
      agemovedeast >= agemin & agemovedeast <= agemax & 
        age > agemax ~ rangemax - (agemovedeast - agemin),
      agemovedeast >= agemin & agemovedeast <= agemax & age >= agemin & 
        age <= agemax ~ as.numeric(age) - agemovedeast,
      agemovedeast > agemax ~ 0
      ),
       
      # (2.7) Lived most formative years in Eastern Germany? yes (1) / no (0)
      eastsoc = case_when(
       soctotyears != 0 & (socyearseast / soctotyears) < 0.5 ~ 0, # West German
       soctotyears != 0 & (socyearseast / soctotyears) > 0.5 ~ 1, # East German
       soctotyears != 0 & (socyearseast / soctotyears) == 0.5 ~ eastintv), 
      
      # (2.8) Add other categories to eastsoc for younger and non-native citizens
      eastsocall = case_when(
       eastsoc == 0 ~ 1, # West German
       eastsoc == 1 ~ 2, # East German
       age <= agemin & !is.na(eastbefore1990) ~ 3, # born before 1990, but too
       # young for socialization
       wherebefore1990 == 6 ~ 4, # born after 1990
       wherebefore1990 == 3 ~ 5) # non-native
    )
  
  essdata
}
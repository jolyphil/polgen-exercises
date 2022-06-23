library(dplyr) # Data wrangling
library(ggplot2) # Data visualization
library(marginaleffects) # Ajusted predictions
library(survey) # Survey design

evs <- readRDS("data/evs.rds") # Load EVS 1981-2017


# Functions to get weighted proportions -----------------------------------

prop <- function(varname, data, design) {
  lapply(levels(data[[varname]]),
         prop_one_level,
         varname = varname,
         design = design) |>
    bind_rows()
}

prop_one_level <- function(level, varname, design) {
  fm <- paste0("~ I(", varname, " == \"", level,   "\")") |>
    as.formula() # Formula to extract proportion for 1 level of var
  p <- svyciprop(fm, design)
  prop_df <- data.frame(level = level,
                        prop = as.numeric(p),
                        lower = attr(p, "ci")[1],
                        upper = attr(p, "ci")[2])
  rownames(prop_df) <- NULL
  prop_df
}

# Function to get weighted difference in percent between postmaterialists and 
# materialists ------------------------------------------------------------

pct_diff_postmat <- function(var, weight) {
  df <- data.frame(var, weight)
  
  df_d <- svydesign(ids = ~1,
                    weights = ~weight,
                    data = df)
  
  prop_df <- prop("var", df, df_d)
  
  diff_pct <- (prop_df$prop[3] - prop_df$prop[1]) * 100
  
  diff_pct
}


# Analysis: Differences between countries over time -----------------------

panel_country <- evs |> 
  filter(country != "DE") |> # Disrupted time-series due to reunification
  filter(!(country == "GB-GBN")) |> # Missing wave 3
  filter(!is.na(weight) & weight != 0) |> 
  group_by(country, wave) |> 
  summarize(pct = pct_diff_postmat(postmat, weight),
            year = mean(year, na.rm = TRUE)) |> 
  ungroup()

# Model with cubic year effect
m1 <- lm(pct ~ poly(year, 3) + country, data = panel_country)

# Extract adjusted predictions
data_pred <- predictions(m1,
                         newdata = datagrid(year = 1981:2017, 
                                            grid_type = "counterfactual")) |> 
  summary(by = "year") |> 
  as.data.frame() |> 
  mutate(pct = estimate,
         country = "Adjusted mean") |> 
  select(country, year, pct)

panel_country |> 
  filter(country != "Adjusted mean") |> 
  ggplot(aes(x = year, y = pct, color = country)) +
  geom_line() +
  geom_line(data = data_pred, size = 3) +
  labs(x = "Year", 
       y = "Percent of postmaterialists vs materialists",
       color = "",
       size = "")

ggsave("figures/evs_pct_postmat.pdf")


# By generation -----------------------------------------------------------

# By generation, over time ------------------------------------------------

# Function to extract predictions by generations, over time
extract_pred_gen_year <- function(country, data) {
  
  df <- data[data$country == country, ]
  df$postmat2 <- NA_integer_
  df$postmat2[df$postmat == "Postmaterialist"] <- 1L
  df$postmat2[df$postmat == "Materialist" | df$postmat == "Mixed"] <- 0L
  
  df_d <- svydesign(ids = ~1,
                    weights = ~weight,
                    data = df)
  
  # Model with interaction between generations and year squared
  m <- svyglm(postmat2 ~ gen + year + I(year^2) + gen:year + gen:I(year^2),
              design = df_d,
              family = quasibinomial)
  
  pred <- predictions(m) |> 
    mutate(country = country)
  
  pred
}

data_plot <- lapply(c("DK", "ES", "FR", "IS", "IT", "NL", "SE"), 
            extract_pred_gen_year, 
            data = evs) |> 
  bind_rows() |> 
  select(country, year, gen, predicted, conf.low, conf.high) |> 
  distinct()

data_plot |> 
  ggplot(aes(x = year, y = predicted, fill = gen, color = gen)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = .3, 
              color = NA) +
  geom_line() +
  facet_wrap(vars(country)) + 
  labs(x = "Year",
       y = "Predicted probability of being postmaterialist",
       fill = "Generations",
       color = "Generations")

ggsave("figures/evs_pred_postmat_gen_year.pdf")


# By generation, over age -------------------------------------------------

# Function to extract predictions by generations, over age
extract_pred_gen_age <- function(country, data) {
  
  df <- data[data$country == country, ]
  df$postmat2 <- NA_integer_
  df$postmat2[df$postmat == "Postmaterialist"] <- 1L
  df$postmat2[df$postmat == "Materialist" | df$postmat == "Mixed"] <- 0L
  
  
  df_d <- svydesign(ids = ~1,
                    weights = ~weight,
                    data = df)
  
  # Model with interaction between generations and age squared
  m <- svyglm(postmat2 ~ gen + age + I(age^2) + gen:age + gen:I(age^2),
              design = df_d,
              family = quasibinomial)
  
  pred <- predictions(m) |> 
    mutate(country = country)
  
  pred
}

data_plot <- lapply(c("DK", "ES", "FR", "IS", "IT", "NL", "SE"), 
                    extract_pred_gen_age, 
                    data = evs) |> 
  bind_rows() |> 
  select(country, age, gen, predicted, conf.low, conf.high) |> 
  distinct()

data_plot |> ggplot(aes(x = age, y = predicted, fill = gen, color = gen)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = .3, 
              color = NA) +
  geom_line() +
  facet_wrap(vars(country))  + 
  labs(x = "Age",
       y = "Predicted probability of being postmaterialist",
       fill = "Generations",
       color = "Generations")

ggsave("figures/evs_pred_postmat_gen_age.pdf")

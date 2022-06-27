library(ggplot2)
library(haven)
library(marginaleffects)
library(survey)

ess_raw <- readRDS("data/ess.rds")


# Prepare data (final adjustments) ----------------------------------------

# The ESS datafile was prepared in the other scripts was made to be imported in 
# Stata. 
# We need to transform a few variable to analyse it in R. 

ess <- ess_raw |> 
  filter(cohort >= 1920 & cohort <= 1995) |> 
  mutate(period = factor(period),
         cohort = factor(cohort),
         eastsoc = case_when(eastsoc == 0 ~ "West",
                             eastsoc == 1 ~ "East"),
         eastsoc = factor(eastsoc, levels = c("West German", "East German"))) |> 
  mutate(across(c(female, edu, unemp, union, city, class5, native, land_de), 
                as_factor))

ess_d <- svydesign(ids = ~1,
                   weights = ~dweight,
                   data = ess)


# Function: get model results ---------------------------------------------

get_model <- function(outcome) {
  
  predictors <- "eastsoc + cohort + eastsoc:cohort + period + eastsoc:period + female + edu + unemp + city +  class5 + land_de"
  fm <- as.formula(paste(outcome, "~", predictors))
  
  survey::svyglm(fm,
                 design = ess_d)
}

plot_pred <- function(model, type) {
  pred_data <- predictions(model) |> 
    summary(by = c(type, "eastsoc"))
  
  outcome <- names(model$model)[1]
  if(type == "cohort"){
    xintercept1 <- 1929
    xintercept2 <- 1970
  } else {
    xintercept1 <- NULL
    xintercept2 <- NULL
  }
  xlab <- case_when(type == "cohort" ~ "Cohorts",
                    type == "period" ~ "Year")
  ylab <- case_when(outcome == "stfdem" ~ "Predicted satisfaction with democracy",
                    outcome == "gincdif" ~ "Predicted attitude towards economic redistribution", 
                    outcome == "imwbcnt" ~ "Predicted attitude towards immigration")
  
  if(outcome == "stfdem") {
    y_limits <- c(0, 10)
    y_breaks <- 0:10
    y_labels <- c("Extremely dissatisfied", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Extremely satisfied")
  }
  
  if(outcome == "gincdif") {
    y_limits <- c(1, 5)
    y_breaks <- 1:5
    y_labels <- c("(1) Agree strongly", "(2) Agree", "(3) Neither agree nor disagree", "(4) Disagree", "(5) Disagree strongly")
  }
  
  if(outcome == "imwbcnt") {
    y_limits <- c(0, 10)
    y_breaks <- 0:10
    y_labels <- c("Worse place to live", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Better place to live")
  }
  
  type <- sym(type) 
  
  pred_data |> 
    ggplot(aes(x = as.numeric(as.character(!!type)), 
               y = estimate, 
               color = eastsoc, 
               group = eastsoc,
               fill = eastsoc)) +
    geom_vline(xintercept = xintercept1, 
               linetype="dashed") +
    geom_vline(xintercept = xintercept2, 
               linetype="dashed") +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                alpha = .3, 
                color = NA) +
    scale_y_continuous(limits = y_limits,
                       breaks = y_breaks,
                       label = y_labels) +
    labs(x = xlab,
         y = ylab,
         color = "",
         group = "",
         fill = ""
         )
}


plot_mfx <- function(model, type) {
  
  mfx_data <- model |> 
    marginaleffects(variables = "eastsoc") |> 
    summary(by = type)

  outcome <- names(model$model)[1]
  if(type == "cohort"){
    xintercept1 <- 1929
    xintercept2 <- 1970
  } else {
    xintercept1 <- NULL
    xintercept2 <- NULL
  }
  xlab <- case_when(type == "cohort" ~ "Cohorts",
                    type == "period" ~ "Year")
  ylab <- case_when(outcome == "stfdem" ~ "Effect of east socialization on satisfaction with democracy",
                    outcome == "gincdif" ~ "Effect of east socialization on attitude towards economic redistribution", 
                    outcome == "imwbcnt" ~ "Effect of east socialization on attitude towards immigration")
  
  type <- sym(type) 
  
  mfx_data |> 
    ggplot(aes(x = as.numeric(as.character(!!type)), 
               y = estimate, 
               group = 1)) +
    geom_hline(yintercept = 0, 
               linetype="dashed",
               color = "red") +
    geom_vline(xintercept = xintercept1, 
               linetype="dashed") +
    geom_vline(xintercept = xintercept2, 
               linetype="dashed") +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, 
                    ymax = conf.high), 
                alpha = .3, 
                color = NA) +
    labs(x = xlab,
         y = ylab,
         color = "",
         group = "",
         fill = ""
    )        
}

export_figures <- function(outcome) {
  m <- get_model(outcome)
  
  m |> 
    plot_pred("cohort")
  
  ggsave(paste0("figures/", outcome, "_pred_cohort.pdf"), 
         width = 5, 
         height = 5)
  
  m |> 
    plot_mfx("cohort")
  
  ggsave(paste0("figures/", outcome, "_mfx_cohort.pdf"), 
         width = 5, 
         height = 5)
  
  m |> 
    plot_pred("period")
  
  ggsave(paste0("figures/", outcome, "_pred_period.pdf"), 
         width = 5, 
         height = 5)
  
  m |> 
    plot_mfx("period")
  
  ggsave(paste0("figures/", outcome, "_mfx_period.pdf"), 
         width = 5, 
         height = 5)
}


export_figures("stfdem")
export_figures("gincdif")
export_figures("imwbcnt")

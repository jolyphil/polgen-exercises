library(dplyr)
library(ggplot2)
library(survey)
library(marginaleffects)

gles <- readRDS(file = "data/gles.rds")
gles_d <- svydesign(ids = ~vpoint,
                    weights = ~n_w_ipfges,
                    data = gles)

# Proportion functions ----------------------------------------------------

pct_afd <- function(group, data = gles, design = gles_d) {
  
  levels <- unique(data[[group]])
  levels <- levels[!is.na(levels)]
  
  pct <- sapply(levels, 
                 pct_afd_one_level, 
                 group = group,
                 data = data,
                 design = design)
  data.frame(level = levels, pct)
}

pct_afd_one_level <- function(group, level, data, design) {
  condition <- paste0(group, " == \"", level, "\"")
  dsub <- subset(design, eval(parse(text = condition)))
  prop("vote", data, dsub)[[6,2]] * 100
}

prop <- function(varname, data, design) {
  lapply(levels(data[[varname]]),
         prop_one_level,
         varname = varname,
         design = design) |>
    dplyr::bind_rows()
}

prop_one_level <- function(level, varname, design) {
  fm <- paste0("~ I(", varname, " == \"", level,   "\")") |>
    as.formula()
  p <- survey::svyciprop(fm, design)
  lower <- attr(prop, "ci")[1]
  prop_df <- data.frame(level = level,
                        prop = as.numeric(p),
                        lower = attr(p, "ci")[1],
                        upper = attr(p, "ci")[2])
  rownames(prop_df) <- NULL
  prop_df
}


# Plot function -----------------------------------------------------------

plot_prop <- function(group) {
  pct_afd(group) |> 
    mutate(pct_chr = sprintf("%.0f", pct)) |> 
    ggplot(aes(x = level, y = pct)) +
    geom_col() +
    geom_text(aes(label = pct_chr), vjust = -0.3) +
    labs(x = "",
         y = "Percent of AfD voters")
  
  filename <- paste0("figures/afd_", group, ".pdf")
  ggsave(filename)
  
}


# Graphs ------------------------------------------------------------------

plot_prop("edu_high")
plot_prop("income")
plot_prop("residence")
plot_prop("east")
plot_prop("age_group")

m <- svyglm(vote_afd ~ east + yearborn + I(yearborn^2) + east:yearborn + east:I(yearborn^2),
            design = gles_d,
            family = quasibinomial)

predictions(m) |> 
  ggplot(aes(x = yearborn, y = predicted, fill = east, color = east)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = .3, 
              color = NA) +
  geom_line() +
  labs(x = "Year of birth",
       y = "Predicted probability of having voted for the AfD",
       fill = "Region",
       color = "Region")

ggsave("figures/afd_cohort.pdf")

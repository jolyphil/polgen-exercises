library(ggplot2)
library(marginaleffects)

ess_raw <- readRDS("data/ess.rds")

ess <- ess_raw |> 
  filter(cohort >= 1920 & cohort <= 1995) |> 
  mutate(cohort = factor(cohort),
         period = factor(period),
         eastsoc = case_when(eastsoc == 0 ~ "West",
                             eastsoc == 1 ~ "East"),
         eastsoc = factor(eastsoc, levels = c("West", "East")))

m <- lm(imwbcnt ~ eastsoc +
          cohort +
          eastsoc:cohort +
          period + 
          eastsoc:period +
          factor(land_de)
        , data = ess)

x <- predictions(m) |> 
  summary(by = c("cohort", "eastsoc"))

x |> 
  ggplot(aes(x = cohort, 
             y = estimate, 
             color = eastsoc, 
             group = eastsoc,
             fill = eastsoc)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = .3, 
              color = NA)

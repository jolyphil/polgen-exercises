# Export figures from APC analyses
# ==============================================================================

library(dplyr) # data wrangling
library(ggplot2) # Data visualization
library(modelsummary) # Visualize model estimates

evs <- readRDS("data/evs.rds")


# Prepare labels and titles -----------------------------------------------

y_labels <- c("Never justifiable", "2", "3", "4", "5", "6", "7", "8", "9", "Always justifiable")
y_title <- "Attitudes towards homosexuality"
years <- c(1990, 1999, 2008, 2017)
xsize = 8
ysize = 6

# Attitudes over time -----------------------------------------------------

evs |> 
  group_by(year) |> 
  summarize(b = mean(homo, na.rm = TRUE),
            ll = t.test(homo)$conf.int[[1]],
            ul = t.test(homo)$conf.int[[2]]) |> 
  ggplot(aes(x = year, y = b)) +
  geom_line() +
  geom_pointrange(aes(ymin = ll, ymax = ul)) + 
  scale_x_continuous(breaks = years) +
  scale_y_continuous(limits = c(1,10),
                     breaks = 1:10,
                     label = y_labels) +
  labs(x = "Year",
       y = y_title)

ggsave("figures/period.pdf", width = xsize, height = ysize)


# Attitudes by cohort -----------------------------------------------------

evs |> 
  ggplot(aes(x = yearborn, y = homo)) +
  geom_smooth(method = "loess") +
  xlim(c(1920, 2000)) +
  scale_y_continuous(limits = c(1,10),
                     breaks = 1:10,
                     label = y_labels) +
  labs(x = "Year of birth",
       y = y_title)

ggsave("figures/cohort.pdf", width = xsize, height = ysize)



# Attitudes by age --------------------------------------------------------

evs |> 
  ggplot(aes(x = age, y = homo)) +
  geom_smooth(method = "loess") +
  xlim(c(18, 80)) +
  scale_y_continuous(limits = c(1,10),
                     breaks = 1:10,
                     label = y_labels) +
  labs(x = "Age",
       y = y_title)

ggsave("figures/age.pdf", width = xsize, height = ysize)


# Attitudes over time by generation ---------------------------------------

evs |> 
  filter(!is.na(gen)) |> 
  group_by(year, gen) |> 
  summarize(b = mean(homo, na.rm = TRUE),
            ll = t.test(homo)$conf.int[[1]],
            ul = t.test(homo)$conf.int[[2]]) |> 
  ggplot(aes(x = year, y = b, color = gen)) +
  geom_line() +
  geom_pointrange(aes(ymin = ll, ymax = ul)) + 
  scale_x_continuous(breaks = years) +
  scale_y_continuous(limits = c(1,10),
                     breaks = 1:10,
                     label = y_labels) +
  labs(x = "Year",
       y = y_title,
       color = "Generation")

ggsave("figures/period_gen.pdf", width = xsize, height = ysize)


# Attitudes by age, by generation -----------------------------------------

evs |> 
  ggplot(aes(x = age, y = homo, color = gen)) +
  geom_smooth(method = "loess") +
  xlim(c(18, 80)) +
  scale_y_continuous(limits = c(1,10),
                     breaks = 1:10,
                     label = y_labels) +
  labs(x = "Age",
       y = y_title)

ggsave("figures/age_gen.pdf", width = xsize, height = ysize)


# Model excluding age -----------------------------------------------------

m1 <- lm(homo ~ gen + factor(year), data = evs)
m2 <- lm(homo ~ gen + factor(year) + student + retired, data = evs)

cm = c("gen1940-1959" = "Gen. 1940-1959",
       "gen1960-1979" = "Gen. 1960-1979",
       "gen1980-1999" = "Gen. 1980-1999",
       "factor(year)1999" = "Year 1999",
       "factor(year)2008" = "Year 2008",
       "factor(year)2017" = "Year 2017",
       "student" = "Student",
       "retired" = "Retired"
)
cm <- rev(cm)

modelplot(list("With controls for age" = m2, "Without controls for age" = m1), coef_map = cm) +
  xlim(c(-1, 3.5)) +
  geom_vline(xintercept = 0,
             linetype="dashed",
             color = "red")

ggsave("figures/models.pdf", width = xsize, height = ysize)

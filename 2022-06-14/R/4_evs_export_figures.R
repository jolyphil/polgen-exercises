library(dplyr)
library(ggplot2)

evs <- readRDS("data/evs.rds")

panel_mean <- evs |> 
  filter(country != "DE") |> 
  group_by(wave) |> 
  summarize(protest = mean(protest, na.rm = TRUE) * 100,
            year = mean(year, na.rm = TRUE)) |> 
  mutate(country = "Mean") |> 
  ungroup()

panel <- evs |> 
  filter(country != "DE") |> 
  group_by(country, wave) |> 
  summarize(protest = mean(protest, na.rm = TRUE) * 100,
            year = mean(year, na.rm = TRUE)) |> 
  ungroup()

panel <- panel |> 
  rbind(panel_mean)

panel |> 
  ggplot(aes(x = year, y = protest, color = country)) +
  geom_line(aes(size = country)) +
  scale_size_manual(values = c("DK" = 0.5,
                               "ES" = 0.5,
                               "FR" = 0.5,
                               "GB-GBN" = 0.5,
                               "IS" = 0.5,
                               "IT" = 0.5,
                               "Mean" = 3,
                               "NL" = 0.5,
                               "SE" = 0.5)) +
  labs(x = "", 
       y = "Protest experience, percent",
       color = "",
       size = "")

ggsave("figures/evs_prop_protest_year.pdf")


library(dplyr)
library(ggplot2)
library(tidyr)

recode_polpart_binary <- function(var){
  newvar <- as.numeric(NA)
  newvar[var == "No"] <- 0
  newvar[var == "Yes"] <- 1
  newvar
}

ess9_de <- readRDS("data/ess9_de.rds")

data_plot <- ess9_de |> 
  filter(!is.na(age_group)) |> 
  mutate(
    across(.cols = c(vote,
                     contact,
                     work_party,
                     work_org,
                     badge,
                     petition,
                     demo,
                     boycott,
                     online_part),
           .fns = recode_polpart_binary)
  ) |> 
  group_by(age_group) |> 
  summarize(across(.cols = c(vote,
                             contact,
                             work_party,
                             work_org,
                             badge,
                             petition,
                             demo,
                             boycott,
                             online_part),
                       .fns = ~ mean(.x, na.rm = TRUE) * 100)) |> 
  pivot_longer(!age_group, names_to = "action", values_to = "pct")

data_plot |> 
  ggplot(aes(x = action, y = pct, fill = age_group)) +
  geom_col(position = "dodge") +
  labs(x = "",
       y = "Percent",
       fill = "Age groups")

ggsave("figures/ess_polpart.pdf")

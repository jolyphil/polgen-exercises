library(dplyr)
library(ggplot2)

gles <- readRDS(file = "data/gles.rds")


# Differences over time for the youngest cohort ---------------------------

gles |> 
  filter(age_group == "16-29" & !is.na(party_id2)) |> 
  group_by(party_id2, survey) |> 
  count() |> 
  ungroup() |> 
  group_by(survey) |> 
  mutate(pct = n / sum(n) * 100,
         pct_chr = sprintf("%.0f", pct)) |> 
  ggplot(aes(x = party_id2, y = pct)) +
  geom_col() +
  geom_text(aes(label = pct_chr), vjust = -0.3) +
  facet_wrap(vars(survey)) +
  labs(x = "Party identification", y = "Percent")

ggsave("figures/pid2_16-29_pre_post.pdf")

gles_16_29 <- gles |> 
  filter(age_group == "16-29")

table(gles_16_29$survey, gles_16_29$party_id2) |> 
  prop.test() # Significant difference



# Differences across age groups -------------------------------------------

# Function to generate graph
plot_party_id <- function(df) {
  df |> 
    ggplot(aes(x = party_id, y = pct, fill = party_id)) +
    geom_col() + 
    geom_text(aes(label = pct_chr), vjust = -0.3) +
    scale_fill_manual(values = c("CDU/CSU" = "#000000",
                                 "SPD" = "#e3010f",
                                 "AfD" = "#00acd3",
                                 "FDP" = "#ffe300",
                                 "DIE LINKE" = "#e60e98",
                                 "GRÜNE" = "#65a129",
                                 "Other party" = "#8c8b8b",
                                 "No party" = "#d3d3d3")) +
    labs(x = "Party identification", y = "Percent") +
    theme(legend.position = "none")
}

# 16-29
gles |> 
  filter(age_group == "16-29" & !is.na(party_id) & survey == "post") |> 
  group_by(party_id) |> 
  count() |> 
  ungroup() |> 
  mutate(pct = n / sum(n) * 100,
         pct_chr = sprintf("%.0f", pct)) |> 
  plot_party_id()

ggsave("figures/pid_16-29_post.pdf")

# 30-54
gles |> 
  filter(age_group == "30-54" & !is.na(party_id) & survey == "post") |> 
  group_by(party_id) |> 
  count() |> 
  ungroup() |> 
  mutate(pct = n / sum(n) * 100,
         pct_chr = sprintf("%.0f", pct)) |> 
  plot_party_id()

ggsave("figures/pid_30-54_post.pdf")

# 55+
gles |> 
  filter(age_group == "55+" & !is.na(party_id) & survey == "post") |> 
  group_by(party_id) |> 
  count() |> 
  ungroup() |> 
  mutate(pct = n / sum(n) * 100,
         pct_chr = sprintf("%.0f", pct)) |> 
  plot_party_id()

ggsave("figures/pid_55_post.pdf")

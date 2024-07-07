library(tidyverse)
library(scales)
library(ggtext)
library(extrafont)
library(ggalt)
library(gridExtra)
library(cowplot)
library(grid)
library(fmsb)
library(janitor)
library(ggtext)
library(ggplot2)

# loading data for top seven leagues
top7 <- read.csv("Top7_CleanData.csv")

target <- "Manuel Ugarte Ribeiro"
# player pizza

df_ls <- top7 %>%
  filter(
    league != "NED-Eredivisie",
    league != "POR-Primeira Liga"
  ) %>%
  group_by(pos) %>%
  mutate(
    Fls90 = Fls / X90s,
    Tackle90 = TklW / X90s,
    Recover90 = Recov / X90s,
    Int90 = Int / X90s,
    Block90 = Blocks.1 / X90s,
    PrgP90 = PrgP.1 / X90s,
    PrgC90 = PrgC.1 / X90s,
    CPA90 = CPA / X90s,
    APTouches = Att.Pen / X90s,
    KP90 = KP / X90s,
    PPA90 = PPA / X90s,
    PL90 = PassLive / X90s,
    G90 = Gls / X90s,
    Challenge90 = Tkl.1 / X90s
  ) %>%
  select(
    player, Min, Won., Fls90, Tackle90,
    Int90, Block90, PrgP90, PrgC90, Succ.,
    CPA90, APTouches, Sh.90, npxG.Sh,
    npxG.2, np.G.xG, KP90, PPA90, PL90,
    xAG, G90, ShtCmp., MedCmp., LngCmp., Challenge90, Tkl.
  ) %>%
  # players with at least 900 minutes
  filter(Min >= 900) %>%
  mutate(across(.cols = (Min:Tkl.), .fns = ~ ntile(.x, 100))) %>%
  ungroup() %>%
  select(-pos) %>%
  filter(player == target)

df_ls_t <- as.data.frame(t(df_ls))

df_ls_t_clean <- df_ls_t %>%
  rownames_to_column(var = "Statistic") %>%
  filter(
    Statistic != "player",
    Statistic != "Minutes Played"
  ) %>%
  mutate(Statistic = recode(Statistic,
    "Min" = "Minutes Played",
    "Won." = "Aerial Duel %",
    "Fls90" = "Fouls Committed",
    "Tackle90" = "Tackles",
    "Int90" = "Interceptions",
    "Block90" = "Blocks",
    "PrgP90" = "Prog Passes",
    "PrgC90" = "Prog Carries",
    "Succ." = "Take-On Success",
    "CPA90" = "Carries into PA",
    "APTouches" = "PA Touches",
    "Sh.90" = "Shots",
    "npxG.Sh" = "npxG per Shot",
    "npxG.2" = "npxG",
    "np.G.xG" = "npG-xG",
    "KP90" = "Key Passes",
    "PPA90" = "Passes into PA",
    "PL90" = "Open Play SCA",
    "xAG" = "xAG",
    "G90" = "Goals",
    "ShtCmp." = "Pass Completion (Short)",
    "MedCmp." = "Pass Completion (Med)",
    "LngCmp." = "Pass Completion (Long)",
    "Challenge90" = "Duels Won",
    "Tkl." = "Duel Success"
  )) %>%
  mutate(Percentile = as.numeric(V1)) %>%
  mutate(group = case_when(
    Statistic %in% c(
      "Aerial Duel %", "Fouls Committed",
      "Tackles", "Interceptions",
      "Blocks", "Duels Won", "Duel Success"
    ) ~ "Defending",
    Statistic %in% c(
      "Prog Passes", "Prog Carries",
      "Take-On Success",
      "Pass Completion (Short)",
      "Pass Completion (Med)",
      "Pass Completion (Long)"
    ) ~ "Possession",
    Statistic %in% c(
      "xAG", "Passes into PA",
      "Carries into PA", "PA Touches",
      "Key Passes", "Open Play SCA"
    ) ~ "Creating",
    Statistic %in% c(
      "Shots", "npxG per Shot", "npxG",
      "npG-xG", "Goals"
    ) ~ "Shooting",
    TRUE ~ "Misc"
  )) %>%
  mutate(isTop = ifelse(Percentile == "100", "Top", group)) %>%
  select(Statistic, Percentile, group, isTop)

df_ls_t_clean %>%
  arrange(group) %>%
  filter(Statistic != "Minutes Played") %>%
  ggplot(aes(
    x = fct_reorder(Statistic, group),
    y = Percentile,
    fill = isTop
  )) +
  ylim(-10, 110) +
  geom_hline(
    yintercept = 0,
    linewidth = 1
  ) +
  scale_x_discrete(labels = label_wrap(11)) +
  geom_col(
    colour = "white",
    width = 1,
    linewidth = 0.5
  ) +
  geom_text(
    aes(
      label = Percentile,
      y = Percentile + 7,
      colour = isTop
    ),
    family = "Noto Sans",
    fontface = "bold",
    size = 3.3,
    show.legend = F
  ) +
  scale_fill_manual(
    values = c(
      Creating = "#F8766D",
      Defending = "#7CAE00",
      Possession = "#00BFC4",
      Shooting = "#C77CFF",
      Top = "darkgoldenrod1"
    ),
    breaks = c("Creating", "Defending", "Possession", "Shooting")
  ) +
  scale_colour_manual(values = c("red", "darkgreen", "skyblue4", "purple", "darkgoldenrod")) +
  labs(
    title = paste0("Manuel Ugarte - Percentile Rankings"),
    subtitle = paste0("PSG | 2023-24 Ligue 1 | vs Top 5 Midfielders"),
    caption = paste0("Viz by @TheNumbers_Game | Data from Opta")
  ) +
  coord_polar(clip = "off") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.size = unit(5, "mm"),
    legend.text = element_markdown(
      family = "Noto Sans",
      face = "bold",
      colour = "black",
      size = 7
    ),
    plot.title = element_text(
      hjust = 0.5,
      family = "Noto Sans",
      face = "bold",
      size = 15,
      colour = "black"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      family = "Noto Sans",
      size = 10,
      colour = "black"
    ),
    plot.caption = element_text(
      family = "Noto Sans",
      colour = "black",
      size = 6
    ),
    axis.text.x = element_text(
      family = "Noto Sans",
      colour = "black",
      face = "bold",
      size = 9
    ),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("Ugarte Circle.jpg",
  width = 2560,
  height = 2560,
  units = "px",
  dpi = 256
)

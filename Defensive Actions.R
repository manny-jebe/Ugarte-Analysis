# loading packages
library(ggsoccer)
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
library(ggpubr)
library(worldfootballR)
library(RColorBrewer)
library(pals)

# import data and set values
events <- read.csv("NANPSG.csv")
target <- "Manuel Ugarte"
match <- "Nantes 0-2 PSG"
comp <- "Ligue 1"

# getting defensive actions
def_actions <- events %>%
  mutate(
    outcome_type = outcomeType,
    player = name
  ) %>%
  filter(player == target) %>%
  filter(type %in% c(
    "Challenge", "BallRecovery", "Clearance", "Tackle",
    "BlockedPass", "Interception", "Foul"
  ))

# successful actions and fouls count
fouls <- length(which(def_actions$type == "Foul"))
succ_actions <- length(which(def_actions$outcome_type == "Successful"))

# plotting data
def_actions %>%
  ggplot(aes(
    x = x,
    y = y
  )) +
  geom_density_2d_filled(aes(alpha = ..level..), bins = 9) +
  scale_fill_brewer(type = "seq", palette = "Blues", direction = 1) +
  geom_point(aes(colour = outcome_type),
    size = 3,
    shape = 21,
    stroke = 1,
    fill = "white"
  ) +
  scale_colour_manual(values = c("purple3", "grey")) +
  annotate_pitch(
    fill = NA,
    colour = "black"
  ) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_pitch() +
  labs(
    title = paste0(target, "'s Defensive Actions - ", match),
    subtitle = paste0(succ_actions, "<span style = 'color: purple3;'> Successful Actions</span> | ", fouls, " Fouls"),
    caption = "Viz by @TheNumbers_Game | Data from Opta"
  ) +
  theme(
    plot.background = element_blank(),
    legend.position = "none",
    plot.title = element_markdown(
      family = "Noto Sans",
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_markdown(
      family = "Noto Sans",
      size = 10
    ),
    plot.caption = element_markdown(family = "Noto Sans")
  )

# saving plot
ggsave("DA.jpg",
  width = 2560,
  height = 1920,
  units = "px",
  dpi = 256
)

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

# load in event data
events <- read.csv("NANPSG.csv")
target <- "Manuel Ugarte"
match <- "Nantes 0-2 PSG"
comp <- "Ligue 1"

# extracting player passes
player_passes <- events %>%
  mutate(
    outcome_type = outcomeType,
    player = name
  ) %>%
  filter(
    player == target,
    type == "Pass"
  )

# defining successful and unsuccessful passes
succ_pass <- length(which(player_passes$outcome_type == "Successful"))
unsucc_pass <- length(which(player_passes$outcome_type != "Successful"))

# defining pitch zones
zones <- data.frame(
  x = c(17, 33, 66, 83, 0, 0, 0, 0),
  xend = c(17, 33, 66, 83, 100, 100, 100, 100),
  y = c(0, 0, 0, 0, 21.25, 36.8, 63.1, 79),
  yend = c(100, 100, 100, 100, 21.25, 36.8, 63.1, 79)
)

# plotting pass map
player_passes %>%
  ggplot(aes(
    x = x,
    y = y
  )) +
  geom_segment(
    data = zones,
    aes(x = x, y = y, yend = yend, xend = xend),
    linetype = "dashed",
    colour = "grey",
    alpha = 0.5
  ) +
  annotate_pitch(
    fill = NA,
    colour = "black"
  ) +
  geom_segment(aes(
    xend = endX,
    yend = endY,
    colour = outcome_type,
    alpha = outcome_type
  )) +
  geom_point(
    aes(
      x = endX,
      y = endY,
      colour = outcome_type,
      fill = outcome_type
    ),
    alpha = 0.9,
    shape = 21,
    stroke = 1,
    size = 3
  ) +
  scale_colour_manual(values = c("purple3", "grey35")) +
  scale_fill_manual(values = c("purple3", "white")) +
  scale_alpha_manual(values = c(1, 0.5)) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_pitch() +
  labs(
    title = paste0(target, "'s Passes - ", match),
    subtitle = paste0(succ_pass, "<span style = 'color: purple3;'> Successful Passes</span> | ", unsucc_pass, "<span style = 'color: black;'> Unsuccessful Passes</span>"),
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
ggsave("PM.jpg",
  width = 2560,
  height = 1920,
  units = "px",
  dpi = 256
)

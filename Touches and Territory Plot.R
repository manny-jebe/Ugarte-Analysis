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
player_number <- "4"

# extracting player touches
player_touches <- events %>%
  mutate(
    outcome_type = outcomeType,
    player = name
  ) %>%
  filter(
    player == target,
    isTouch == "True"
  )

# creating convex hull (territory) for player touches
touch_hull <- player_touches %>%
  filter(
    between(x, mean(x) - sd(x), mean(x) + sd(x)),
    between(y, mean(y) - sd(y), mean(y) + sd(y))
  ) %>%
  slice(chull(x, y))

# defining pitch zones
zones <- data.frame(
  x = c(17, 33, 66, 83, 0, 0, 0, 0),
  xend = c(17, 33, 66, 83, 100, 100, 100, 100),
  y = c(0, 0, 0, 0, 21.25, 36.8, 63.1, 79),
  yend = c(100, 100, 100, 100, 21.25, 36.8, 63.1, 79)
)

# plotting touch map
player_touches %>%
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
  geom_polygon(
    data = touch_hull,
    fill = "purple3",
    colour = "purple3",
    alpha = 0.5
  ) +
  geom_point(
    shape = 21,
    stroke = 1,
    colour = "purple3",
    fill = "white",
    size = 2
  ) +
  geom_point(
    aes(
      x = mean(x),
      y = mean(y)
    ),
    size = 8,
    shape = 21,
    stroke = 1.5,
    colour = "black",
    fill = "purple3",
    alpha = 0.5
  ) +
  geom_text(
    aes(
      x = mean(x),
      y = mean(y)
    ),
    label = player_number,
    family = "Noto Sans",
    colour = "white"
  ) +
  theme_pitch() +
  labs(
    title = paste0(target, "'s Touches - ", match),
    subtitle = "Open player touches and territory plot",
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
ggsave("TM.jpg",
  width = 2560,
  height = 1920,
  units = "px",
  dpi = 256
)

# loading packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(worldfootballR)
library(scales)
library(paran)
library(factoextra)
library(gt)
library(gtExtras)
library(ggtext)
library(extrafont)


# selecting player
target <- "Manuel Ugarte Ribeiro"

# loading FBref season data
player.possession <- fb_big5_advanced_season_stats(
  season_end_year = 2024,
  stat_type = "possession",
  team_or_player = "player",
  time_pause = 7
)

player.passing <- fb_big5_advanced_season_stats(
  season_end_year = 2024,
  stat_type = "passing",
  team_or_player = "player",
  time_pause = 7
)

player.passtypes <- fb_big5_advanced_season_stats(
  season_end_year = 2024,
  stat_type = "passing_types",
  team_or_player = "player",
  time_pause = 7
)

player.shooting <- fb_big5_advanced_season_stats(
  season_end_year = 2024,
  stat_type = "shooting",
  team_or_player = "player",
  time_pause = 7
)

player.gca <- fb_big5_advanced_season_stats(
  season_end_year = 2024,
  stat_type = "gca",
  team_or_player = "player",
  time_pause = 7
)

player.misc <- fb_big5_advanced_season_stats(
  season_end_year = 2024,
  stat_type = "misc",
  team_or_player = "player",
  time_pause = 7
)

player.defense <- fb_big5_advanced_season_stats(
  season_end_year = 2024,
  stat_type = "defense",
  team_or_player = "player",
  time_pause = 7
)

team.possession <- fb_big5_advanced_season_stats(
  season_end_year = 2024,
  stat_type = "possession",
  team_or_player = "team",
  time_pause = 7
)

team.passing <- fb_big5_advanced_season_stats(
  season_end_year = 2024,
  stat_type = "passing",
  team_or_player = "team",
  time_pause = 7
)

# extracting team touches
team.touches <- team.possession %>%
  filter(Team_or_Opponent == "team") %>%
  mutate(TeamTouches = Touches_Touches) %>%
  dplyr::select(Squad, TeamTouches, Poss)

# extracting team passes
team.passes <- team.passing %>%
  filter(Team_or_Opponent == "team") %>%
  dplyr::mutate(TeamPasses = Cmp_Total) %>%
  dplyr::select(Squad, TeamPasses)

# joining dataframes
player.possession <- player.possession %>%
  left_join(team.touches) %>%
  left_join(team.passes)

player.data <- player.possession %>%
  left_join(player.passing) %>%
  left_join(player.passtypes) %>%
  left_join(player.shooting) %>%
  left_join(player.gca) %>%
  left_join(player.misc) %>%
  left_join(player.defense)

# preparing data for PCA
defender.data <- player.data %>%
  # filtering for midfielders with 7 90s
  filter(
    Pos %in% c("MF", "MF,DF", "MF,FW"),
    Mins_Per_90 >= 7
  ) %>%
  # possession and per 90 adjustments and metric calcs
  mutate(Poss_Adj = 50 / (100 - Poss)) %>%
  mutate(
    PAdjTklInt90 = (`Tkl+Int` * Poss_Adj) / Mins_Per_90,
    TklInt90 = `Tkl+Int` / Mins_Per_90
  ) %>%
  mutate(
    Carry100T = PrgC_Carries / Touches_Touches * 100,
    ProgPassPerc = PrgP / Cmp_Total * 100,
    LongPassPerc = Att_Long / Att_Total * 100,
    PAdjPass90 = Cmp_Total / Poss_Adj / Mins_Per_90,
    PAdjPCarry90 = PrgC_Carries / Poss_Adj / Mins_Per_90,
    PAdjFT90 = Final_Third / Poss_Adj / Mins_Per_90
  ) %>%
  mutate(TrueInt = Int + Blocks_Blocks) %>%
  mutate(
    PAdjTrueInt90 = TrueInt * Poss_Adj / Mins_Per_90,
    DefTouchPerc = `Def 3rd_Touches` / Touches_Touches * 100,
    MidTouchPerc = `Mid 3rd_Touches` / Touches_Touches * 100,
    AttTouchPerc = `Att 3rd_Touches` / Touches_Touches * 100,
    TouchCentrality = Touches_Touches / TeamTouches * 100,
    RecCentrality = Rec_Receiving / TeamPasses * 100,
    PAdjClear = Clr * Poss_Adj / Mins_Per_90,
    AerialRate = Won_percent_Aerial,
  ) %>%
  mutate(
    Pass90 = Cmp_Total / Mins_Per_90,
    FT90 = Final_Third / Mins_Per_90,
    TrueInt90 = TrueInt / Mins_Per_90,
    LongC90 = Cmp_Long / Mins_Per_90,
    LongA90 = Att_Long / Mins_Per_90,
    PCarry90 = PrgC_Carries / Mins_Per_90,
    PPass90 = PrgP / Mins_Per_90,
    Clear90 = Clr / Mins_Per_90,
    Def3rd90 = `Def 3rd_Touches` / Mins_Per_90,
    Mid3rd90 = `Mid 3rd_Touches` / Mins_Per_90,
    Att3rd90 = `Att 3rd_Touches` / Mins_Per_90,
    Touch90 = Touches_Touches / Mins_Per_90,
    AerialW90 = Won_Aerial / Mins_Per_90,
    FTC90 = Final_Third_Carries / Mins_Per_90,
    Cross90 = Crs / Mins_Per_90,
    TB90 = TB_Pass / Mins_Per_90,
    SCA90 = SCA90_SCA,
    xApKP = xAG / KP,
    Carry90 = Carries_Carries / Mins_Per_90
  ) %>%
  # selecting metrics for PCA
  dplyr::select(
    Player, PAdjTklInt90, Tkl_percent_Challenges, Pass90, PCarry90,
    ProgPassPerc, FT90, PPass90, PAdjClear, LongC90, LongA90,
    Def3rd90, Mid3rd90, Att3rd90, Touch90,
    PAdjTrueInt90, Carry100T, AerialRate, AerialW90, RecCentrality,
    TouchCentrality
  ) %>%
  na.omit() %>%
  # omitting players that appear multiple times
  filter(
    Player != "Azor Matusiwa",
    Player != "Nemanja Matić"
  ) %>%
  column_to_rownames(var = "Player")

# conducting PCA
def.pca <- prcomp(defender.data, scale. = TRUE)
def.data.n <- scale(defender.data)

# what makes up each PC?
var <- get_pca_var(def.pca)
a <- fviz_contrib(def.pca, "var", axes = 1, xtickslab.rt = 90) # default angle=45°
plot(a, main = "Variables percentage contribution of first Principal Components")


# creating scores
# extracting player ages
player.ages <- player.possession %>%
  filter(
    Player != "Álex Moreno",
    Player != "Issiaga Sylla"
  ) %>%
  filter(Mins_Per_90 >= 7) %>%
  dplyr::select(Player, Pos, Squad, Age)

# creating dataframe from PCA
similarities <- as.data.frame(def.pca$x) %>%
  rownames_to_column(var = "Player")

# locating our target player
location <- which(similarities == target,
  arr.ind = TRUE
)[1]

# calculating similarity scores
similarity.scores <- similarities %>%
  dplyr::select(Player, PC1, PC2, PC3, PC4, PC5) %>%
  # implementing Euclidean distancing
  mutate(
    Dist1 = (PC1 - PC1[location])^2,
    Dist2 = (PC2 - PC2[location])^2,
    Dist3 = (PC3 - PC3[location])^2,
    Dist4 = (PC4 - PC4[location])^2,
    Dist5 = (PC5 - PC5[location])^2
  ) %>%
  mutate(TotalDist = Dist1 + Dist2 + Dist3 + Dist4 + Dist5) %>%
  mutate(RootDist = sqrt(TotalDist)) %>%
  # calculating score
  mutate(Score = (1 - rescale(RootDist)) * 100) %>%
  # adding player ages
  left_join(player.ages) %>%
  dplyr::select(Player, Age, Squad, Score3)


# creating tables
# loading club badges
Images <- read.csv(
  file = "C:/Users/emmaj/OneDrive/Documents/Similarity Scores/Images.csv",
  header = TRUE, sep = ","
)

# adding badges to dataframe
scores <- left_join(similarity.scores, Images, by = "Squad") %>%
  # reuploading certain images
  mutate(Link = case_when(
    Squad == "Athletic Club" ~ "https://upload.wikimedia.org/wikipedia/en/thumb/9/98/Club_Athletic_Bilbao_logo.svg/212px-Club_Athletic_Bilbao_logo.svg.png
",
    Squad == "Atlético Madrid" ~ "https://upload.wikimedia.org/wikipedia/en/thumb/f/f4/Atletico_Madrid_2017_logo.svg/180px-Atletico_Madrid_2017_logo.svg.png
",
    TRUE ~ Link
  ))

# setting player's club colour for table
club_colour <- "darkblue"

# plotting player score table
plot <- scores %>%
  filter(
    Player != "Manuel Ugarte Ribeiro",
    # filtering for age
    Age < 44
  ) %>%
  dplyr::mutate(Bar = Score3) %>%
  arrange(desc(Score3)) %>%
  slice(1:12) %>%
  dplyr::select(Link, Player, Age, Squad, Score3, Bar) %>%
  rownames_to_column(var = "Rank") %>%
  gt() %>%
  gt_img_rows(columns = Link) %>%
  gt_plt_bar_pct(
    column = Bar, scaled = TRUE,
    fill = club_colour, background = "black"
  ) %>%
  gt_highlight_rows(rows = 1, fill = "#d6af36") %>%
  gt_highlight_rows(rows = 2, fill = "#d6d6d6") %>%
  gt_highlight_rows(rows = 3, fill = "#a17652") %>%
  gt_merge_stack(
    col1 = Player, col2 = Squad,
    palette = c("black", club_colour)
  ) %>%
  tab_header(
    title = md("Which Players Played Most Like Ugarte This Season?"),
    subtitle = html(str_c("<em>PC Similarity Scores. Scores calculated using several style-related metrics, reduced using Principal Component Analysis. Top 5 League AMs. Data from the 2023-24 season </em>"))
  ) %>%
  cols_label(
    Score3 = "Similarity Score",
    Bar = "",
    Link = ""
  ) %>%
  opt_row_striping() %>%
  cols_align("center") %>%
  cols_width(7 ~ px(125))

# saving table
gtsave(
  data = plot,
  filename = "Similarity Scores.png",
  vwidth = 1920,
  vheight = 1080
)

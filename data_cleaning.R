library(tidyverse)

#
player_bio = read.csv("~/STAT 460/player_bio.csv") %>%
  select(player_name, player_height_inches, player_weight)

season_stats_36 = read.csv("~/STAT 460/season_stats.csv") %>%
  select(player_name, POS = pos, AGE = age, TM = tm, G = g, GS = gs, MP = mp, FG = fg, FGA = fga, 
         FG_pct = fg_2,
         FG3 = x3p, FGA3 = x3pa, FG3_pct = x3p_2, FG2 = x2p, FGA2 = x2p_2, FT = ft, FTA = fta, 
         FT_pct = ft_2,
         ORB = orb, DRB = drb, TRB = trb, AST = ast, STL = stl, BLK = blk, TOV = tov, Fouls = pf,
         PTS = pts)

season_stats_36[season_stats_36$player_name == "Nikola JokiÄ‡",]$player_name = "Nikola Jokic"

dups = season_stats_36 %>%
  group_by(player_name) %>% 
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1)

dups_stats = season_stats_36 %>%
  filter(player_name %in% dups$player_name) %>%
  filter(TM == "TOT")

non_dups = season_stats_36 %>%
  group_by(player_name) %>% 
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n == 1) 

non_dups_stats = season_stats_36 %>%
  filter(player_name %in% non_dups$player_name)

stats = rbind(dups_stats, non_dups_stats)
  
raptor = read.csv("~/STAT 460/latest_RAPTOR_by_player.csv") %>%
  select(player_name, mp, raptor_offense, raptor_defense, raptor_total) 

df = player_bio %>%
  inner_join(raptor, by = c("player_name")) %>%
  inner_join(stats, by = "player_name")

write.csv(df, "final_df.csv")


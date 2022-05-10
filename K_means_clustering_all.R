library(tidyverse)
library(ggbiplot)
library(class)
library(gghighlight)

embiid = c(84, 280, 4.127825, 3.57639, 7.704215, 2297, 10.4, 20.9, 0.499, 1.5,  3.9, 0.371, 9, 17, 0.529, 10.2, 12.6, 0.814, 2.3,
  10.2, 12.5, 4.5, 1.2, 1.6, 3.4, 2.8, 32.6)

clustering_df = read.csv("~/STAT 460/460Project/clustering_df.csv", row.names = "player_name")  %>%
  select(-X)

clustering_df[nrow(clustering_df) + 1,] = embiid
rownames(clustering_df)[rownames(clustering_df) == "270"] <- "Joel Embiid"

clustering_df = clustering_df %>%
  select(-FG, -FG3, -FG2, -FT) %>%
  filter(MP >= 1000) %>%
  select(-MP) %>%
  scale()
  
set.seed(1)
cluster = kmeans(x = clustering_df, centers = 5)

ggbiplot(prcomp(clustering_df), labels = rownames(clustering_df),
         groups = as.factor(cluster$cluster)) +
  theme_minimal() +
  guides(color = F)



km_centers = data.frame(cluster$centers)


km_centers$Cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3',
                        'Cluster 4', 'Cluster 5') 

km_centers <- km_centers %>% 
  dplyr::rename(height = player_height_inches,
         weight = player_weight,
         RAP_T = raptor_total, RAP_O = raptor_offense, RAP_D = raptor_defense) %>%
  pivot_longer(!Cluster, names_to = 'feature', values_to = 'z_val') 


km_centers$feature <- factor(km_centers$feature, levels=c("RAP_T", "RAP_D",
                                                          "RAP_O",
                                                          'PTS', 'AST', 'FGA', 'FG_pct', 
                                                          'FGA2','FG2_pct', 'FGA3', 'FG3_pct', 
                                                          'FTA', 'FT_pct','ORB', 'DRB', 
                                                          'TRB','STL', 'BLK', 'TOV', "Fouls",
                                                          'height',
                                                          'weight'))



km_centers$Cluster <- factor(km_centers$Cluster, levels=c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4',
                                                          'Cluster 5'))

km_centers %>% 
  ggplot(aes(x=feature, y=z_val, color=Cluster)) + 
  geom_point() + 
  gghighlight(use_direct_label = FALSE) + 
  facet_wrap(~ Cluster, ncol=3) + 
  labs(x = "Predictor", y = "Cluster Center", 
       title = "Visualizing K-Means Cluster Makeups") + 
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), 
        panel.grid.minor = element_blank())


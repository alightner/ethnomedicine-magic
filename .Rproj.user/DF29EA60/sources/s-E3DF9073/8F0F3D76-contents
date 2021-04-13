library(maps)
world_map <- map_data("world")
load('eHRAF-data/coded-data.rda')
load('eHRAF-data/culture_coord.rda')
culture_world <- culture_world[culture_world$culture_id %in% df$culture_id,]

eHRAF_world_map <- 
  ggplot(world_map, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill="lightgray", colour = "white", alpha=0.7) +
  geom_point(
    data = culture_world, 
    aes(
      longitude,
      latitude, 
      colour = subsistence_type, 
      shape = subsistence_type, 
      size = records
    )
  ) +
  geom_text_repel(
    data = culture_world,
    aes(longitude, latitude, label = culture),
    force = 3,
    size = 2.75,
    colour = '#333333'
  ) +
  scale_y_continuous(limits = c(-60, 80)) +
  coord_fixed() +
  labs(x='', y='', shape = 'Subsistence', colour = 'Subsistence', size = 'Text records') +
  guides(shape = guide_legend(override.aes = list(size = 4))) +
  theme_bw(11)

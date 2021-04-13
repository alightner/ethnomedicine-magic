da <- dl %>% 
  dplyr::select(
    id, starts_with('healthprob')
  ) %>% 
  pivot_longer(-id, names_to='health_consult', values_to='consult') %>% 
  mutate(health_consult=str_replace(health_consult, 'friend', 'ff'),
         health_consult=str_replace(health_consult, 'family', 'ff'),
         health_consult=str_replace(health_consult, 'ff', 'friends/family')) %>%
  filter(consult>0) 

da$health_consult <- unlist(lapply(str_split(da$health_consult, pattern='_'), function(x) x[2]))
da$consult <- NULL

d2 <- data.frame()
for(i in unique(da$id)) {
  tmp <- rep(NA, 3)
  resp <- unique(da$health_consult[da$id==i])
  tmp[1:length(resp)] <- resp
  d2 <- rbind(d2, data.frame(
    first_health=resp[1], second_health=resp[2], third_health=resp[3], id=i
  ))
}
d2 <- d2 %>% left_join(data.frame(id=dl$id, christian=dl$christian), by='id')
d2$second_health[is.na(d2$second_health)] <- "don\'t\nknow"
d2$third_health[is.na(d2$third_health)] <- "don\'t\nknow"
d2 <- d2 %>% count(first_health, second_health, third_health, christian)

d2$religion <- d2$christian
d2$religion <- ifelse(d2$religion==1, "christian", "traditional")

d2$religion[d2$religion=='christian'] <- 'Christians'
d2$religion[d2$religion=='traditional'] <- 'Traditional'

field_alluvial2 <- 
  ggplot(as.data.frame(d2),
         aes(y=n, axis1=first_health, axis2=second_health, axis3=third_health)) +
    geom_flow(aes(fill=factor(religion)), colour='black') +
    facet_wrap(~religion) +
  geom_stratum(width = 0.275, reverse = TRUE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = TRUE, size=3) +  # , size=3.15
  theme_void() +
  scale_fill_viridis_d() +
  labs(x='', y='', fill='') +
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x =element_blank(),
    axis.title.x = element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y =element_blank(),
    axis.title.y = element_blank()
  )

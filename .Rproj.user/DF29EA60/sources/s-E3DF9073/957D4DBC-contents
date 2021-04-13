source('package-list.R')
load('field-data/field-assist-data')
source('field-dictionary.R')

# Descriptive stats -------------------------------------------------------

fperc_female <- 100 - round(100*mean(dl$male))
fperc_christian <- round(100*mean(dl$christian, na.rm=TRUE))
fperc_traditional <- 100-fperc_christian
perc_srconflict <- round(100*mean(dl$sr_conflict, na.rm=TRUE))
trad_dk <- (xtabs(~medexp_dk+christian, data=dl))[2,1]
chr_dk <- (xtabs(~medexp_dk+christian, data=dl))[2,2]

# Health problems: Who assists? -------------------------------------------

health_assist <- 
  dl %>% 
  dplyr::select(
    id,
    starts_with('healthprob')
  ) %>% 
  mutate(
    healthprob1_dk = abs(rowSums(across(starts_with('healthprob1')))-1),
    healthprob2_dk = abs(rowSums(across(starts_with('healthprob2')))-1),
    healthprob3_dk = abs(rowSums(across(starts_with('healthprob3')))-1)
  ) %>% 
  pivot_longer(-id, names_to='health_consult', values_to='consult') %>% 
  mutate(health_consult=str_replace(health_consult, 'friend', 'ff'),
         health_consult=str_replace(health_consult, 'family', 'ff'),
         health_consult=str_replace(health_consult, 'ff', 'friends/family')) %>% 
  group_by(health_consult) %>% 
  summarise(
    freq=sum(consult),
    avg=mean(consult)
  ) %>% 
  dplyr::mutate(
    priority=case_when(
      stringr::str_detect(health_consult, 'healthprob1') ~ "first",
      stringr::str_detect(health_consult, 'healthprob2') ~ "second",
      TRUE ~ "third"
    )
  )

health_assist$health_consult <- unlist(lapply(str_split(health_assist$health_consult, pattern='_'), function(x) x[2]))
health_assist$priority <- factor(health_assist$priority, levels=c('first', 'second', 'third'))
health_assist$priority <- factor(health_assist$priority, levels=c('third', 'second', 'first'))
health_assist$health_consult[health_assist$health_consult=='dk'] <- "don't know"
health_assist$health_consult <- factor(health_assist$health_consult, levels=c(
  'clinic', 'friends/family', 'laibon', 'church', 'self', "don't know"
))

field_whoHelps <- 
  ggplot(health_assist) + 
  geom_mosaic(aes(weight=freq, x=product(priority), fill=health_consult)) +
  theme_bw(base_size=10) + 
  labs(x='', y='', fill='') + 
  coord_flip() + 
  scale_fill_viridis_d() + 
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x =element_blank(),
    axis.title.x = element_blank()
  ) +
  annotate('text', x=0.8, y=-0.05, label='First\nchoice', size=3) +
  annotate('text', x=0.4, y=-0.05, label='Second\nchoice', size=3) +
  annotate('text', x=0.1, y=-0.05, label='Third\nchoice', size=3)

# Alluvial plot -----------------------------------------------------------

source('FIELD-alluvial.R')

# Medical explanations ----------------------------------------------------

source('FIELD-medical-explanations.R')

# Why patronize the laibon? -----------------------------------------------

source('FIELD-laibon-trust.R')

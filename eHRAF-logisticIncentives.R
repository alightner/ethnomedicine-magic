d0 <- df[c('textid', socialVars_layperson, 'secretive_knowledge')] %>% 
  pivot_longer(-textid, names_to='var_name', values_to='presence')
d0$var_type <- NA
d0$var_type[d0$var_name=='evidence_success' | d0$var_name=='evidence_failure'] <- 'evidence of success'
d0$var_type[d0$var_name=='patron_socioeconomic' | d0$var_name=='patronage_based_efficacy'] <- 'patronage based efficacy'
d0$var_type[d0$var_name=='common_problem' | d0$var_name=='uncommon_serious'] <- 'helps w/ uncommon\n& serious problems'
d0$var_type[d0$var_name=='reputation_efficacy' | d0$var_name=='anti_reputation_efficacy'] <- 'reputation for efficacy'
d0$var_type[d0$var_name=='low_status' | d0$var_name=='prestige'] <- 'has prestige/status'
d0$var_type[d0$var_name=='trust' | d0$var_name=='distrust'] <- 'trustworthy'
d0$var_type[d0$var_name=='confers_benefits' | d0$var_name=='imposes_costs'] <- 'confers benefits'
d0$var_type[d0$var_name=='no_payment' | d0$var_name=='receives_payment'] <- 'offers free services'
d0$var_type[d0$var_name=='social_learning' | d0$var_name=='secretive_knowledge'] <- 'teaches others'
d0 <- d0[complete.cases(d0),]
d0$cost_benefit <- NA
d0$cost_benefit[d0$var_name %in% vars_rec_positive] <- 'Incentive'
d0$cost_benefit[d0$var_name %in% vars_rec_negative] <- 'Disincentive'
d0$cost_benefit[d0$var_name=='secretive_knowledge'] <- 'Disincentive'
d0$cost_benefit <- ifelse(d0$cost_benefit=='Incentive',1,0)
d0$cost_benefit[d0$var_name=='patron_socioeconomic'] <- 0
d0$cost_benefit[d0$var_name=='common_problem'] <- 0

mvars <- d0 %>% 
  group_by(var_type) %>% 
  summarise(mod_vars=unique(var_type))
pn_mods <- d0 %>% 
  group_by(var_type) %>%
  group_map(~broom::tidy(glm(presence ~ cost_benefit, data=.x, family=binomial)))
pnm2 <- tibble(
  Model=pn_mods,
  Estimate=purrr::map_dbl(Model, ~.x$estimate[2]),
  SE=purrr::map_dbl(Model, ~.x$std.error[2])
)
pnm2$Estimate2 <- exp(pnm2$Estimate)
pnm2$upperCI2 <- exp(pnm2$Estimate+2*pnm2$SE)
pnm2$lowerCI2 <- exp(pnm2$Estimate-2*pnm2$SE)
pnm2$variable <- mvars$mod_vars
laypersons_models <- pnm2

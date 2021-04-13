formula_string = "{outcome} ~ acculturation + (1|culture_id/author_id)"
dd <- d
inv_dict <- names(var_dict2)
names(inv_dict) <- var_dict2
colnames(dd) <- inv_dict[colnames(dd)]
dd <- dd %>% dplyr::select(-acculturation, everything(), acculturation)
dd <- cbind(dd, data.frame(culture_id=df$culture_id, author_id=df$author_id))

acculturationMods <- textrecord_support(dd, formula_string, nonNum=3)
fe_acculturation <- data.frame(vars=acculturationMods$vars, effect=NA, lowerCI=NA, upperCI=NA, stringsAsFactors = FALSE)
fe_acculturation$effect <- unlist(lapply(acculturationMods$Tidy, function(x) x$estimate[2]))
fe_acculturation$lowerCI <- unlist(lapply(acculturationMods$Tidy, function(x) x$conf.low[2]))
fe_acculturation$upperCI <- unlist(lapply(acculturationMods$Tidy, function(x) x$conf.high[2]))

fe_acculturation <- fe_acculturation %>% left_join(data.frame(vars=names(colMeans(df[fe_acculturation$vars])), 
                                          evidence=colMeans(df[fe_acculturation$vars])), by='vars')
fe_acculturation <- fe_acculturation[fe_acculturation$evidence>=0.1,]
fe_acculturation$vars <- var_dict2[fe_acculturation$vars]

fe_acculturation$alpha <- sign(fe_acculturation$lowerCI)==sign(fe_acculturation$upperCI)

fe_acculturation$class <- 'Others'
fe_acculturation$class[fe_acculturation$vars %in% var_dict2[religious_vars]] <- 'Religiosity'
fe_acculturation$class[fe_acculturation$vars %in% var_dict2[market_vars]] <- 'Market specialists'
# fe_acculturation$class[fe_acculturation$vars %in% var_dict2[mentor_vars]] <- 'Mentorship'
fe_acculturation$class[fe_acculturation$vars %in% var_dict2[efficacy_vars]] <- 'Efficacy'
# fe_acculturation$class[fe_acculturation$vars %in% var_dict2[anti_efficacy_vars]] <- 'Inefficacy'
# fe_acculturation$class[fe_acculturation$vars %in% var_dict2[anti_mentor_vars]] <- 'Anti-mentorship'

fe_acculturation$class <- factor(fe_acculturation$class, levels=c('Religiosity', 
                                                                  'Efficacy', 'Market specialists',
                                                                  'Others'))

eHRAF_acculturation <- 
  ggplot(fe_acculturation, aes(x=exp(effect), y=reorder(vars, effect), colour=class)) +
  geom_point() +
  geom_errorbarh(aes(xmin=exp(lowerCI), xmax=exp(upperCI)), height=0, lwd=1.75, alpha=0.4) +
  theme_bw() +
  scale_x_log10() +
  scale_colour_manual(values=c(viridis::magma(11)[8], 
                               # viridis::magma(11)[1],
                               '#FFD300',
                               viridis::magma(11)[4],
                               '#666666')) +
  geom_vline(xintercept=1, alpha=0.4, linetype=3) +
  labs(x='Odds ratio', y='', colour='')
  
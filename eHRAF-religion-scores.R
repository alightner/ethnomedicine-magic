
# Religiosity scores per text record --------------------------------------

religious_vars <- c('Supernatural',
  'Religious leadership',
  'Costly ritual',
  'Expert prescribes behavior/ritual',
  'Learns by revelation',
  'Divination')
dv <- d
religious_scores <- rowMeans(dv[religious_vars])
dv <- dv %>% dplyr::select(-religious_vars)
dv$Religion <- religious_scores
dv$culture_id <- df$culture_id
c_df2$religion_score <- rowSums(c_df2[inv_dict[religious_vars]])

# Elasticnet model takes time; uncomment to run or load directly load
# y <- c_df2$religion_score
# x <- c_df2[sapply(c_df2, is.numeric)]
# x <- x[!(colnames(x) %in% inv_dict[religious_vars])]
# x <- x[sapply(x, function(x) sum(is.na(x))==0)] %>% 
#   dplyr::select(-c(longitude,
#                    subsistence_id,
#                    records,
#                    case, model,
#                    religion_score)) %>% 
#   scale() %>% as.data.frame() %>% as.matrix()
# m_relscore <- cv.glmnet(x=x,y=y, family='poisson', alpha=1, relax=TRUE, trace=TRUE)
# # plot(m_relscore)

load('eHRAF-data/religiousScoreModel.rda')

coefs <- coef(m_relscore, s=m_relscore$lambda.1se)
coefs2 <- coefs[coefs!=0]
rn <- rownames(coefs)
names(coefs2) <- rn[which(coefs!=0)]
coefs2 <- coefs2[-which(names(coefs2) %in% c('(Intercept)', 'no_payment', 'anti_hierarchy'))]
coefs2 <- coefs2[which(names(coefs2) %in% inv_dict[colnames(d)])]
names(coefs2) <- var_dict2[names(coefs2)]

eHRAF_religionScorePlot <- ggdotchart(coefs2) +
  labs(x='Estimate') +
  theme_bw(12) +
  geom_vline(xintercept=0, alpha=0.4, linetype=3) +
  ggtitle('Correlates of religiosity')

c_df$religion_score <- rowSums(c_df[inv_dict[religious_vars]])

# Region level scores -----------------------------------------------------

m2 <- glmer(religion_score ~ region + (1|culture_id), family=poisson, data=c_df, nAGQ=0)
emm2 <- emmeans(m2, spec='region', type='response')
emmDF <- as.data.frame(summary(emm2))
emmDF$region <- as.character(emmDF$region)
emmDF <- emmDF[emmDF$region!='Middle East',]   # small sample/ very noisy
emmDF$region[emmDF$region=='Middle America and the Caribbean'] <- "Central America"

Regional_ReligionScore <- 
  ggplot(emmDF, aes(x=rate, y=region)) +
  geom_point(size=2.5) +
  geom_errorbarh(aes(xmin=rate-(2*SE), xmax=rate+(2*SE)), height=0.1, lwd=1) +
  theme_bw() +
  labs(x='\nMean religiosity score', y='', colour='') +
  theme(strip.text.y=element_text(angle=0)) +
  theme(legend.position = "none")

# Subsistence level scores ------------------------------------------------

m2 <- glmer(religion_score ~ subsistence_type + (1|culture_id), family=poisson, data=c_df)
emm2 <- emmeans(m2, spec='subsistence_type', type='response')
emmDF <- as.data.frame(summary(emm2))
emmDF$subsistence_type <- as.character(emmDF$subsistence_type)

Subsistence_ReligionScore <- 
  ggplot(emmDF, aes(x=rate, y=subsistence_type)) +
  geom_point(size=2.5) +
  geom_errorbarh(aes(xmin=rate-(2*SE), xmax=rate+(2*SE)), height=0.1, lwd=1) +
  theme_bw() +
  labs(x='\nMean religiosity score', y='', colour='') +
  theme(strip.text.y=element_text(angle=0)) +
  theme(legend.position = "none")


## Cross-cultural analyses of the supernatural ----------------------------

# Regional emmeans --------------------------------------------------------

c_df <- df %>% left_join(
  culture_df, by='culture_id'
)
m2 <- glmer(supernatural ~ region + (1|culture_id), family=binomial, data=c_df, nAGQ=0)
emm2 <- emmeans(m2, spec='region', type='response')
emmDF <- as.data.frame(summary(emm2))
emmDF$region <- as.character(emmDF$region)
emmDF <- emmDF[emmDF$region!='Middle East',]   # small sample/ very noisy
emmDF$region[emmDF$region=='Middle America and the Caribbean'] <- "Central America"

Regional_emmeans <- 
  ggplot(emmDF, aes(x=100*prob, y=region)) +
  geom_point(size=2.5) +
  geom_errorbarh(aes(xmin=100*prob-(200*SE), xmax=100*prob+(200*SE)), height=0.1, lwd=1) +
  theme_bw() +
  labs(x='\nPercent supernatural text records', y='', colour='') +
  theme(strip.text.y=element_text(angle=0)) +
  theme(legend.position = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%"))

# Subsistence emmeans -----------------------------------------------------

m2 <- glmer(supernatural ~ subsistence_type + (1|culture_id), family=binomial, data=c_df)
emm2 <- emmeans(m2, spec='subsistence_type', type='response')
emmDF <- as.data.frame(summary(emm2))
emmDF$subsistence_type <- as.character(emmDF$subsistence_type)

Subsistence_emmeans <- 
  ggplot(emmDF, aes(x=100*prob, y=subsistence_type)) +
  geom_point(size=2.5) +
  geom_errorbarh(aes(xmin=100*prob-(200*SE), xmax=100*prob+(200*SE)), height=0.1, lwd=1) +
  theme_bw() +
  labs(x='\nPercent supernatural text records', y='', colour='') +
  theme(strip.text.y=element_text(angle=0)) +
  theme(legend.position = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%"))

# Culture and text predictors of supernatural -----------------------------

c_df2 <- c_df[sapply(c_df, is.numeric)]
tmp <- sort(sapply(c_df2, function(x) sum(is.na(x))))
c_df2 <- c_df2[colnames(c_df2) %in% names(tmp[tmp<35])]
c_df2$latitude <- abs(c_df2$latitude)
culture_df$latitude <- abs(culture_df$latitude)

## This elasticnet model takes a while to run. Uncomment to do so, or load the output below.

# y <- c_df2$supernatural
# x <- c_df2 %>%
#   dplyr::select(-supernatural) %>%
#   replace(is.na(.), 0) %>%
#   as.matrix()
# m2 <- cv.glmnet(x=x, y=y, family='binomial', alpha=1, relax=TRUE, trace=TRUE)
# plot(m2)

load('eHRAF-data/supernaturalCultureLasso2.rda')

coefs <- coef(m2, s=m2$lambda.1se)
coefs2 <- coefs[coefs!=0]
rn <- rownames(coefs)
names(coefs2) <- rn[which(coefs!=0)]
coefs2 <- coefs2[-which(names(coefs2)=='(Intercept)')]
tmp <- sort(colSums(c_df2[colnames(c_df2) %in% names(coefs2)]))
tmp <- names(tmp[tmp>round(0.05*nrow(c_df2))])
coefs2 <- coefs2[names(coefs2) %in% tmp]
names(coefs2) <- var_dict2[names(coefs2)]
coefs2 <- coefs2[names(coefs2) %in% colnames(d)]

corrSupernatural_FULL <- ggdotchart(exp(coefs2)) +
  labs(x='Odds ratio') +
  theme_bw(12) +
  geom_vline(xintercept=1, alpha=0.4, linetype=3) +
  ggtitle('Correlates of supernatural theories of disease') +
  scale_x_log10()

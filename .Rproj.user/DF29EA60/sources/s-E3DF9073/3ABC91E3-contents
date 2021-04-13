rm(list=ls())
source('package-list.R')
source('functions.R')
load('eHRAF-data/var_dict2')
inv_dict <- names(var_dict2)
names(inv_dict) <- var_dict2
source('eHRAF-variables.R')

df <- read_csv('eHRAF-data/coded-data.csv')
dm <- read_csv('eHRAF-data/IRR-domains.csv')
culture_df <- read_csv('eHRAF-data/culture-data.csv')
d <- df[sapply(df, is.numeric)]
colnames(d) <- var_dict2[colnames(d)]

source('eHRAF-worldmap.R')
source('eHRAF-models.R')

# Minimum spanning tree ---------------------------------------------------

source('eHRAF-mst.R')

# Descriptive stats -------------------------------------------------------

perc_religious_leader <- round(mean(df$religious_leader)*100)
perc_supernatural <- round(mean(df$supernatural)*100)
perc_divination <- round(mean(df$divination)*100)
supernat_nat_perc <- round(mean(df0$supernatural==1 & df0$natural==1)*100)
supernat_perc <- round(mean(df0$supernatural==1 & df0$natural==0)*100)
nat_perc <- round(mean(df0$supernatural==0 & df0$natural==1)*100)

# Incentives and disincentives --------------------------------------------

# source('eHRAF-compile-evidence.R')    # Uncomment to run; takes some time because of the cluster bootstrap
load('eHRAF-data/specialist_evidence.rda')
load('eHRAF-data/layperson_evidence.rda')
specialist_evidence <- specialist_evidence[specialist_evidence$vars %in% colnames(d),]
layperson_evidence <- layperson_evidence[layperson_evidence$vars %in% colnames(d),]
specialist_evidence$cat_id[specialist_evidence$cat_id=='Specialist attributes'] <- 'Social\nattributes'
specialist_evidence$cat_id[specialist_evidence$cat_id=='Cognitive'] <- 'Knowledge\nattributes'
layperson_evidence$incentive <- factor(layperson_evidence$incentive, levels=c('Incentives', 'Disincentives'))

eHRAF_specialists <- 
  ggplot(specialist_evidence, aes(x=estimate, y=reorder(vars, estimate), colour=type)) +
  geom_point() +
  geom_errorbarh(aes(xmin=lowerCI, xmax=upperCI), height=0, lwd=1.75, alpha=0.3) +
  theme_bw(base_size=10) +
  facet_grid(cat_id~., space='free', scales='free') +
  theme(strip.text.y = element_text(angle=0, hjust=0)) +
  labs(x='', y='', colour='') +
  scale_colour_manual(values=c(viridis::magma(11)[8],
                               viridis::magma(11)[4])) +
  scale_x_continuous(labels = function(x) paste0(x, "%"))

eHRAF_laypersons <- 
  ggplot(layperson_evidence, aes(x=estimate, y=reorder(vars, estimate), 
                               colour=type)) +
  geom_point() +
  geom_errorbarh(aes(xmin=lowerCI, xmax=upperCI), height=0, lwd=1.75, alpha=0.3) +
  theme_bw(base_size=10) +
  facet_grid(incentive~., space='free', scales='free') +
  theme(strip.text.y = element_text(angle=0, hjust=0)) +
  labs(x='', y='', colour='') +
  scale_colour_manual(values=c(viridis::magma(11)[8],
                               viridis::magma(11)[4])) +
  scale_x_continuous(labels = function(x) paste0(x, "%"))

source('eHRAF-logisticIncentives.R')

eHRAF_laypersons3 <- 
  ggplot(laypersons_models, aes(x=Estimate2, y=reorder(variable, Estimate2))) +
  geom_point() +
  geom_vline(xintercept=1, alpha=0.2) +
  geom_errorbarh(aes(xmin=lowerCI2, xmax=upperCI2),
                 lwd=1.75, height=0, alpha=0.3) +
  theme_bw() +
  labs(x='\nEstimate (odds ratios)', y='') +
  scale_x_log10()

# Inter-rater reliability -------------------------------------------------

source('eHRAF-IRR-check.R')

# Domain plot -------------------------------------------------------------

source('eHRAF-domainplot.R')

# Cultural group level variation ------------------------------------------

source('eHRAF-cultural-variation2.R')
source('eHRAF-SCCS.R')

# Acculturation -----------------------------------------------------------

source('eHRAF-acculturation.R')

# Religiosity scores ------------------------------------------------------

source('eHRAF-religion-scores.R')

# Comparison of different "explanatory predictions" -----------------------
## This section computes and plots "total scores"

source('eHRAF-models.R')

mlist <- list(
  list('Religiosity', religious_vars),
  list('Efficacious healing', efficacy_vars),
  list('Inefficacious healing', anti_efficacy_vars),
  list('Market specialists', market_vars),
  list('Prestigious mentors/teachers', mentor_vars),
  list('Low status/distrusted', anti_mentor_vars)
)

modScoreList <- lapply(mlist, 
                       function(i)
                       {
                         model <- i[[1]]
                         model_vars <- i[[2]]
                         w <- rep(length(model_vars), nrow(df))
                         
                         if(sum(as.matrix(df[,model_vars])) !=0){
                           m2 <- glmer(rowSums(df[,model_vars])/w ~ 1+(1|culture_id/author_id), family=binomial, weights=w, data=df, nAGQ=0)
                           # model
                           estimate <- logit.inv(fixef(m2)[['(Intercept)']])*100
                           ci <- logit.inv(confint(m2, method='Wald')['(Intercept)',])
                           lowerCI <- ci[[1]]*100
                           upperCI <- ci[[2]]*100
                           
                         } else {
                           estimate <- 0
                           lowerCI <- 0
                           upperCI <- 0
                         }
                         model_d <- data.frame(estimate, lowerCI, upperCI, mod_id=model)
                         return(model_d)
                       }
)

modScored <- data.table::rbindlist(modScoreList)  

tmp_vars <- rbind(layperson_evidence %>% dplyr::select(-incentive), specialist_evidence)
tmp_vars$vars <- inv_dict[tmp_vars$vars]
tmp_vars <- tmp_vars %>% dplyr::filter(vars %in% unlist(lapply(mlist, function(x) unlist(x[[2]]))))
tmp_vars$vars <- var_dict2[tmp_vars$vars]

modScored$type <- 'score'
modScored <- modScored %>% 
  dplyr::select(vars=mod_id, estimate:upperCI, type) 
modScored <- rbind(modScored, tmp_vars %>% dplyr::select(-cat_id))  

modScored$model <- NA
modScored$model[inv_dict[modScored$vars] %in% religious_vars] <- 'Religiosity'
modScored$model[inv_dict[modScored$vars] %in% efficacy_vars] <- 'Efficacious healing'
modScored$model[inv_dict[modScored$vars] %in% anti_efficacy_vars] <- 'Inefficacious healing'
modScored$model[inv_dict[modScored$vars] %in% market_vars] <- 'Market specialists'
modScored$model[inv_dict[modScored$vars] %in% mentor_vars] <- 'Prestigious mentors\n& teachers'
modScored$model[inv_dict[modScored$vars] %in% anti_mentor_vars] <- 'Low status/distrusted'
modScored$model[modScored$type=='score'] <- modScored$vars[modScored$type=='score']
modScored$model[modScored$model=='Prestigious mentors/teachers'] <- 'Prestigious mentors\n& teachers'
modScored$vars[modScored$type=='score'] <- 'Total score'

modScored$vars <- 
  factor(modScored$vars,
         levels=c(
           'Total score',
           unique(
             subset(
               modScored, vars!='Total score'
             )$vars[order(subset(
               modScored, vars!='Total score'
             )$estimate, decreasing=TRUE)], order=TRUE
           )
         )       
  )
modScored$vars <- factor(modScored$vars, levels=rev(levels(modScored$vars)))
modScored$model <- 
  factor(modScored$model,
         levels=modScored$model[
           modScored$vars=='Total score'
         ][
           order(modScored$estimate[modScored$vars=='Total score'])
         ]  
  )
modScored$model <- factor(modScored$model, levels=rev(levels(modScored$model)))

eHRAF_modelPlot <- 
  ggplot(modScored, aes(x=estimate, y=vars, colour=type)) +
  geom_point() +
  geom_errorbarh(aes(xmin=lowerCI, xmax=upperCI), height=0, lwd=1.75, alpha=0.3) +
  theme_bw(base_size=10) +
    facet_grid(model~., space='free', scales='free') +
  labs(x='', y='', colour='') +
    theme(strip.text.y=element_text(angle=0, hjust=0)) +
  scale_colour_manual(values=c(viridis::magma(11)[8],
                               '#008080',
                               viridis::magma(11)[4])) +
  scale_x_continuous(labels = function(x) paste0(x, "%"))

# List of the variables ---------------------------------------------------

# Heatmap for variable list
wide_x <- rbind(
  unique(data.frame(vars=specialist_evidence$vars, category=specialist_evidence$cat_id)),
  unique(data.frame(vars=layperson_evidence$vars, category=layperson_evidence$incentive)),
  unique(data.frame(vars=var_dict2[religious_vars], category='Religiosity')),
  unique(data.frame(vars=var_dict2[market_vars], category='Market\nspecialists')),
  unique(data.frame(vars=var_dict2[efficacy_vars], category='Efficacious\nhealing')),
  unique(data.frame(vars=var_dict2[mentor_vars], category='Prestigious\nmentors')),
  unique(data.frame(vars=var_dict2[anti_mentor_vars], category='Low status')),
  unique(data.frame(vars=var_dict2[anti_efficacy_vars], category='Inefficacious\nhealing'))
)

tmp <- as.data.frame(table(wide_x))
tmp <- as_tibble(table(wide_x))
tmp <- tmp %>% pivot_wider(names_from=category, values_from=n)

tmp[(nrow(tmp)+1):(nrow(tmp)+4),] <- tibble(data.frame(vars=colnames(d)[!(colnames(d) %in% tmp$vars)],
                                                       tibble(as.data.frame(matrix(0, nrow=4, ncol=ncol(tmp)-1)))))

study1_vars <- tmp
eHRAF_variables <- hagenheat3(study1_vars, seriation_method='PCA_angle') +
  guides(fill=FALSE) +
  scale_fill_gradient2(low='white', high='#641A80FF')


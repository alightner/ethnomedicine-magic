vars <- read_csv('sccs/variables.csv')
codes <- read_csv('sccs/codes.csv')
socdf <- read_csv('sccs/societies.csv')
scdf <- read.csv('sccs/data.csv',
                 colClasses=c(
                   'character','character','numeric','character','numeric', rep('NULL',4)),
                 header=TRUE)
cdvar <- vars %>% dplyr::select(var_id=id, category, title, type)
scdf <- scdf %>% left_join(cdvar, by='var_id')
var_list2 <- cdvar$var_id[cdvar$type=='Ordinal' | cdvar$type=='Continuous']

# Generating the SCCS numeric dataframe; uncomment to run (slows rendering of paper)
# ds <- data.frame(sccs_id=unique(scdf$soc_id))
# 
# for(i in 1:length(var_list2)) {
#   d2 <- as_tibble(scdf[scdf$var_id==var_list2[i],]) %>%
#     dplyr::select(sccs_id=soc_id,   # this is for the culture codes key to be used later
#                   #sccs_year=year,  # could be useful
#                   code)
#   colnames(d2) <- c('sccs_id', vars$title[vars$id==var_list2[i]])
#   ds <- ds %>% left_join(d2, by='sccs_id')
# }

load('sccs/numericSCCS.rda')  # this is the saved result of the above loop that is commented out

# Assessing and filtering columns based on missing data
# dotchart(sort(sapply(ds, function(x) sum(is.na(x)))), labels='')
tmp <- sapply(ds, function(x) sum(is.na(x)))
upperlim <- 100
t_col <- rep(NA, upperlim)
t_row <- rep(NA, upperlim)
for(i in 0:(upperlim-1)) {
  tmp_d <- ds[colnames(ds) %in% names(tmp[tmp <= i])]
  tmp_d <- tmp_d[complete.cases(tmp_d),]
  t_col[i+1] <- ncol(tmp_d)
  t_row[i+1] <- nrow(tmp_d)
}

ds2 <- ds[colnames(ds) %in% names(tmp[tmp < min(which(t_row==186))])]   # including all the cultures
ds2 <- ds2[complete.cases(ds2),]
ds2 <- ds2 %>% left_join(data.frame(sccs_id=socdf$id,
                             Latitude=abs(socdf$Lat)), by='sccs_id')

# PCA ---------------------------------------------------------------------

fpca <- prcomp(ds2[-1], scale.=TRUE)
# plot(fpca)

SCCS_pcaloads1 <- 
  pca_loadings_plot(fpca, components = 1) + 
  theme_bw(11) +
  labs(x='', y='') +
  scale_colour_gradient2(low=viridis::magma(11)[4], mid='white', high=viridis::magma(11)[8])
SCCS_pcaloads2 <- 
  pca_loadings_plot(fpca, components = 2) + 
  theme_bw(11) +
  labs(y='') +
  scale_colour_gradient2(low=viridis::magma(11)[4], mid='white', high=viridis::magma(11)[8])

# Join by culture codes ---------------------------------------------------

culture_codes <- read.delim("sccs/Culture codes.txt", stringsAsFactors=FALSE)
culture_codes <- culture_codes[complete.cases(culture_codes),]
culture_codes$SCCS <- paste0('SCCS', as.character(culture_codes$SCCS))
colnames(culture_codes) <- c('culture_id', 'sccs_id')
sccs_key <- as_tibble(data.frame(culture=socdf$pref_name_for_society,
                                 sccs_id=socdf$id,
                                 stringsAsFactors=FALSE))

culture2 <- culture_df[culture_df$culture_id %in% df$culture_id,]
culture2 <- culture2 %>% 
  left_join(culture_codes, by='culture_id') %>% 
  left_join(data.frame(sccs_id=ds2$sccs_id,
                       pc1=fpca$x[,1], 
                       pc2=fpca$x[,2]), by='sccs_id')

c_df2 <- culture2 %>% left_join(df, by='culture_id')

sm1 <- glmer(supernatural ~ pc1 + pc2 + (1|culture_id), data=c_df2, family='binomial')
sm2 <- glmer(supernatural ~ scale(pc1) + scale(pathogen_stress) + scale(latitude) + (1|culture_id), data=c_df2, family='binomial')
# Exploratory glmers for the supplementary
# pathogen, latitude, cultural complexity, pc1, pc2
sm3 <- glmer(supernatural ~ scale(pathogen_stress) + (1|culture_id), data=c_df2, family='binomial')
sm4 <- glmer(supernatural ~ scale(pathogen_stress) + scale(latitude) + (1|culture_id), data=c_df2, family='binomial')
sm5 <- glmer(supernatural ~ scale(pathogen_stress) + scale(latitude) + scale(pc1) +  (1|culture_id), data=c_df2, family='binomial')

culture3 <- df %>% 
  group_by(culture_id) %>% 
  summarise(nobs=length(supernatural),
            nauthors=length(unique(author_id)),
            supernatural=mean(supernatural)) %>% 
  left_join(culture2, by='culture_id')

culture3$supernat_binary <- (culture3$supernatural>mean(culture3$supernatural))

culture3$pc1[culture3$culture=='Wolof'] <- culture3$pc1[culture3$culture=='Hausa'] - 0.15  
# this^ "manual jitter" avoids overlapping points with Hausa on the plot, and is purely aesthetic

set.seed(20212021)
SCCS_biplot <- 
  ggplot(culture3, aes(x=pc1, y=pc2, 
                     label=culture)) +
  geom_jitter(aes(fill=supernat_binary, size=nauthors), pch=21, colour='black', 
              alpha=0.5, width=0.07, height=0.07) +
  geom_text_repel()  +
  theme_bw() +
  scale_fill_binary() +
  labs(x='\nculture scale / complexity (PC1)', y='pathogen stress / proximity to equator (PC2)\n',
       fill='>average\nproportion\nsupernatural', size='# ethnographers') +
  scale_size(range=c(3,8), breaks=c(1,2,4,8)) +
  geom_vline(xintercept=0, alpha=0.4, linetype=3) +
  geom_hline(yintercept=0, alpha=0.4, linetype=3) +
  guides(fill=guide_legend(order=1, override.aes=list(size=4)),
         size=guide_legend(order=2))


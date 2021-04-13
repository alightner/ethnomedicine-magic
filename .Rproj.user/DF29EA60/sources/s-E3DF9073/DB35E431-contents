d_field <- read_csv('field-data/relsciTrust.csv')

# PCA laibon health -------------------------------------------------------

pca <- prcomp(d_field[-which(colnames(d_field)=='Christian')], scale.=TRUE)
# plot(pca)
field_LaibonPCA <- pca_loadings_plot(pca)

d_field$Christian2 <- ifelse(d_field$Christian==1,'Christian','Traditional')
field_Laibon_biplot <- 
  autoplot(pca, label=FALSE, size=2,
         alpha=0.4,
         data=d_field,
         loadings=TRUE, loadings.label=TRUE, loadings.label.repel=TRUE,
         frame=TRUE, frame.type='norm', frame.colour='Christian2'
) +
  theme_bw(base_size=10) +
  labs(colour='', fill='')

field_Laibon_biplot <- 
  autoplot(pca, label=FALSE, size=2,
           alpha=0.5,
           colour='Christian2',
           data=d_field,
           loadings=TRUE, loadings.label=TRUE, loadings.label.repel=TRUE
  ) +
    stat_ellipse(aes(colour=Christian2)) +
    scale_colour_binary() +
  theme_bw(base_size=10) +
  labs(colour='', fill='') +
    geom_count(aes(colour=Christian2)) + 
    labs(size='number observations') +
    theme(legend.position ='top')

field_Laibon_biplot$layers[[2]]$aes_params$colour <- '#666666'
field_Laibon_biplot$layers[[3]]$aes_params$colour <- '#666666'

# Single dimension (PCA) --------------------------------------------------

d_field$pc1 <- pca$x[,1]
pcaLaibon1 <- 
  ggplot(d_field, aes(x=pc1, y=Christian2, colour=Christian2, fill=Christian2)) +
    stat_dots() +
    geom_vline(xintercept=0, alpha=0.7, linetype=3) +
  scale_colour_binary() +
  scale_fill_binary() +
  theme_classic(base_size=10) +
  labs(colour='', fill='', x='PC1', y='')

pcaLaibon2 <- 
  pca_loadings_plot(pca, components=1) +
  coord_flip() +
    theme_bw(11) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_colour_gradient2(low=viridis::magma(11)[4], mid='white', high=viridis::magma(11)[8])

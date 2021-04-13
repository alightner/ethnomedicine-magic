load('field-data/medExplain.rda')
codes <- md$code1
codes <- unlist(str_split(codes, pattern=', '))
md2 <- data.frame(matrix(0, nrow=nrow(md), ncol=length(names(table(codes)))))
colnames(md2) <- names(table(codes))
for(i in colnames(md2)) {
  md2[i] <- as.numeric(str_detect(md$code1, pattern=i))
}
md2 <- rename(md2, `don't know`=dk)
md3 <- md2 %>% dplyr::select(-outcome)
md0 <- md3
md0$row_s <- rowSums(md0)
ann_df <- md0 %>% 
  mutate(
    id=1:nrow(md0),
    type=case_when(
      `don't know` | prayer & row_s<=2 ~ "don't know",
      condition & row_s <= 3 ~ "how-to explanations",
      TRUE ~ "mechanistic explanations"
    )) %>% 
  dplyr::select(id, type)
rownames(md3) <- as.numeric(1:nrow(md3))
ann_df$id <- as.character(ann_df$id)

fieldPCA_CausalHeatmap <- 
  hagenheat5(t(md3), seriation_method='PCA_angle', ann_col = ann_df) +
  theme(axis.text.x = element_blank(), legend.position = 'top')

# PCA ---------------------------------------------------------------------

pca2 <- prcomp(md3)
field_PCA_loadings <- pca_loadings_plot(pca2, components=c(1,2)) +
  labs(y='')
field_PCA_biplot <- autoplot(pca2, label=FALSE, size=3,
         alpha=0.4,
         data=md3,
         loadings=TRUE, loadings.label=TRUE) +
  theme_bw(base_size=10)

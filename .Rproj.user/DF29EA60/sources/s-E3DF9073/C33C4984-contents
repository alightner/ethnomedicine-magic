dm <- dm[dm$textid %in% df$textid,]
d_edges <- data.frame()
for(i in unique(dm$textid)){
  tmp <- dm[dm$textid==i,]
  if(nrow(tmp) >1){
    m <- t(combn(nrow(tmp),2))
    d_edges <- rbind(
      d_edges,
      data.frame(matrix(as.character(tmp$domain)[m], ncol=2))
    )
  }
}
g <- graph_from_edgelist(as.matrix(d_edges))
E(g)$weight <- 1
V(g)$freq <- table(dm$domain)[names(V(g))]
g <- igraph::simplify(g, edge.attr.comb=list(weight='sum'))
g2 <- induced_subgraph(g, vids=V(g)[V(g)$freq>=5])

# iterate through each domain > threshold
cds <- sort(table(dm$domain))
cds <- names(cds)[cds>=15]
pr_supernat <- rep(NA, length(cds))
ind = 1
for(i in cds) {
  pr_supernat[ind] <- mean(df$supernatural[df$textid %in% dm$textid[dm$domain==i]])
  ind <- ind+1
}
d_supernat <- data.frame(domains=cds, pr_supernat)
g3 <- induced_subgraph(g2, v=V(g2)[names(V(g2)) %in% cds]) %>% 
  set_vertex_attr('supernatural', index=d_supernat$domains, value=d_supernat$pr_supernat)

eHRAF_domain_supernatural <- 
  ggraph(g3, 'nicely') +
  geom_edge_link0(aes(edge_width=weight/max(E(g)$weight)), 
                  edge_colour="lightgray", alpha=0.4) +
  geom_node_point(aes(size=freq, fill=supernatural), alpha=0.6, colour='black', shape=21) +
  scale_fill_viridis_c() +
  geom_node_text(aes(label=name), repel=TRUE) +
  scale_edge_width(range = c(0.05,4))+
  theme_graph(base_family='Helvetica') +
    scale_size(range=c(4,9)) +
  labs(x='', y='', size='# obs.', edge_width='weights', fill='proportion\nsupernatural\n')




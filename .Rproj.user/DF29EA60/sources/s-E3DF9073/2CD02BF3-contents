# Minimum spanning tree ---------------------------------------------------

m <- as.matrix(dist(t(d), method='binary'))

gm <- graph_from_adjacency_matrix(m, mode = 'undirected', weighted = T, diag = F)
gm <- igraph::mst(gm, algorithm = 'prim')

V(gm)$support <- colSums(d)

set.seed(69420)
eHRAF_mst <- ggraph(gm, 'fr') + 
  geom_node_point(aes(size=support), alpha=0.3) +  
  geom_edge_link(alpha=0.3) +
  geom_node_text(aes(label=name), repel=TRUE, size=3) +
  theme_graph(base_size=12) +
  # theme_bw() +   # use this to adjust annotation positions if the layout changes
  labs(size='Evidence') +
  annotate('text', x=16.5, y=15, label='Prestigious teachers', size=4, fontface=2) +
  annotate('text', x=-10, y=5.5, label='Efficacious healers', size=4, fontface=2) +
  annotate('text', x=17, y=-4, label='Feared diviners', size=4, fontface=2) +
  ggtitle('A taxonomy of ethnomedical specialists')

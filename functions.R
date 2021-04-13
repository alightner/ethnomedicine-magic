require(broom.mixed)
require(glue)
require(arm)
require(lme4)
# function script for project ---------------------------------------------

domain_add <- function(x, df, domain_data)
{
  xout <- rep(0, nrow(df))
  xout[df$textid %in% domain_data$textid[domain_data$domain==x]] <- 1
  return(xout)
}

anti_col <- function(x)
{
  anti_x <- rep(0, length(x))
  anti_x[x==-1] <- 1
  return(anti_x)
}
rm_negs <- function(x)
{
  x[x==-1] <- 0
  return(x)
}

stripMultiples <- function(x)
{
  y <- NULL
  for(i in 1:length(x)){
    tmp <- unlist(strsplit(x[i], ', '))
    y <- c(y,tmp)
  }
  return(y)
}

# thedata should only include vars of interest
textrecord_support <- function(thedata, formula_string, nonNum=2)
{
  
  thevars <- names(thedata[-c((length(thedata)-(nonNum-1)):length(thedata))])
  
  formulae <- glue_data(list(outcome = thevars), formula_string)
  
  models <-
    purrr::map(
      formulae,
      ~ glmer(
        as.formula(.x),
        family = binomial,
        data = thedata,
        nAGQ = 0
      )
    )
  
  tibble(
    Level = "text records",
    vars = thevars,
    #Variable = names(thevars),    # previously thevars
    Model = models,
    Tidy = purrr::map(Model, broom.mixed::tidy, conf.int = T),
    Estimate = purrr::map_dbl(Tidy, ~ invlogit(.x$estimate[1])),
    lowerCI = purrr::map_dbl(Tidy, ~ invlogit(.x$conf.low[1])),
    upperCI = purrr::map_dbl(Tidy, ~ invlogit(.x$conf.high[1])),
    authorSD = purrr::map_dbl(Tidy, ~ .x$estimate[2]), # Very fragile
    cultureSD = purrr::map_dbl(Tidy, ~ .x$estimate[3]) # Very fragile
  )
} 


adjCultureScore <- function(varslist, modname)
{
  pop_mod <- scoreModels_culture(test_vars=varslist, df, culture_key)
  pop_mod <- pop_mod[complete.cases(pop_mod),]
  pop_mod2 <- df %>% 
    group_by(culture_id) %>% 
    summarise(num_txt=length(textid)) %>% 
    left_join(pop_mod, by='culture_id') %>% 
    mutate(adj_score=`for`/num_txt)
  adjScores <- pop_mod2$adj_score/length(varslist)
  names(adjScores) <- pop_mod2$culture_id
  adjdf <- tibble(culture_id=names(adjScores),
                  adjMod=adjScores)
  colnames(adjdf) <- c('culture_id', modname)
  #return(pop_mod2$adj_score)
  return(adjdf)
}

# csm1_pop_mod <- scoreModels_culture(test_vars=csm1_vars, df, culture_key)
# csm1_pop_mod <- csm1_pop_mod[complete.cases(csm1_pop_mod),]
# csm1_pop_mod2 <- df %>% 
#   group_by(culture_id) %>% 
#   summarise(num_txt=length(textid)) %>% 
#   left_join(csm1_pop_mod, by='culture_id') %>% 
#   mutate(adj_score=`for`/num_txt)

qdf <- function(x)
{
  x0 <- rep(0,length(x))
  if(x[1]>0) x0[1:x[1]] <- 1
  return(x0)
}


modScore <- function(model, mod_type, df_evidence)
{
  est <- df_evidence$estimate[df_evidence$mod_id==model & df_evidence$type==mod_type]
  #se <- (df_evidence$upperCI[df_evidence$mod_id==model & df_evidence$type==mod_type] - df_evidence$lowerCI[df_evidence$mod_id==model & df_evidence$type==mod_type])/4
  se <- sd(est)
  return(list(mean(est), se))
}


resample <- function(dat, cluster, replace) {
  
  # exit early for trivial data
  if(nrow(dat) == 1 || all(replace==FALSE))
    return(dat)
  
  # sample the clustering factor
  cls <- sample(unique(dat[[cluster[1]]]), replace=replace[1])
  
  # subset on the sampled clustering factors
  sub <- lapply(cls, function(b) subset(dat, dat[[cluster[1]]]==b))
  
  # sample lower levels of hierarchy (if any)
  if(length(cluster) > 1)
    sub <- lapply(sub, resample, cluster=cluster[-1], replace=replace[-1])
  
  # join and return samples
  do.call(rbind, sub)
  
}


logit.inv <- function(x) {
  return(exp(x)/(1+exp(x)))
}


cultureEffects <- function(m, fixedEffect)   # returnAIC=FALSE
{
  model <- m[[1]]
  model_vars <- m[[2]]
  w <- rep(length(model_vars), nrow(c_df))
  
  if(sum(as.matrix(df[,model_vars])) !=0){
    m2 <- glmer(rowSums(c_df[,model_vars])/w ~ eval(parse(text=fixedEffect))+(1|culture_id), family=binomial, weights=w, data=c_df)
    estimates <- logit.inv(fixef(m2))
    ci <- logit.inv(confint(m2, method='Wald'))
    lowerCI <- ci[2:nrow(ci),1]
    upperCI <- ci[2:nrow(ci),2]
    
  } else {
    
    estimate <- 0
    lowerCI <- 0
    upperCI <- 0
    
  }
  model_d <- data.frame(estimates, lowerCI, upperCI, mod_id=model)
  row.names(model_d) <- 
    stringr::str_remove_all(as.character(row.names(model_d)), pattern=fixed("eval(parse(text = fixedEffect))"))
  model_d$vars <- row.names(model_d)
  
  # if(returnAIC==TRUE) model_d$AIC <- AIC()
  
  return(model_d)
}


hagenheat2 <- function (d, method = "seriate", seriation_method = NULL, independent = F, 
                        hc_method = "ward.D", dist_method = "euclidean", scale. = "none", 
                        viridis_option = "D", wrap_labels = T, rotate_labels = F, 
                        ann_col = NULL) 
{
  if (inherits(d, "dist")) {
    rwnms <- labels(d)
    if (is.null(seriation_method)) 
      seriation_method <- "Spectral"
    o <- seriation::seriate(d, method = seriation_method)
    row_order <- seriation::get_order(o)
    col_order <- row_order
    d <- as.matrix(d)
    if (is.null(rwnms)) {
      warning("Lack of labels on dist object might make plot difficult to interpret")
      rwnms <- as.character(1:nrow(d))
    }
  }
  else {
    rwnms <- character(0)
    if (inherits(d, "data.frame")) {
      if (is.character(d[[1]]) | is.factor(d[[1]])) {
        rwnms <- d[[1]]
        d <- d[-1]
      }
      d <- as.matrix(d)
    }
    if (mode(d) == "logical") 
      d <- d * 1
    if (mode(d) != "numeric") 
      stop("d must be convertible to a numeric matrix")
    if (length(di <- dim(d)) != 2) 
      stop("'d' must have 2 dimensions")
    nr <- di[1L]
    nc <- di[2L]
    if (nr <= 1 || nc <= 1) 
      stop("'d' must have at least 2 rows and 2 columns")
    if (length(rwnms) == 0) {
      if (length(rownames(d)) > 1) {
        rwnms <- rownames(d)
      }
      else {
        rwnms <- as.character(1:nrow(d))
      }
    }
    if (method == "seriate") {
      if (is.null(seriation_method)) 
        seriation_method <- "PCA"
      if (independent) {
        o <- seriation::seriate(dist(d, method = dist_method), 
                                method = seriation_method)
        row_order <- seriation::get_order(o)
        o <- seriation::seriate(dist(t(d), method = dist_method), 
                                method = seriation_method)
        col_order <- seriation::get_order(o)
      }
      else {
        o <- seriation::seriate(d, method = seriation_method)
        row_order <- seriation::get_order(o, dim = 1)
        col_order <- seriation::get_order(o, dim = 2)
      }
    }
    else if (method == "hclust") {
      hclustrows <- hclust(dist(d, method = dist_method), 
                           method = hc_method)
      row_order <- hclustrows$order
      hclustcols <- hclust(dist(t(d), method = dist_method), 
                           method = hc_method)
      col_order <- hclustcols$order
    }
    else {
      stop("method must be 'seriate' or 'hclust'")
    }
  }
  if (scale. == "row") {
    d <- as_tibble(t(scale(t(d))))
  }
  else if (scale. == "column") {
    d <- as_tibble(scale(d))
  }
  else {
    d <- as_tibble(d)
  }
  rwnms <- factor(rwnms, levels = rwnms[row_order])
  d <- dplyr::bind_cols(rowname = rwnms, d)
  p <- d %>% tidyr::gather(key = key, value = value, -1) %>% 
    mutate(key = factor(key, levels = colnames(d[-1])[col_order]), 
    ) %>% ggplot(aes_string("key", "rowname", fill = "value", colour="value")) + 
    geom_raster() + scale_fill_viridis_c(option = viridis_option) + 
    labs(x = "", y = "") + theme_minimal()
  if (wrap_labels) 
    p <- p + scale_x_discrete(labels = scales::label_wrap(10))
  if (rotate_labels) 
    p <- p + theme(axis.text.x = element_text(angle = 90, 
                                              hjust = 1, vjust = 0.5))
  if (!is.null(ann_col)) {
    nms <- names(ann_col)
    ann_col$value = 1
    h <- nrow(d)/20
    p <- p + ggnewscale::new_scale_fill() + geom_tile(data = ann_col, 
                                                      aes_string(x = nms[1], y = nrow(d) + h, fill = nms[2]), 
                                                      height = h, width = 1) + coord_cartesian(clip = "off") + 
      theme(plot.margin = unit(c(2, 1, 1, 1), "lines"))
  }
  return(p)
}

scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels=function(x) gsub(reg, "", x), ...)
}



hagenheat4 <- function (d, method = "seriate", seriation_method = NULL, independent = F, 
                        hc_method = "ward.D", dist_method = "euclidean", scale. = "none", 
                        viridis_option = "D", wrap_labels = T, rotate_labels = F, 
                        ann_col = NULL) 
{
  if (inherits(d, "dist")) {
    rwnms <- labels(d)
    if (is.null(seriation_method)) 
      seriation_method <- "Spectral"
    o <- seriation::seriate(d, method = seriation_method)
    row_order <- seriation::get_order(o)
    col_order <- row_order
    d <- as.matrix(d)
    if (is.null(rwnms)) {
      warning("Lack of labels on dist object might make plot difficult to interpret")
      rwnms <- as.character(1:nrow(d))
    }
  }
  else {
    rwnms <- character(0)
    if (inherits(d, "data.frame")) {
      if (is.character(d[[1]]) | is.factor(d[[1]])) {
        rwnms <- d[[1]]
        d <- d[-1]
      }
      d <- as.matrix(d)
    }
    if (mode(d) == "logical") 
      d <- d * 1
    if (mode(d) != "numeric") 
      stop("d must be convertible to a numeric matrix")
    if (length(di <- dim(d)) != 2) 
      stop("'d' must have 2 dimensions")
    nr <- di[1L]
    nc <- di[2L]
    if (nr <= 1 || nc <= 1) 
      stop("'d' must have at least 2 rows and 2 columns")
    if (length(rwnms) == 0) {
      if (length(rownames(d)) > 1) {
        rwnms <- rownames(d)
      }
      else {
        rwnms <- as.character(1:nrow(d))
      }
    }
    if (method == "seriate") {
      if (is.null(seriation_method)) 
        seriation_method <- "PCA"
      if (independent) {
        o <- seriation::seriate(dist(d, method = dist_method), 
                                method = seriation_method)
        row_order <- seriation::get_order(o)
        o <- seriation::seriate(dist(t(d), method = dist_method), 
                                method = seriation_method)
        col_order <- seriation::get_order(o)
      }
      else {
        o <- seriation::seriate(d, method = seriation_method)
        row_order <- seriation::get_order(o, dim = 1)
        col_order <- seriation::get_order(o, dim = 2)
      }
    }
    else if (method == "hclust") {
      hclustrows <- hclust(dist(d, method = dist_method), 
                           method = hc_method)
      row_order <- hclustrows$order
      hclustcols <- hclust(dist(t(d), method = dist_method), 
                           method = hc_method)
      col_order <- hclustcols$order
    }
    else {
      stop("method must be 'seriate' or 'hclust'")
    }
  }
  if (scale. == "row") {
    d <- as_tibble(t(scale(t(d))))
  }
  else if (scale. == "column") {
    d <- as_tibble(scale(d))
  }
  else {
    d <- as_tibble(d)
  }
  rwnms <- factor(rwnms, levels = rwnms[row_order])
  d <- dplyr::bind_cols(rowname = rwnms, d)
  p <- d %>% tidyr::gather(key = key, value = value, -1) %>% 
    mutate(key = factor(key, levels = colnames(d[-1])[col_order]), 
    ) %>% ggplot(aes_string("key", "rowname", fill = "value", colour="value")) + 
    geom_raster() + scale_fill_viridis_c(option = viridis_option) + 
    labs(x = "", y = "") + theme_minimal()
  if (wrap_labels) 
    p <- p + scale_x_discrete(labels = scales::label_wrap(10))
  if (rotate_labels) 
    p <- p + theme(axis.text.x = element_text(angle = 90, 
                                              hjust = 1, vjust = 0.5))
  if (!is.null(ann_col)) {
    nms <- names(ann_col)
    ann_col$value = 1
    h <- nrow(d)/20
    p <- p + ggnewscale::new_scale_fill() + 
      geom_raster(data = ann_col, 
                  aes_string(x = nms[1], y = nrow(d) + h, fill = nms[2]), 
                  height = h, width = 1) + coord_cartesian(clip = "off") + 
      theme(plot.margin = unit(c(2, 1, 1, 1), "lines"))
  }
  return(p)
}


hagenheat3 <- function (d, method = "seriate", seriation_method = NULL, independent = F, 
                        hc_method = "ward.D", dist_method = "euclidean", scale. = "none", 
                        viridis_option = "D", wrap_labels = T, rotate_labels = F, 
                        ann_col = NULL) 
{
  if (inherits(d, "dist")) {
    rwnms <- labels(d)
    if (is.null(seriation_method)) 
      seriation_method <- "Spectral"
    o <- seriation::seriate(d, method = seriation_method)
    row_order <- seriation::get_order(o)
    col_order <- row_order
    d <- as.matrix(d)
    if (is.null(rwnms)) {
      warning("Lack of labels on dist object might make plot difficult to interpret")
      rwnms <- as.character(1:nrow(d))
    }
  }
  else {
    rwnms <- character(0)
    if (inherits(d, "data.frame")) {
      if (is.character(d[[1]]) | is.factor(d[[1]])) {
        rwnms <- d[[1]]
        d <- d[-1]
      }
      d <- as.matrix(d)
    }
    if (mode(d) == "logical") 
      d <- d * 1
    if (mode(d) != "numeric") 
      stop("d must be convertible to a numeric matrix")
    if (length(di <- dim(d)) != 2) 
      stop("'d' must have 2 dimensions")
    nr <- di[1L]
    nc <- di[2L]
    if (nr <= 1 || nc <= 1) 
      stop("'d' must have at least 2 rows and 2 columns")
    if (length(rwnms) == 0) {
      if (length(rownames(d)) > 1) {
        rwnms <- rownames(d)
      }
      else {
        rwnms <- as.character(1:nrow(d))
      }
    }
    if (method == "seriate") {
      if (is.null(seriation_method)) 
        seriation_method <- "PCA"
      if (independent) {
        o <- seriation::seriate(dist(d, method = dist_method), 
                                method = seriation_method)
        row_order <- seriation::get_order(o)
        o <- seriation::seriate(dist(t(d), method = dist_method), 
                                method = seriation_method)
        col_order <- seriation::get_order(o)
      }
      else {
        o <- seriation::seriate(d, method = seriation_method)
        row_order <- seriation::get_order(o, dim = 1)
        col_order <- seriation::get_order(o, dim = 2)
      }
    }
    else if (method == "hclust") {
      hclustrows <- hclust(dist(d, method = dist_method), 
                           method = hc_method)
      row_order <- hclustrows$order
      hclustcols <- hclust(dist(t(d), method = dist_method), 
                           method = hc_method)
      col_order <- hclustcols$order
    }
    else {
      stop("method must be 'seriate' or 'hclust'")
    }
  }
  if (scale. == "row") {
    d <- as_tibble(t(scale(t(d))))
  }
  else if (scale. == "column") {
    d <- as_tibble(scale(d))
  }
  else {
    d <- as_tibble(d)
  }
  rwnms <- factor(rwnms, levels = rwnms[row_order])
  d <- dplyr::bind_cols(rowname = rwnms, d)
  p <- d %>% tidyr::gather(key = key, value = value, -1) %>% 
    mutate(key = factor(key, levels = colnames(d[-1])[col_order]), 
    ) %>% ggplot(aes_string("key", "rowname", fill = "value")) + 
    geom_tile(colour='gray') + scale_fill_viridis_c(option = viridis_option) + 
    labs(x = "", y = "") + theme_minimal()
  if (wrap_labels) 
    p <- p + scale_x_discrete(labels = scales::label_wrap(10))
  if (rotate_labels) 
    p <- p + theme(axis.text.x = element_text(angle = 90, 
                                              hjust = 1, vjust = 0.5))
  if (!is.null(ann_col)) {
    nms <- names(ann_col)
    ann_col$value = 1
    h <- nrow(d)/20
    p <- p + ggnewscale::new_scale_fill() + geom_tile(data = ann_col, 
                                                      aes_string(x = nms[1], y = nrow(d) + h, fill = nms[2]), colour='white',
                                                      height = h) + coord_cartesian(clip = "off") + 
      theme(plot.margin = unit(c(2, 1, 1, 1), "lines"))
  }
  return(p)
}

hagenheat5 <- function (d, method = "seriate", seriation_method = NULL, independent = F, 
                        hc_method = "ward.D", dist_method = "euclidean", scale. = "none", 
                        viridis_option = "D", wrap_labels = T, rotate_labels = F, 
                        ann_col = NULL) 
{
  if (inherits(d, "dist")) {
    rwnms <- labels(d)
    if (is.null(seriation_method)) 
      seriation_method <- "Spectral"
    o <- seriation::seriate(d, method = seriation_method)
    row_order <- seriation::get_order(o)
    col_order <- row_order
    d <- as.matrix(d)
    if (is.null(rwnms)) {
      warning("Lack of labels on dist object might make plot difficult to interpret")
      rwnms <- as.character(1:nrow(d))
    }
  }
  else {
    rwnms <- character(0)
    if (inherits(d, "data.frame")) {
      if (is.character(d[[1]]) | is.factor(d[[1]])) {
        rwnms <- d[[1]]
        d <- d[-1]
      }
      d <- as.matrix(d)
    }
    if (mode(d) == "logical") 
      d <- d * 1
    if (mode(d) != "numeric") 
      stop("d must be convertible to a numeric matrix")
    if (length(di <- dim(d)) != 2) 
      stop("'d' must have 2 dimensions")
    nr <- di[1L]
    nc <- di[2L]
    if (nr <= 1 || nc <= 1) 
      stop("'d' must have at least 2 rows and 2 columns")
    if (length(rwnms) == 0) {
      if (length(rownames(d)) > 1) {
        rwnms <- rownames(d)
      }
      else {
        rwnms <- as.character(1:nrow(d))
      }
    }
    if (method == "seriate") {
      if (is.null(seriation_method)) 
        seriation_method <- "PCA"
      if (independent) {
        o <- seriation::seriate(dist(d, method = dist_method), 
                                method = seriation_method)
        row_order <- seriation::get_order(o)
        o <- seriation::seriate(dist(t(d), method = dist_method), 
                                method = seriation_method)
        col_order <- seriation::get_order(o)
      }
      else {
        o <- seriation::seriate(d, method = seriation_method)
        row_order <- seriation::get_order(o, dim = 1)
        col_order <- seriation::get_order(o, dim = 2)
      }
    }
    else if (method == "hclust") {
      hclustrows <- hclust(dist(d, method = dist_method), 
                           method = hc_method)
      row_order <- hclustrows$order
      hclustcols <- hclust(dist(t(d), method = dist_method), 
                           method = hc_method)
      col_order <- hclustcols$order
    }
    else {
      stop("method must be 'seriate' or 'hclust'")
    }
  }
  if (scale. == "row") {
    d <- as_tibble(t(scale(t(d))))
  }
  else if (scale. == "column") {
    d <- as_tibble(scale(d))
  }
  else {
    d <- as_tibble(d)
  }
  rwnms <- factor(rwnms, levels = rwnms[row_order])
  d <- dplyr::bind_cols(rowname = rwnms, d)
  p <- d %>% tidyr::gather(key = key, value = value, -1) %>% 
    mutate(key = factor(key, levels = colnames(d[-1])[col_order]), 
    ) %>% 
    ggplot(aes_string("key", "rowname", fill = "value")) + 
    geom_raster(show.legend = FALSE) + 
    scale_fill_viridis_c(option = viridis_option) + 
    #theme(axis.text.x = element_blank(), legend.position = 'none') +
    # guides(fill=FALSE) +
    labs(x = "", y = "") + theme_minimal()
  if (wrap_labels) 
    p <- p + scale_x_discrete(labels = scales::label_wrap(10))
  if (rotate_labels) 
    p <- p + theme(axis.text.x = element_text(angle = 90, 
                                              hjust = 1, vjust = 0.5))
  if (!is.null(ann_col)) {
    nms <- names(ann_col)
    ann_col$value = 1
    h <- nrow(d)/20
    anncolour <- c('#000066', '#3333FF', '#0ab8f2')
    p <- p +
      ggnewscale::new_scale_fill() +
      geom_tile(data = ann_col,
                aes_string(x = nms[1], y = nrow(d) + h + 0.25, 
                           colour = nms[2],
                           fill=nms[2]), 
                height=h) +
      scale_fill_manual(values=anncolour) +
      scale_colour_manual(values=anncolour) +
      guides(fill='none',
             colour=guide_legend(override.aes=list(fill=anncolour))) +
      #scale_colour_aaas() +
      coord_cartesian(clip = "off") +
      #theme(plot.margin = unit(c(1, 1, 1, 1), "lines")) +
      labs(colour='')
  }
  return(p)
}



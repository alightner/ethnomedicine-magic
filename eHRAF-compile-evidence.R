
# Variable scores by text record ------------------------------------------

formula_string = "{outcome} ~ 1 + (1|culture/author)"
d0 <- df[colnames(df) %in% inv_dict[colnames(d)]]
d0$culture <- df$culture_id
d0$author <- df$author_id
mod_record <- textrecord_support(d0, formula_string)

tm_support <- data.frame(
  vars=mod_record$vars,
  estimate=mod_record$Estimate,
  lowerCI=mod_record$lowerCI,
  upperCI=mod_record$upperCI,
  stringsAsFactors = FALSE
)

modlist1 <- data.table(rbindlist(
  list(data.frame(vars=cogVars, cat_id='Cognitive'),
       data.frame(vars=socialVars_specialist, cat_id='Specialist attributes')
)))

modlist2 <- data.frame(vars=socialVars_layperson, cat_id='Non-specialist interactions', incentive=NA)
modlist2$incentive[modlist2$vars %in% vars_rec_negative] <- 'Disincentives'
modlist2$incentive[modlist2$vars %in% vars_rec_positive] <- 'Incentives'

tm_specialist <- tm_support %>% left_join(modlist1, by='vars')
tm_layperson <- tm_support %>% left_join(modlist2, by='vars')

tm_specialist <- tm_specialist[complete.cases(tm_specialist),]
tm_layperson <- tm_layperson[complete.cases(tm_layperson),]


# Variable scores by culture ----------------------------------------------

d0 <- cbind(
  data.frame(
    culture_id=df$culture_id,
    author_id=df$author_id,
    stringsAsFactors=FALSE
  ),
  df[colnames(df) %in% inv_dict[colnames(d)]]
)

c_trials <- 1e3
c_mat <- matrix(NA, nrow=c_trials, ncol=ncol(d0[sapply(d0, is.numeric)]))

for(i in 1:c_trials){
  c_mat[i,] <- unlist(sapply(
    resample(d0, c('culture_id', 'author_id'), replace=c(TRUE,TRUE)),
    function(x) if(is.numeric(x)) mean(x)
  ))
}

c_mat2 <- as.data.frame(c_mat)
colnames(c_mat2) <- colnames(d0[sapply(d0, is.numeric)])

sdlist <- sapply(c_mat2, sd)

d0 <- df %>% 
  group_by(culture_id) %>% 
  summarise(across(where(is.numeric), sum)) 
d0[sapply(d0, is.numeric)] <- d0[sapply(d0, is.numeric)] > 0

c_ts <- tibble(
  vars=colnames(d0[colnames(d0) %in% inv_dict[colnames(d)]]),
  c_estimate=colMeans(d0[colnames(d0) %in% inv_dict[colnames(d)]]),
  c_sd=sdlist
)
c_ts$c_upperCI <- c_ts$c_estimate + 2*c_ts$c_sd
c_ts$c_lowerCI <- c_ts$c_estimate - 2*c_ts$c_sd
c_ts$c_upperCI[c_ts$c_upperCI>1]<-1
c_ts$c_lowerCI[c_ts$c_lowerCI<0]<-0


# Reshaping the dataframe -------------------------------------------------

tm_specialist <- tm_specialist %>% left_join(c_ts, by='vars')
tm_layperson <- tm_layperson %>% left_join(c_ts, by='vars')
tm_specialist$vars <- var_dict2[tm_specialist$vars]
tm_layperson$vars <- var_dict2[tm_layperson$vars]

## specialists
tm1 <- tm_specialist %>% 
  tibble() %>% 
  dplyr::select(vars:cat_id)
tm2 <- tm_specialist %>% 
  tibble() %>% 
  dplyr::select(
    vars,
    estimate=c_estimate,
    lowerCI=c_lowerCI,
    upperCI=c_upperCI,
    cat_id
  )
tm1$type <- 'text'
tm2$type <- 'culture'
specialist_evidence <- data.table::rbindlist(list(tm1,tm2))
specialist_evidence$estimate <- specialist_evidence$estimate*100
specialist_evidence$lowerCI <- specialist_evidence$lowerCI*100
specialist_evidence$upperCI <- specialist_evidence$upperCI*100 

## laypersons
tm1 <- tm_layperson %>% 
  tibble() %>% 
  dplyr::select(vars:incentive)
tm2 <- tm_layperson %>% 
  tibble() %>% 
  dplyr::select(
    vars,
    estimate=c_estimate,
    lowerCI=c_lowerCI,
    upperCI=c_upperCI,
    cat_id,
    incentive
  )
tm1$type <- 'text'
tm2$type <- 'culture'
layperson_evidence <- data.table::rbindlist(list(tm1,tm2))
layperson_evidence$estimate <- layperson_evidence$estimate*100
layperson_evidence$lowerCI <- layperson_evidence$lowerCI*100
layperson_evidence$upperCI <- layperson_evidence$upperCI*100

# Writing data file -------------------------------------------------------

write.table(layperson_evidence, file='eHRAF-data/layperson_evidence.csv', sep=',', row.names=FALSE)
write.table(specialist_evidence, file='eHRAF-data/specialist_evidence.csv', sep=',', row.names=FALSE)
save(layperson_evidence, file='eHRAF-data/layperson_evidence.rda')
save(specialist_evidence, file='eHRAF-data/specialist_evidence.rda')


# IRR for CH and AL codes -------------------------------------------------

ch_d <- read_csv('eHRAF-data/CH-coded-dataset.csv')
al_d <- read_csv('eHRAF-data/AL-coded-dataset.csv')

# check to make sure everything is lined up:
# table(al_d$textid==ch_d$textid)
# table(colnames(al_d)==colnames(ch_d))

ch_d$coder <- 'cynthia'
al_d$coder <- 'aaron'
ps <- data.table::rbindlist(list(al_d, ch_d))

a = c(); k = c(); v = c()
for (i in 2:(ncol(ps)-1)){
  nm = names(ps)[i]
  e.wide = dcast(ps, textid ~ coder, value.var=names(ps)[i])
  agr = agree(e.wide[,2:3])
  kpa = kappa2(e.wide[,2:3])
  a = c(a, agr$value)
  k = c(k, kpa$value)
  v = c(v, nm)
}

ps.irr=data.frame(variable=v, agreement=round(a, 2), kappa=round(k, 2))

al <- al_d[sapply(al_d, is.numeric)]
ch <- ch_d[sapply(ch_d, is.numeric)]

ch2 <- as.vector(as.matrix(ch))
al2 <- as.vector(as.matrix(al))
ratings = data.frame(al=al2, ch=ch2)

ac1 = gwet.ac1.table(table(ratings$al, ratings$ch))

kap_irr = kappa2(ratings)

ps.total.agree = agree(cbind(al2, ch2))
ps.total.kappa = kappa2(cbind(al2, ch2))

kappa = ps.total.kappa$value
props = prop.table(table(c(al2, ch2)))
K = 3 # number of categories

w = props[[1]]^2 + props[[2]]^2 + props[[3]]^2
s = kappa*(1-(w*K)) - K*(1-w)
a =  (s - sqrt(s*s - s*K*(kappa*(K-1)*(K-2) + (1-w)*(kappa-1))))/(s*K)
rater.accuracy = round(100*a, 1)




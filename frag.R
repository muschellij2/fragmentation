load("data/10hour/act50.rda")

cases = unique(act50$SEQN)

frag = data.frame(SEQN = cases,mu.r = NA, mu.a = NA, lambda.r = NA, lambda.a = NA)

for(i in 1: length(cases)){
  id.i = cases[i]
  
  act.i = subset(act50,SEQN == id.i)[,-c(1:2)]
  wear.i = subset(wear50, SEQN == id.i)[,-c(1:2)]
  r = NULL
  a = NULL
  
  x = na.omit(c(t(act.i)))
  w = na.omit(c(t(wear.i)))
  
  x = as.integer(x)
  w[which(w == 0)] = NA
  
  y = accel.bouts(counts = x, thresh.lower = 100, bout.length = 1)
  y = y * w
  mat = rle2(y)
  mat = mat[which(!is.na(mat[,1])),]
  rest = as.matrix(mat[mat[,1] == 0,])[,2]
  act = as.matrix(mat[mat[,1] == 1,])[,2]
  
  frag$mu.r[i] = mean(rest)
  frag$mu.a[i] = mean(act)
  
  frag$lambda.r[i] = 1/mean(rest)
  frag$lambda.a[i] = 1/mean(act)
 
}

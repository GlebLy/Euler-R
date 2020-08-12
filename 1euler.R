v <- 2:999
x <- c(v[v%%5==0],v[v%%3==0])

y<-x[! duplicated(x)]
sum(y)

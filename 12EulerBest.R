options(max.print = .Machine$integer.max)
Ndelitelej <- function(predel, l){
  count <- rep(0,length(l));
  for (i in 1:length(l)){
    while (predel%%l[i]==0){
      count[i] <- count[i] + 1;
      predel <- predel/l[i];
      #print(paste0("индикатор=", i, " ,Делимое=",predel, " ,Count=", count[i]));
    }
  }
  return(count);
}
euclidovoSitoDelitelej <- function(predel){
  if (predel<2){
    return(data.frame(prime = 1, Ndelitelej = 0));
  }else if (predel == 2) {
    return(data.frame(prime = 2, Ndelitelej = 1));
   } else {
    l <- 2:predel;
    i <- 1;
    while ( l[i] < sqrt(predel) ){
      l <- c(l[i] , l[!l%%l[i]==0]);
      i <- i + 1;
    }
    l <- l[predel%%l==0];
    df <- data.frame(prime = l, Ndelitelej = Ndelitelej(predel,l));
 }
 return(df);
}
countDiv <- function(n,m){
  dfn <- euclidovoSitoDelitelej(n);
  dfm <- euclidovoSitoDelitelej(m);
  df <- rbind(dfn, dfm);
  Ndelitelej <- aggregate(df$N, by=list(prime=df$p), FUN=sum);#Plusuem odinakowye osnownye chisla w data.frami
   return(prod(df$N+1));#chislo deliteley cherez formulu (a+1)(b+1)(c+1)... gde a,b,c,.. eto x^a*y^b*z^c*...
}
par <- function(n){
  if(n%%2==0)return(T);
  return(F);
}
kolDelitelej <- function(i){
  if(par(i)){
    n <- i/2;
    m <- i + 1;
  }else{
    m <- i;
    n <- (i + 1)/2;
  }
  countDiv(n, m);
}
triugolnoe <- function(i)i*(i+1)/2;
df <- data.frame(i=1:12500);
df$triugolnoe = triugolnoe(df$i);
df$kolDelitelej = sapply(df$i, kolDelitelej);
df$mnogoDelitelej = df$kolDelitelej >= 500;
subset(df, mnogoDelitelej == T);
subset(df, kolDelitelej > 400);
write.csv(df, file = "triugolnoeYkolDelitelBest.csv");
df

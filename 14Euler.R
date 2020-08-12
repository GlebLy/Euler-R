collatz.prime <- function(n){
  prime <- vector();
  j <- 1;
  while (n != 1){
    if (n %% 2 == 0){
      n <- n/2;
    }else{
      if (isPrime(n)){
        prime[j] <- n;
        j <- j + 1;
      }
      n <- 3*n + 1;
    }
   }
  return(prime);
}
collatz.chain <- function(n){
  i <- 1;
  m <- 0;
  j <- 1;
  while (n != 1){
    if (n %% 2 == 0){
      n <- n/2;
      m[i] <- n;
    }else{
      n <- 3*n + 1;
      m[i]  <- n;
    }
    i <- i + 1;
   }
  return(m);
}
isPrime <- function(x){
  if(x<2){
    return(FALSE);
  }else{
    flag <- 2;
    while (flag<x){
      if(x%%flag==0){
        return(FALSE);
        }
      flag <- flag + 1;
     }
     return(TRUE);
   }
}
euclidovoSito <- function(predel){
  if (predel<2){
    return(NULL);
  }else if (predel == 2) {
    l = 2;
   } else {
    l <- 2:predel;
    i <- 1;
    while ( l[i] < sqrt(predel) ){
      l <- c(l[i], l[!l%%l[i]==0]);
      i <- i + 1;
    }
  }
  return(l);
}
#Создаем вектор с основными числами, а потом дата.фраме с цепочкой основных чисел включенных в кахдое из основных чисел
euclidovoSitoCol <- function(predel){
  if (predel<2){
    return(data.frame(prime = 1, Nd = 0));
  }else if (predel == 2) {
    return(data.frame(prime = 2, Nd = 1));
   } else {
    l <- 2:predel;
    i <- 1;
    while ( l[i] < sqrt(predel) ){
      l <- c(l[i], l[!l%%l[i]==0]);
      i <- i + 1;
    }
    print(l);
    i <- 2;
    vectors <- list(collatz.prime(l[1]));
    dlina <- c(length(collatz.prime(l[1])));
    while (i <= length(l)){
      vectors <- c(vectors, list(collatz.prime(l[i])));
      dlina <- c(dlina, length(collatz.prime(l[i])));
      i <- i + 1;
     }
     df <-  as.data.frame(cbind(prime = l, dlina = dlina, chain = vectors));
   }
 return(df);
}
seqCollatz <- function(nachalo, predel){
  listPrime <- euclidovoSito(predel);
  listPrime <- listPrime[listPrime > nachalo];
  j <- length(listPrime);
  ms <- collatz.prime(listPrime[j]);
  df <- data.frame(prime = listPrime[j], dlina = length(ms));
  while (j > 1){
    if (!(listPrime[j] %in% ms )){
      ms = c(ms, collatz.prime(listPrime[j]));
      ps <- collatz.prime(listPrime[j]);
      print(ps);
      dfnew <- data.frame(prime = listPrime[j], dlina = length(ps));
      df <- rbind(df,dfnew);
    }
    j <- j - 1;
   }
 return(df);
}
maxSeqCollatz <- function(predel){
  df <- seqCollatz(1, predel);
  write.csv(df, file = "primeCollatz.csv");
  maxiDlina <- df[df$dlina >= max(df$dlina),];
  minLargeCollatzPrime <- min(maxiDlina$prime);
  col <- minLargeCollatzPrime;
  nowijPredel <- predel%/%minLargeCollatzPrime;
  while (nowijPredel > 1){
    dfnowij <- seqCollatz(1, nowijPredel);
    maxiDlina <- dfnowij[dfnowij$dlina >= max(dfnowij$dlina),];
    minLargeCollatzPrime <- min(maxiDlina$prime);
    col <- c(col, minLargeCollatzPrime);
    nowijPredel <- nowijPredel%/%minLargeCollatzPrime;
   }
   prodCollatz <- prod(col);
   collat <- collatz.chain(prodCollatz);
 return(collat);
}
#subset(df, dlina > 48)
#maxDlinaPrime = df[which.max(df$dlina),]$prime[[1]]
#maxPrime = max(collatz.prime(maxDlinaPrime))
#collatz.chain(maxPrime)
#Быдущая функция для решения проблемы
df <- seqCollatz(1, 1e3)
x <- read.csv("primeCollatz.csv");
m <- as.data.frame(x);
nachalo <- m[[2]][1];
df <- seqCollatz(nachalo, 1e6)
subset(df, dlina > 48)
maxDlinaPrime = df[which.max(df$dlina),]$prime[[1]]
maxPrime = max(collatz.prime(maxDlinaPrime))
collatz.chain(maxPrime)
#-----------------------------------------------------------------#
euclidovoSitoCol(maxPrime)
#Не работает завис на 3617
bigPrimecollatzPrimeDoMilliona <- function(){
  maxPrime <- 100;
  df <- 0;
  while (maxPrime < 1e6){
    df<-seqCollatz(maxPrime)
    maxDlinaPrime = df[which.max(df$dlina),]$prime;
    maxPrime = max(collatz.prime(maxDlinaPrime));
    print(maxPrime);
   }
 return(maxPrime);
}
df1e4 <- euclidovoSitoCol(1e4)
subset(df1e4, dlina > 20)
maxDlinaPrime = df1e4[which.max(df1e4$dlina),]$prime[[1]]
maxPrime = max(collatz.prime(maxDlinaPrime))
subset(df1e4, dlina > 30)

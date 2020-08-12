collatz.chain <- function(n){
  i <- 1;
  chain <- vector();
  while (n != 1){
    if (n %% 2 == 0){
      n <- n/2;
    }else{
      n <- 3*n + 1;
    }
    chain[i]  <- n;
    i <- i + 1;
   }
  return(chain);
}
collatz.chainLength <- function(n){
  i <- 1;
  chain <- vector();
  count <- 0;
  while (n != 1){
    if (n %% 2 == 0){
      n <- n/2;
      count <- count + 1;
    }else{
      n <- (3*n + 1)/2;
      count <- count + 2;
    }
    i <- i + 1;
   }
  return(count);
}
collatz.chainMax <- function(nachalo, predel){
  listNumber <- nachalo:predel;
  j <- length(listNumber);
  #ms <- collatz.chain(listNumber[j]);
  collatz.max <- collatz.chainLength(listNumber[j]);
  answer <- 0;
  while (j > 1){
      len <- collatz.chainLength(listNumber[j]);
      if (collatz.max <= len){
        answer <- listNumber[j];
        print(answer);
        collatz.max <- len;
      }
      j <- j - 1;
   }
 return(answer);
}
#maxCollatz <- collatz.chainMax(1,1e6)
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
seqCollatz <- function(nachalo, predel){
  listPrime <- euclidovoSito(predel);
  listPrime <- listPrime[listPrime > nachalo];
  j <- length(listPrime);
  ms <- collatz.prime(listPrime[j]);
  listPrimeClear <- listPrime[j];
  while (j >= 1){
    if (!(listPrime[j] %in% ms )){
      ms = c(ms, collatz.prime(listPrime[j]));
      listPrimeClear <- c(listPrimeClear,listPrime[j]);
    }else{
      print(paste0("Удалили: ",listPrime[j]));
    }
    j <- j - 1;
   }
 return(listPrimeClear);
}
collatz.prime <- function(n){
  if (n<3){
    return(NULL);
  }else{
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
}
edgelist <- data.frame(a = 2, b = 1)
x <- seqCollatz(1,1e2);
for (n in 1:length(x)) {
   print(n);
   chain <- as.character(c(x[n], collatz.chain(x[n])))
   chain <- data.frame(a = chain[-length(chain)], b = chain[-1])
   edgelist <- rbind(edgelist, chain)
}
library(igraph)
g <- graph.edgelist(as.matrix(edgelist))
g <- simplify(g)
par(mar=rep(0,4))
V(g)$color <- degree(g, mode = "out") + 1
plot(g,
     layout=layout.kamada.kawai,
     vertex.color=V(g)$color,
     vertex.size=6,
     vertex.label.cex=.7,
     vertex.label.color="black",
     edge.arrow.size=.1,
     edge.color="black"
     )

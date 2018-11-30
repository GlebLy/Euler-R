listPrime <- function(chislo){
  v <- 2:chislo;
  #z <- 2:chislo;
  k <- 1;
  while(k < chislo){
    i <- 1;
    while(i < k){
      #print(v[k]);
      #print(v[i]);
      if(v[k] %% v[i] == 0){
        v[k] <- v[k]/v[i];
      }
      i<-i+1;
    }
    k<-k+1;
    #print(k);
  }
  return(v)
}
prod(listPrime(10))
prod(listPrime(20))

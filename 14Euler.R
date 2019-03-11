collatz <- function(n){
  i <- 1;
  m <- 0;
  mp <- 0;
  j <- 1;
  while (n != 1){
    if (n %% 2 == 0){
      n <- n/2;
      m[i] <- n;
    }else{
      if (isPrime(n)){
        mp[j] <- n;
        j <- j + 1;
      }
      n <- 3*n + 1;
      m[i]  <- n;
    }
    i <- i + 1;
   }
  return(mp);
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
seqCollatz <- function(predel){
  df <- data.frame(prime = 1, dlina = 0);
  j <- 1;
  while (j < predel){
    if (isPrime(j)){
      ms <- collatz(j);
      print(ms);
      dfnew <- data.frame(prime = j, dlina = length(ms));
      df <- rbind(df,dfnew);
    }
    j <- j + 1;
   }

 return(df);
}
seqCollatz(100)

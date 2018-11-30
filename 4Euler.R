isPalindrom <- function(x){
  #as.character() или toString()
  c <- toString(x);
  i <- 1;
  while(i<=nchar(c)/2){
    if(substr(c, i, i) != substr(c, nchar(c) - i + 1,  nchar(c) - i + 1)){
      return(FALSE);
    }
    i <- i + 1;
  }
  return(TRUE);
}
bigPalindrom <- function(x){
  verxPredel <- 10^x - 1;
  nizhPredel <- 10^(x-1) - 1;
  i <- verxPredel;
  palin <- 0;
  while(i>nizhPredel){
    j <- verxPredel;
    if(i%%11==0 | j%%11==0){
      while(j>nizhPredel){
        if(isPalindrom(i*j) & palin < i*j){
          palin <- i*j;
        }
        j <- j - 1;
      }
    }
    i <- i - 1;
  }
  return(palin);
}

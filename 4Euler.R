isPalindrom <- function(x){

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

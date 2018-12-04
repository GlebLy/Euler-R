etoFactor <- function(x){
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

faktorN <- function(x){
  sum <- 0;
  i <- 0;
  while (sum<x){
    if (etoFactor(i)){
      sum <- sum + 1;
      #print(paste("Это фактор",i));
      #print(paste("Это суматор",sum));
    }
    if (sum == x){
      return(i);
    }
    i <- i + 1;
   }
   return(NULL);
}

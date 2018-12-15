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

summaFactorov <- function(predel){
  summa <- 0;
  #j <- 0;
  i <- 0;
  while (i<predel){
    if (etoFactor(i)){
      #print(i);
      #j <- j + 1;
      summa <- summa + i;
    }
    i <- i + 1;
   }
 return(summa);
}

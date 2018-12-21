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

euclidovoSito <- function(predel){
  if (predel<2){
    return(NULL);
  }else if (predel == 2) {
    return(predel);
   } else {
    l <- 2:predel;
    i <- 1;
    while ( l[i] < sqrt(predel) ){

        #print(i);
        l <- c(l[i] , l[!l%%l[i]==0]);
        #print(l);

      i <- i + 1;
     }
 }
 return(l);
}

sum(as.numeric(euclidovoSito(2e6)));

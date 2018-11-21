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

deliteliFactora <- function(chislo){
  x <- 1:chislo;
  z <- 1:chislo;
  factor <- etoFactor(z[which(13195 %% x == 0)]);
  return(factor);
  }

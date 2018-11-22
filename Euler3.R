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
	i <- 1;
	deliteli <- z[which(chislo %% x == 0)];
	
	etoFactora <- 1:length(deliteli);
	while( i <= length(deliteli)){
  	etoFactora[i] <- etoFactor(deliteli[i]);
		factor <- deliteli[which(etoFactora == 1)];
		
		i <- i + 1;
		
		}
  return(factor);
  }

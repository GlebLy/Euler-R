summaPar <- function(x){
vectorFib[1] = 1;
vectorFib[2] = 2;
prededuscij = 1;
sechasnyj = 2;
sleduyuscij = 3;
summa = 2;
i = 3;
while(sleduyuscij<=x){
  sleduyuscij = sechasnyj + prededuscij;
  #print(paste("Следующий", sleduyuscij));
  vectorFib[i] <- sleduyuscij;
  prededuscij = sechasnyj;
  if(sleduyuscij%%2==0){
    summa <- summa + sleduyuscij;
  }
  sechasnyj = sleduyuscij;
  i <- i + 1;
}
return(summa);
}

summaPar(4e6);

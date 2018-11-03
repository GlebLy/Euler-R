vectorFib = numeric(400000);
vectorFib[1] = 1;
vectorFib[2] = 2;
prededuscij = 1;
sechasnyj = 2;
sleduyuscij = 3;
summa = 2;
i = 3;
while(i<=10){
  sleduyuscij = sechasnyj + prededuscij;
  vectorFib[i] <- sleduyuscij;
  prededuscij = sechasnyj;
  if(sleduyuscij%%2==0){
    summa <- summa + sleduyuscij;
  }
  sechasnyj = sleduyuscij;
  i <- i+1;
}

vectorFib
summa

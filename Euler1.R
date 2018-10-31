y <- vector(mode = "character", length = 100);
y <- character(100);

  i<-1
  sum<-0
  while(i<100){
    if(i%%3==0 & i%%5==0) {
      y[i] <- "Fizz Buzz"
      sum <- sum+i
    }else if(i%%3==0){
      y[i] <- "Buzz"
      sum <- sum+i
    } else if(i%%5==0){
      y[i] <- "Fizz"
      sum <- sum+i
    } else {
      y[i] <- i
    }
    i<-i+1
  }
  sum
y

pytagor1000 <- function(){
  y <- numeric(3);
  for (i in 1:999){
    b <- 1;
    while (i+b<1000){
      #print(b);
      if (i^2 + b^2 == (1000-i-b)^2){
        y[1] <- i;
        y[2] <- b;
        y[3] <- 1000-i-b;
      }
      b <- b + 1;
     }
   }
 return(y);
}

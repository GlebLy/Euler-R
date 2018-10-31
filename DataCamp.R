#Fizz-Buzz, imperative style

y <- vector(mode = "character", length = 100)
y <- character(100)

for(i in 1:100){
  if(i%%15==0){
    y[i] <- "Fizz-Buzz"
  } else if(i%%3==0){
    y[i] <- "Fizz"
  } else if(i%%5==0){
    y[i] <- "Buzz"
  } else{
    y[i] <- i
  }
}
y

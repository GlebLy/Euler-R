raznostKvadratow <- function(x){
  m <- matrix(1:x,nrow = x,ncol = x);
  matrixX <- m*t(m);
  diagX <- diag(1:x)^2;
  return(sum(matrixX-diagX));
}

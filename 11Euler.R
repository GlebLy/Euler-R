x <- read.table("/home/gleb/Escritorio/matrix");
m <- as.matrix(x);

prod4PoStrokam <- function(m){
  prod4S <- matrix(0, 20, 17);
  razmernost <- dim(prod4S);
  j <- 1;
  while (j<=razmernost[1]){
    #print(prod4S);
    v <- m[j,];
    i <- 1;
    max<-prod(v[i:(i+3)]);
    vecMax <- v[i:(i+3)];
    while (i<=razmernost[2]){
      prod4S[j,i] <- prod(v[i:(i+3)]);
      if (max < prod(v[i:(i+3)])){
        max <- prod(v[i:(i+3)]);
        vecMax <- v[i:(i+3)];
      }
      i <- i + 1;
     }
     #print(j);
     j <- j + 1;
   }
 return(list(max = max, vectorMax = vecMax));
}
prod4PoStolbcam <- function(m){
  prod4S <- matrix(0, 20, 17);
  razmernost <- dim(prod4S);
  j <- 1;
  while (j<=razmernost[1]){
    #print(prod4S);
    v <- m[,j];
    i <- 1;
    max <- prod(v[i:(i+3)]);
    while (i<=razmernost[2]){
      prod4S[j,i] <- prod(v[i:(i+3)]);
      if (max < prod(v[i:(i+3)])){
        max <- prod(v[i:(i+3)]);
        vecMax <- v[i:(i+3)];
      }
      i <- i + 1;
     }
     #print(j);
     j <- j + 1;
   }
 return(list(max = max, vectorMax = vecMax));
}
prod4diag <- function(m){
  j <- 1;
  d <- 20 + row(m) - col(m);
  list <- split(m,d);
  max <- 0;
  while (j<=length(list)){
    v <- unlist(list[j]);
    #print(v);
    i <- 1;
    while ( (i<=length(v) - 3) && (length(v)>4) ){
      if (max < prod(v[i:(i+3)])){
        #print(paste0("индикатор",i));
        max <- prod(v[i:(i+3)]);
        vecMax <- v[i:(i+3)];
      }
      i <- i + 1;
     }
     j <- j + 1;
   }
 return(list(max = max, vectorMax = vecMax));
}
prod4Adiag <- function(m){
  j <- 1;
  colminus <- sqrt(length(m)) + 1 - col(m);
  d <- 20 + row(m) - colminus;
  list <- split(m,d);
  max <- 0;
  while (j<=length(list)){
    v <- unlist(list[j]);
    #print(v);
    i <- 1;
    while ( (i<=length(v) - 3) && (length(v)>4) ){
      if (max < prod(v[i:(i+3)])){
        #print(paste0("индикатор",i));
        max <- prod(v[i:(i+3)]);
        vecMax <- v[i:(i+3)];
      }
      i <- i + 1;
     }
     j <- j + 1;
   }
 return(list(max = max, vectorMax = vecMax));
}

maximus <- function(m){
  Diagonal = c("Stolbcam", "Strokam", "Diagonal", "Adiag");
  max <- c(prod4PoStolbcam(m)$max, prod4PoStrokam(m)$max, prod4diag(m)$max, prod4Adiag(m)$max );
  #vecMax <- c(prod4PoStolbcam(m)$vec, prod4PoStrokam(m)$vec, prod4diag(m)$vec, prod4Adiag(m)$vec);
  df <- data.frame(Diagonal, max, stringsAsFactors = F);
  #list <- list(Strokam = prod4PoStrokam(m), Stolbcam = prod4PoStolbcam(m), Diagonal = prod4diag(m), Adiag = prod4Adiag(m));
 return(df);
}

maximus(m)

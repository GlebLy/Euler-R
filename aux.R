kvadrKoren <- function(a,b,c){
  D = b^2-4*a*c;
  if(D >= 0){
    x1 = (-b - D^(0.5))/(2*a);
    x2 = (-b + D^(0.5))/(2*a);
    return(c(x1,x2));
  }else{
    return(NULL);
  }
}

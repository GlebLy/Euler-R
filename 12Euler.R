kolDelitelej <- function(triugolnoe){
  schetchik <- 0;
  for(i in 0:triugolnoe+1){
    if(triugolnoe%%i==0)schetchik <- schetchik + 1;
  }
  schetchik;
}
triugolnoe <- function(i)i*(i+1)/2;
df <- data.frame(i=1:6000);
df$triugolnoe = triugolnoe(df$i);
df$kolDelitelej = sapply(df$triugolnoe, kolDelitelej);
df$mnogoDelitelej = df$kolDelitelej >= 500;
df2 <- data.frame(i=6001:12000);
df2$triugolnoe = triugolnoe(df2$i);
df2$kolDelitelej = sapply(df2$triugolnoe, kolDelitelej);
df2$mnogoDelitelej = df2$kolDelitelej >= 500;
df <- rbind(df, df2);
df3 <- data.frame(i=12001:13000);
df3$triugolnoe = triugolnoe(df3$i);
df3$kolDelitelej = sapply(df3$triugolnoe, kolDelitelej);
df3$mnogoDelitelej = df3$kolDelitelej >= 500;
df <- rbind(df, df3);
subset(df, mnogoDelitelej == T);
write.csv(df, file = "triugolnoeYkolDelitel.csv");
df

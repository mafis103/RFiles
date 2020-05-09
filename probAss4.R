set.seed(21)
B<-10000
S<-replicate(B,{
  K<-sample(c(6,-1),replace=TRUE,500,prob=c(5/38,33/38))
  sum(K)
})
mean(S>0)

avg <- 500* (6*(5/38) + -1*(33/38))
se <- sqrt(500)*abs((6 - -1))*sqrt((5/38)*(33/38))
se


pnorm(0,avg,se)

#mean(S>=8)

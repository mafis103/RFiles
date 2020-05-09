set.seed(21)
p <- seq(0.25, 0.95, 0.05)
B<-1000
S<-replicate(B,{
  K<-sample(c(1,0),replace=TRUE,44,prob=c(0.76,0.24))
  sum(K)
})
print(p[1])
print(" ergibt ")
quantile(S,0.8)
?quantile

L<-replicate(B,{
  K<-sample(c(1,0),replace=TRUE,44,prob=c(p[11],1-p[11]))
  sum(K)
})
print(p[11])
print(" ergibt ")
quantile(S,probs=p,names=TRUE)


#avg <- B * (1*0.2 + -0.25*0.8)
#se <- sqrt(44)*abs((1 - -0.25))*sqrt(0.2*0.8)
#se

#1-pnorm(8,avg,se)

#mean(S>=8)

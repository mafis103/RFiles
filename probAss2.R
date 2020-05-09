meal <- rep(c("sides", "drinks", "entree"), times = c(2,1,1))    # create an urn with 2 red, 3 blue
menu <- rep(c("sides", "drinks", "entree"), times = c(6,2,6))

number<-nrow(combinations(6,3))*nrow(combinations(3,1))*nrow(combinations(6,1))
number

sides<-function(amount){
  variation<-nrow(combinations(3,1))*nrow(combinations(amount,2))*nrow(combinations(6,1))
  print("Menge:")
  print(amount)
  print("Varianten:")
  print(variation)
}

sapply(2:12,sides)

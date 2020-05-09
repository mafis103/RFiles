beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object
sample(beads, 1)    # sample 1 bead at random
mean(beads)

B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions

rolls<-rep(c("cyan", "magenta", "yellow"), times =c(3,5,7))
B<-100000
events<-replicate(B,sample(rolls,1))
tabs<-table(events)
prop.table(tabs)
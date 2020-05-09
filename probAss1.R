library(gtools)
medals <- rep(c(1,1,1,0,0,0,0,0))    
    # view beads object
set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")


results <- replicate(10000, {
  jams_wins <- sample(runners, 3)
  jams_wins[1]=="Jamaica" & jams_wins[2]=="Jamaica" & jams_wins[3]=="Jamaica"
})
mean(results)

#?permutations
#permutations(3,3)    # ways to choose 2 numbers in order from 1:5
#all_medals <- permutations(3, 3)
#all_medals
#nrow(all_medals)
sample(c(win=17,loss=-1), 3, replace=TRUE)
?sample



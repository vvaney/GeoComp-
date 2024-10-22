##	This bot plays completely randomly.  It doesn't care
##		what has come before, or try to predict anything,
##		it randomly chooses one of the three options and 
##		plays it:

player_randombot <- function(my_plays=NULL, their_plays=NULL, outcomes=NULL) {
	# random_num = runif(1)
	# if (0 <= random_num <= 0.33) {
		# play <- "rock"
	# } else if (0.33 < random_num <= 0.66) {
		# play <- "paper"
	# } else {
		# play <- "scissors"
	# }
	
	##	Simpler, and equivalent to uniform sampling
	##		distribution from above:
	play <- sample( c("rock", "paper", "scissors"), size=1)
	
	return(play)
}

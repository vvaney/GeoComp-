##	This bot constantly changes up its play, and does so 
##		randomly, based on not playing the same choice twice
##		in a row, and flipping a coin to choose between
##		remaining two options:

player_switchbot <- function(my_plays=NULL, their_plays=NULL, outcomes=NULL) {
	##	Toss a coin and choose between one of the two
	##		choices you *did not* choose the last time:
	
	##	Check to see if this is the first play of the game:
	if ( length(their_plays) == 0 ) {
		##	Choose "rock" if it is:
		play <- "rock"
	} else {
		if ( tail(my_plays, 1) == "rock" ) {
			play <- sample( c("paper", "scissors"), size=1, prob=c(0.5, 0.5))
		} else if ( tail(my_plays, 1) == "paper" ) {
			play <- sample( c("rock", "scissors"), size=1, prob=c(0.5, 0.5))
		} else {
			play <- sample( c("rock", "paper"), size=1, prob=c(0.5, 0.5))
		}
	}
	return(play)
}

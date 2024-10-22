##	This bot uses a simple algorithm to rotate through
##		all three choices sequentially, each play choosing
##		the next choice in the list of levels, which are 
##		you guessed it, "rock," "paper," and "scissors."

player_rotatebot <- function(my_plays=factor(levels=c("rock","paper","scissors")), their_plays=NULL, outcomes=NULL) {
	##	Rotate the choice each round:
	play <- levels(my_plays)[(length(my_plays) %% 3) + 1]

	return(play)
}

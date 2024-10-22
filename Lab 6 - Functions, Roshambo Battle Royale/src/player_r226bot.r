##	This bot uses a random choice model, but biases its 
##		selection towards choosing "scissors."  The syntax of
##		the sample() function allows you to input custom 
##		probabilities in the (prob=) argument:

player_r226bot <- function(my_plays=NULL, their_plays=NULL, outcomes=NULL) {
	##	A biased random sample:
	play <- sample( c("rock", "paper", "scissors"), size=1, prob=c(0.2, 0.2, 0.6))
	return(play)
}

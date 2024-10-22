##	This is a slightly more sophisticated bot.  It analyzes
##		its opponent's last play and then chooses to play the 
##		option that would have won based on that last play:

player_copybot <- function(my_plays=NULL, their_plays=NULL, outcomes=NULL) {
	##	Check to see if this is the first play of the game
	##		and play "rock" since we have nothing to go on:
	if ( length(their_plays) == 0 ) {
		play <- "rock"
	} else {
		if ( their_plays[length(their_plays)] == "rock" ) {
			play <- "paper"
		} else if ( their_plays[length(their_plays)] == "paper" ) {
			play <- "scissors"
		} else {
			play <- "rock"
		}
	}
	return(play)
}

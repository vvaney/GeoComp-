##	So this bot uses a basic statistical likelihood estimation
##		to choose the option that would have beaten the most 
##		frequent choice of its opponent.  It sums up the number
##		of times the opponent has chosen "rock," "paper," or 
##		"scissors," and then plays the option that would have
##		beaten that most frequent choice:

player_freqbot <- function(my_plays=NULL, their_plays=factor(levels=c("rock","paper","scissors")), outcomes=NULL) {
	##	This bot chooses whatever will have beaten the 
	##	opponent's most frequent play:
	
	##	Create a table of counts of our opponent's plays:
	frequencies <- table(their_plays)
	
	##	Use the which.max() function on the table to pull
	##		a subset from the table, and then the names()
	##		function to get the name of that single column
	##		to return the most frequent play our opponent
	##		makes:
	most_freq <- names( which.max( frequencies ) )
	
	##	Calculate the play to beat our opponent's most 
	##		frequent play:
	if ( most_freq == "rock") {
		play <- "paper"
	} else if (most_freq == "paper") {
		play <- "scissors"
	} else {
		play <- "rock"
	}
	
	return(play)
}

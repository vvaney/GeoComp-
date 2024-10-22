##	This is the function for running head to head competitions
##		between the rock-paper-scissors bots.  See the player_...
##		files in this folder for self-contained examples of the bots
##		themselves.
##		
##		Bots are just functions	that you provide to roshambo() in 
##		the player1 and player2	arguments.  You can change the 
##		default number of rounds and plotting arguments to control 
##		how long the match will run in numbers of rounds, and 
##		whether plotting occurs	as you go or just at the end of the match:

roshambo <- function(player1, player2, rounds = 1000, plotting=TRUE) {
	##	These are the variables that will hold each player's
	##		play history, as well as a player-centric history 
	##		of outcomes.  Each time a player plays, it is added
	##		to the appropriate list of plays, and the code below
	##		will compare the plays and assign the appropriate 
	##		outcome for each player.
	p1_plays <- factor(levels=c("rock", "paper", "scissors"))
	p2_plays <- factor(levels=c("rock", "paper", "scissors"))
	p1_outcomes <- factor(levels=c("win", "loss", "draw"))
	p2_outcomes <- factor(levels=c("win", "loss", "draw"))
	
	##	Set up primary loop for the specified number of rounds:
	for (i in 1:rounds) {
		##	For each round, the player functions, player1() and
		##		player2() are fed their own play history (my_plays),
		##		the play history of their opponent (their_plays), 
		##		and their outcomes ("win"/"loss"/"draw") 
		##		against that opponent (p1_outcomes or p2_outcomes):
		
		##	Calculate the plays for each player:
		p1_play <- player1(my_plays=p1_plays, their_plays=p2_plays, outcomes=p1_outcomes)
		p2_play <- player2(my_plays=p2_plays, their_plays=p1_plays, outcomes=p2_outcomes)
		
		##	Assign this round's play to the play history for each:
		p1_plays[i] <- p1_play
		p2_plays[i] <- p2_play
		
		##	Calculate the outcome for this round and assign it
		##		to each player's outcome vector:
		if ( p1_play == p2_play ) {
			p1_outcomes[i] <- "draw"
			p2_outcomes[i] <- "draw"
		} else if (p1_play == "rock") {
			if (p2_play == "paper") {
				p1_outcomes[i] <- "loss"
				p2_outcomes[i] <- "win"
			} else {
				p1_outcomes[i] <- "win"
				p2_outcomes[i] <- "loss"
			}
		} else if (p1_play == "paper") {
			if (p2_play == "scissors") {
				p1_outcomes[i] <- "loss"
				p2_outcomes[i] <- "win"
			} else {
				p1_outcomes[i] <- "win"
				p2_outcomes[i] <- "loss"
			}
		} else if (p1_play == "scissors") {
			if (p2_play == "rock") {
				p1_outcomes[i] <- "loss"
				p2_outcomes[i] <- "win"
			} else {
				p1_outcomes[i] <- "win"
				p2_outcomes[i] <- "loss"
			}
		}
		
		##	Summarize results in a 2 x 3 plot:
		if (plotting | (i == rounds) ) {
			panel_par <- par(mfrow=c(3,2), mar=c(3,3,1,1), oma=c(0,0,3,1))
			plot(p1_plays)
			plot(p2_plays)
			plot(p1_outcomes)
			plot(p2_outcomes)
			
			##	mosaicplot() is useful for comparing two factors:
			mosaicplot(p1_plays ~ p1_outcomes, color=TRUE, main="Player 1")
			mosaicplot(p2_plays ~ p2_outcomes, color=TRUE, main="Player 2")
			mtext( paste("Player 1:", table(p1_outcomes)[1], "vs.", "Player 2:", table(p2_outcomes)[1]), side=3, line=1, outer=TRUE, cex=2, font=2 )
			par(panel_par)
		}
	}
	
	##	Create a data.frame object from each player's 
	##		play history and outcomes:
	output = data.frame(p1_plays, p2_plays, p1_outcomes, p2_outcomes)
	
	##	Print a summary of the output data.frame:
	print(summary(output))
	
	##	But return the data.frame itself:
	return( output )
}

##	This function will run the player you provide against the
##		seven basic players I coded for you.  Your goal is to 
##		win better than half of all the rounds played (1000 per
##		matchup).

##	Set the working directory to our source code location:
setwd("Lab 6 - Functions, Roshambo Battle Royale/src/")

##	First, we'll source() the roshambo() match function, as
##		well as the seven basic players:
source("roshambo.r")
source("player_randombot.r")
source("player_rockbot.r")
source("player_rotatebot.r")
source("player_r226bot.r")
source("player_switchbot.r")
source("player_copybot.r")
source("player_freqbot.r")

##	The throwdown() function expects to receive a player
##		bot (in the form of a function, yes, we can
##		pass functions objects as arguments to other 
##		functions), and has an optional argument to control
##		the number of rounds to play against each competitor:
throwdown <- function(player, rounds=1000) {
	##	Create vectors for holding the outcomes from our
	##		matchups, the name of the opponent, number of 
	##		wins, losses, and draws:

	wins <- numeric()
	losses <- numeric()
	draws <- numeric()

	opponents <- c(
		"player_randombot",
		"player_rockbot",
		"player_rotatebot",
		"player_r226bot",
		"player_switchbot",
		"player_copybot",
		"player_freqbot"
	)
	
	##	Set up primary loop for the opponents listed above:
	i <- 1
	for (opponent_name in opponents) {
		print( paste( "Player 1 vs.", opponent_name ) )

		##	Because we are passing our opponents to the
		##		the loop as function names in strings, rather
		##		than the functions themselves, we have to use
		##		the get() function to load that bot's function
		##		code by name into a new function object called
		##		new_opponent
		new_opponent <- get(opponent_name)

		##	Pass our player and the opponent player to the roshambo()
		##		competition function, with the specified number of rounds,
		##		storing the output from all of the rounds in the "outcome"
		##		data object.
		outcome <- roshambo(player1=player, player2=new_opponent, rounds=rounds)
		
		##	Calculate a frequency table on our player's outcomes:
		frequencies <- table( outcome$p1_outcomes )

		##	Store our outcomes against the current opponent in our
		##		outcome vectors:
		wins[i] <- frequencies[["win"]]
		losses[i] <- frequencies[["loss"]]
		draws[i] <- frequencies[["draw"]]

		i <- i + 1
	}
	
	##	Construct a final outcome data.frame listing our
	##		wins, losses, and draws against each of the opponents:
	output = data.frame(opponents, wins, losses, draws)

	##	Print out the win/loss ratio against all opponents:
	print( paste( "Win/Loss Ratio:", sum(output$wins) / sum(output$losses) ))
	
	return( output )
}


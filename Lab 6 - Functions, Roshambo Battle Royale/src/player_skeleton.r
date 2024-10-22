player_skeleton <- function(my_plays=NULL, their_plays=NULL, outcomes=NULL) {
  frequencies <- table(their_plays)
  
  if (length(their_plays) < 102) {
    # Play rock if fewer than 102 plays
    play <- "rock"
  } else {
    if (all(tail(their_plays, 102) == "rock")) {
      play <- "paper"
    } else if (all(tail(their_plays, 102) == rep(c("rock", "paper", "scissors"), times = 34))) {
      plays_cycle <- c("paper", "scissors", "rock")
      play <- plays_cycle[(length(my_plays) %% 3) + 1]
    } else if (all(tail(their_plays, 102) == "paper")) {
      plays_cycle <- c("rock", "scissors", "paper")
      play <- plays_cycle[(length(my_plays) %% 3) + 1]
    } else {
      most_freq <- names(which.max(frequencies))
      play <- ifelse(most_freq == "scissors", "rock",
                     ifelse(most_freq == "rock", "paper", "scissors"))
    }
  }
  
  if (is.null(play)) {
    last_play <- tail(my_plays, 1)
    play <- ifelse(last_play == "rock", sample(c("paper", "scissors"), 1),
                   ifelse(last_play == "paper", sample(c("rock", "scissors"), 1),
                          sample(c("rock", "paper"), 1)))
  }
  
  return(play)
}


##	Let's load two of the sample players. We can
##		do this by using the source() function
##		to load and evaluate the contents of a
##		source code file.  By default, source() looks
##		in the current working directory for the 
##		specified file, so set your working directory
##		to the /src folder from the lab:
# setwd("H:/tmp/src/")
# source("player_randombot.r")
# source("player_rockbot.r")

##	What did these source files load?
# ls()

##	Examine the contents and structure of these 
##		players:
# class(player_rockbot)
# args(player_rockbot)
# formals(player_rockbot)
# body(player_rockbot)

##	You should have noticed by now that our players
##		will have no other goal than to take the
##		information provided, which is the history
##		of our own plays, the history of our opponents
##		plays, and the outcomes of those plays as
##		arguments, perform some magic and return our
##		next play based on what that magic tells us to 
##		do.  We need another function to actually 
##		conduct the competition between our players.
##
##		To pit our player against one of the other 
##		players we use the roshambo() function
##		from the "roshambo.r" source code:
source("roshambo.r")
roshambo(player_skeleton, player_copybot)

##	Look at the supplied arguments, which player
##		is which by default, and how can we change
##		the number of rounds over which the competition
##		runs?  How does it determine who wins each round
##		and what does it do with that information?

##	Finally, if we source the throwdown() function
##		from its source code file, you can pit your
##		player against all of the sample players I've
##		provided you.  Your grade will be based on your
##		ability to score a combined win/loss ratio greater
##		than 1.9 over 1000 rounds against each competitor:
source("throwdown.r")

#throwdown(player_skeleton)


##	I'm not going to provide you with any more examples.
##		But not because I don't care.  :)  You should be able 
##		to figure out how to run the throwdown code, change 
##		the number of rounds to the required 1000 for your
##		final test, and try plotting the wins for each opponent
##		if you have extra time.
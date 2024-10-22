player_combined_bot <- function(my_plays=NULL, their_plays=NULL, outcomes=NULL) {
  if (length(their_plays) == 0) {
    play <- "rock"
  } else {
    last_play <- tail(my_plays, 1)
    last_their_play <- tail(their_plays, 1)
    frequencies <- table(their_plays)
    most_freq <- names(which.max(frequencies))
    
    # Determine if the opponent is `player_copybot`
    if (last_their_play == "rock" && last_play == "scissors" ||
        last_their_play == "paper" && last_play == "rock" ||
        last_their_play == "scissors" && last_play == "paper") {
      # Adapt to `player_copybot`
      if (last_play == "rock") {
        play <- "scissors"  # Copybot will play paper, so you play scissors
      } else if (last_play == "paper") {
        play <- "rock"  # Copybot will play scissors, so you play rock
      } else {
        play <- "paper"  # Copybot will play rock, so you play paper
      }
    } else if (length(their_plays) >= 3) {
      # Adapt to `player_rotatebot`
      rotate_sequence <- c("rock", "paper", "scissors")
      next_index <- (match(last_their_play, rotate_sequence) %% 3) + 1
      play <- rotate_sequence[next_index]
    } else if (most_freq == "rock") {
      # Adapt to `player_freqbot`
      play <- "paper"
    } else if (most_freq == "paper") {
      play <- "scissors"
    } else if (most_freq == "scissors") {
      play <- "rock"
    } else if (last_play == "rock") {
      # Adapt to `player_switchbot`
      play <- sample(c("paper", "scissors"), size=1, prob=c(0.5, 0.5))
    } else if (last_play == "paper") {
      play <- sample(c("rock", "scissors"), size=1, prob=c(0.5, 0.5))
    } else {
      play <- sample(c("rock", "paper"), size=1, prob=c(0.5, 0.5))
    }
  }
  
  return(play)
}

throwdown(player_combined_bot)

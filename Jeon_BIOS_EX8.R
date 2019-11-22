
#1. Analysis of data surrounding sports teams has grown into a major business for the teams themselves and the media. 
#One cool summary plot that media outlets generate to summarize a game, in this case basketball, 
#is a line graph depicting the cumulative score for each team as a function of time in the game (see below).
library(ggplot2)
library(dplyr)

data <- read.table("UWvMSU_1-22-13.txt", header = T)

uw <- data[data$team=="UW",]
uw[,"cum"] <- cumsum(uw$score)

msu <- data[data$team=="MSU",]
msu[,"cum"] <- cumsum(msu$score)


ggplot() + geom_line(data=uw, aes(x=time, y=cum),col="blue")+
  geom_line(data=msu, aes(x=time,y=cum),col="red") +
  labs(x="Time",y="Cumulative Score")

#22. Write a game called “guess my number”. The computer will generate a random number between 1 and 100.
#The user types in a number and the computer replies “lower” if the random number is lower than the guess, 
#“higher” if the random number is higher, and “correct!” if the guess is correct. The player can continue guessing up 
#to 10 times.
guess_game <- function() {  #Run by guess_game()
  answer <- round(runif(1) * 100, digits = 0)
  guess <- -1
  count <- 10

  while(guess != answer)
  { 
    guess <- readline(prompt="Enter an integer: ")
    
    if (guess == answer)
    {
      cat("Correct!")
    }
    
    else if (count==1)
    {
      cat("Remaining count is 0. Fail...")
      break
    }
    
    else if (guess < answer)
    {
      cat("higher!")
      count <- count-1
      cat(c("    remaining count is ",count))
    }
    
    else if(guess > answer)
    {
      cat("lower!")
      count <- count-1
      cat(c("    remaining count is ",count))
    }
  }
}

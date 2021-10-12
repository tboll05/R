# Name: Project2_Playing_Cards
# Purpose: This program taking a data set that represents playing cards.
#          The user is prompted on which card game they want to play.
#          Based on their input, the values of the cards are modified.
#          The user can then use the shuffle and deal functions to simulate playing cards.
#
#          Question for later thought.  How would I see the shuffled version of deck?
#
#Author: Timothy Bollig
#Date last edited: 10/11/2021
#Copyright: GNU General Public License v3.0
#Contact: bollig.timothy@gmail.com

################################################################################
################################################################################

#Import card data set as cards and save a copy into a variable named deck.
cards <- read.csv("cards.csv", stringsAsFactors = FALSE)
deck <- cards

#Function: setup
#Purpose: Define deal and shuffle functions and set up a separate environment
#for objects used by these functions.
#Output: Returns the deal and shuffle functions.
setup <- function(deck){
  DECK <- deck

  #Function: DEAL
  #Purpose: Select first card in deck and return it and overwrite deck with remaining card.
  #Output: A card vector with face, suit, and value.
  DEAL <- function(){
    card <- deck[1,]
    assign("deck", deck[-1,], envir = parent.env(environment()))
    card
  }

  #Function: SHUFFLE
  #Purpose: Overwrite the deck data set in a random order, simulating a shuffled deck.
  #Output: A randomized order of the original deck data set.
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random,], envir = parent.env(environment()))
  }
  
  list(deal = DEAL, shuffle = SHUFFLE)
}

#Establish a vector of valid inputs
valid_inputs <- c("regular","hearts","blackjack")

#Prompt user for input.  Valid input: Hearts, Blackjack, Regular.
#Input will determine how card values are changed in deck before passing it to setup.
game <- readline(prompt = "What game do you want to play? (Regular, Hearts, Blackjack")
game <- tolower(game)

#While loop to validate user input is valid.  If the input is not in the list of
#valid inputs then the user will be prompted to enter something valid.
while (!(game %in% valid_inputs)){
  game <- readline(prompt = "Invalid option. Please enter Regular, Hearts, or Blackjack")
  game <- tolower(game)
}

#Depending on input, modify the values of cards accordingly and then pass deck to setup.

#Regular game does not modify card values.
#Hearts will set card values to 0.  Any Hearts cards are set to 1.  Queen of Spades is set to 13.
#Blackjack will set face cards to 10 and aces to NA.

if (game == "regular"){
  cards <- setup(deck)
}else if (game == "hearts"){
  deck$value <- 0
  deck$value[deck$suit == "hearts"] <- 1
  queen_of_spades <- deck$face == "queen" & deck$suit == "spades"
  deck$value[queen_of_spades] <- 13
  cards <- setup(deck)
}else if (game == "blackjack"){
  facecard <- deck$face %in% c("king","queen","jack")
  deck$value[facecard] <- 10
  deck$value[deck$face == "ace"] <- NA
  cards <- setup(deck)
}

#Assign the deal and shuffle functions created by the setup function.
#This allows user to execute these functions.
deal<- cards$deal
shuffle <- cards$shuffle

##################################End of File#################################
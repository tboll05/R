# Name: Project1_Weighted_Dice
# Purpose: This program simulates the rolling of weighted dice.
#   The program in it's original state simulated 10,000 rolls but can
#   be updated to prompt the user for input on the number of desired rolls
#
#Author: Timothy Bollig
#Date: 9/16/2021
#Copyright: GNU General Public License v3.0
#Contact: bollig.timothy@gmail.com


#The prob argument makes 1 - 5 have a 1/8 chance of coming up and 6 a 3/8 chance
roll <- function() {
  die <- 1:6
  dice <- sample(die, size=2, replace=TRUE, prob=c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
  sum(dice)
}

#Simulate 10,000 dice rolls
rolls <- replicate(10000, roll())

#Visualize results of rolls
qplot(rolls, binwidth=1)
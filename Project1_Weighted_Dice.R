# Name: Project1_Weighted_Dice
# Purpose: This program simulates the rolling of fair and weighted dice.
#   10,000 rolls of each pair are simulated and then visualized for comparison.
#
#Author: Timothy Bollig
#Date last edited: 9/19/2021
#Copyright: GNU General Public License v3.0
#Contact: bollig.timothy@gmail.com

#Load ggplot library
#library(ggplot2)


#Function simultates rolling a fair pair of dice
roll_fair <- function() {
  die <- 1:6
  dice <- sample(die, size=2, replace=TRUE)
  sum(dice)
}

#Function simulates rolling a pair of dice weighted towards higher numbers
#The prob argument makes 1 - 5 have a 1/8 chance of coming up and 6 a 3/8 chance
roll_weighted <- function() {
  die <- 1:6
  dice <- sample(die, size=2, replace=TRUE, prob=c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
  sum(dice)
}

#Simulate 10,000 fair dice rolls
fair_rolls <- replicate(10000, roll_fair())

#Simulate 10,000 weighted dice rolls
weighted_rolls <- replicate(10000, roll_weighted())

#Create a plot where two graphs will display side by side
#mfrow=c(1,2) means number of rows and number of columns in plot
par(mfrow=c(1,2))

#Visualize and compare the results of rolls
hist(fair_rolls, main="Fair Dice (10,000 rolls)", xlab="Sums of Rolls")
hist(weighted_rolls, main="Weighted Dice  (10,000 rolls)", xlab="Sums of Rolls")


#Visualize with ggplot2
# qplot(fair_rolls, binwidth=1)
# qplot(weighted_rolls, binwidth=1)
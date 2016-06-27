# PrisonersDilemma
This program uses data from a Prisoners Dilemma experiment in order to estimate the parameters of a simple reinforcement learning model.

###PD_Instructions.pdf
This document outlines the ideas and methodology behind this program. This should be read first to get a sense of the direction of the project.

###pddata.csv
This is the data used in the program. It was collected in a classroom experiment performed by a Cal Poly Economics professor. 20 individuals were asked to play a prisoners dilemma game with classmates 100 times. "choice" is a binary variable that denotes whether or not a student chose outcome A of the prisoners game (1 if choose A). "otherchoice" is likewise for the student that was being played with. "round" indicates which iteration of the game was being recorded. "pa" and "pb" indicate which player received the $1 payment from the game (student A, student B, both, or neither). "econ" indicates whether or not the student is an economics major.

###Priz_Dilemma
This program takes the given data and provides output according to the desired results outlined in the PD_Instructions file. Specifically, it estimates the parameters of two different learning reinforcement model specifications using a log-likelihood function that is passed through a built-in R optimization function.

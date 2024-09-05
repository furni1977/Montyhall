#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' Select a Random Door.
#' @description
#' This function randomly selects one of the available doors in the Monty Hall game.
#' @details
#' The Monty Hall problem involves three doors, behind one of which is a car and behind the other two are goats. This function simulates the contestant's initial choice of a door.
#' @param 
#' None
#' @return
#' A single numeric value representing the selected door (between 1 and 3). 
#' @examples
#' Randomly select a door, select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Open a Goat Door.
#' @description 
#' This function simulates the host opening a door that reveals a goat after the contestant has made their initial door selection in the Monty Hall problem.
#' @details
#' Depending on whether the contestant's initial pick is a car or a goat, the function selects a door to open. If the contestant picks a car, one of the two doors hiding goats is opened. If the contestant picks a goat, the other goat door is opened.
#' @param 
#' game A character vector of length 3, representing the placement of the "car" and "goat" behind the doors.
#' a.pick A numeric value (between 1 and 3) representing the door chosen by the contestant.
#' @return 
#' A numeric value representing the door number (between 1 and 3) that the host opens, which reveals a goat.
#' @examples
#' game <- c("car", "goat", "goat") 
#' random game setup
#' a.pick <- 1 
#' contestant selects door 1
#' open_goat_door(game, a.pick)
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Make a Final Door Selection.
#' @description
#' This function determines the contestant's final door choice in the Monty Hall problem based on whether they choose to stay with their initial pick or switch to the remaining unopened door.
#' @details 
#' The contestant has the option to either stay with their original door selection or switch to the other unopened door. If they stay, the initial door choice is returned. If they switch, the function returns the other unopened door.
#' @param 
#' stay A logical value (TRUE or FALSE) indicating whether the contestant decides to stay with their initial pick (default is TRUE).
#' opened.door A numeric value representing the door number (1, 2, or 3) that the host opened to reveal a goat.
#' a.pick A numeric value representing the contestant's initial door choice (1, 2, or 3).
#' @return
#' A numeric value representing the contestant's final door selection (1, 2, or 3).
#' @examples
#' Contestant chooses to stay
#' change_door(stay = TRUE, opened.door = 3, a.pick = 1)
#' Contestant chooses to switch
#' change_door(stay = FALSE, opened.door = 2, a.pick = 1)
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine if the Contestant Wins or Loses.
#' @description 
#' This function determines whether the contestant has won or lost the Monty Hall game by comparing their final door selection with the location of the car.
#' @details 
#' The function takes the contestant's final door selection and checks whether the selected door hides a car or a goat. If the contestant selected the door with the car, they win. Otherwise, they lose.
#' @param
#' final.pick A numeric value representing the contestant's final door choice (1, 2, or 3).
#' game A character vector of length 3 that represents the positions of the car and goats behind the doors. For example, `c("car", "goat", "goat")`. 
#' @return 
#' A character string indicating whether the contestant won or lost the game. Returns `"WIN"` if the contestant picks the car, and `"LOSE"` if the contestant picks a goat.
#' @examples
#' Example game setup
#' game <- c("car", "goat", "goat")
#' Contestant selects the car
#' determine_winner(final.pick = 1, game = game)
#' Contestant selects a goat
#' determine_winner(final.pick = 2, game = game)
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play a Monty Hall Game
#' @description
#' This function simulates one complete round of the Monty Hall game, testing both strategies: staying with the initial door selection or switching to the other door after a goat door is revealed.
#' @details
#' The game begins with a contestant selecting a random door. One of the remaining doors hiding a goat is then revealed. The contestant is given two options: stay with the original door or switch to the other unopened door. The function simulates both strategies and determines the outcome (win or lose) for each.
#' @param 
#' None 
#' @return
#' A data frame with two columns: `strategy` (indicating whether the contestant stayed or switched) and `outcome` (indicating whether the contestant won or lost for each strategy). 
#' @examples
#' Play a single Monty Hall game and display the results
#' play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Play game for 100 doors and store outcomes.
#' @description
#' This function simulates multiple rounds of the Monty Hall game, for a specified number of games, and returns the list of outcomes for each game. By default, it plays 100 rounds.
#' @details
#' The function simulates multiple Monty Hall games, each with a contestant's initial choice, the host revealing a door, and the contestant making a final choice. It then stores the results of both strategies (stay and switch) and computes the proportion of wins for each strategy.
#' @param
#' n The number of games to be played (default is 100). 
#' @return
#' A data frame containing the outcomes for each game strategy (stay or switch) and whether the contestant won or lost. 
#' @examples
#' Play 100 Monty Hall games and store the results
#' play_n_games(100)
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}


  
setup.predator <- function(eat, kill, repro){
  if(!is.numeric(eat) | !is.numeric(kill) |!is.numeric(repro))
    stop("needs numeric data")
  if(length(kill) !=1)
    stop("predators need one kill probability")
  if(length(repro) != 1)
    stop("predators need one repro probability")
  sated <- length(eat)
  return(list(eat=eat, kill=kill, repro=repro, sated=sated))
}

#Every time a predator eats a prey item it should fully kill it(1) and be sated(0) 




#The new.loc function should: 
#avoid water and walking off of the grid 
#reproduce and eat herbivores 
#needs to take carnivore.matrix and come up with a set of possible new locations

predator.array <-  array("", dim=c(dim(terrain), timesteps+1))


herbivore.matrix <- matrix(sample(c(NA, "",c(1:5)), 9^2, replace=TRUE), nrow=9, ncol=9)
herbivore.matrix

predator.matrix <- matrix(sample(c(NA, "",herbivore.matrix),9^2,replace=TRUE), nrow = 9, ncol = 9)
predator.matrix

new.loc <- function(cell, predator, temp){
  if(cell == "")
    return("")
  if(cell >=70)
    return("predator")
  if(is.na(cell))
    return(NA)  
  if(cell == "predator")
    return("predator")
  if(runif(1) <= info$survive[predator]) #$refers to a sp column relative to a sp data frame
    return(info$survive[plant])
}

#^^I need to simulate a predator consuming prey, what would that look like? 
#if a herbivore is in a cell the predator needs to both occupy that cell and set the 
#herbivore to 0 

predator.timestep <- function(predator, terrain, info){
  predator <- function(reproduce, eat, survive){
    survive <- function(cell, info){
      if(cell == "")
        return("")
      if(is.na(cell))
        return(NA)  
      if(runif(1) <= info$survive[predator.array]) 
        return(info$survive[predator.array])
      }
    reproduce<- function(row, col){
      possible.locations <- as.matrix(expand.grid(row+c(-1, 0, 1), col+c(-1, 0, 1)))
      return(predator)
      }
    eat <- function(sample(species_names, 1, prob=predator.array[row,column])){
      }
  }
}
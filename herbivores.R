#' A function that check to make sure that the parameters eat, kill, repro are correct
#'@param eat probability of a herbivore eating a plant
#'@param kill probability of herbivore killing a plant when it eats
#'@param repro probability of herbivore reproducing after it eats
#'@return a list; numeric elements for eat, kill, repro and sated
#'@export

setup.herbivores <- function(eat, kill, repro){
  if(!is.numeric(eat))
    stop("Herbivores need numeric data")
  if(kill <= 0)
    stop("Herbivores need one kill probability")
  if(repro <= 0)
    stop("Herbivores need one reproduction probability")
  sated <- 5
  return(list(eat=eat, kill=kill, repro=repro, sated=sated))
}

# Herbivore moves to new row,column
#   herbivore eats plant in row,column or reproduces
##
#' Takes herbivores, plants, and info as parameters and loops over them to determine how plants move through ecosystem,
#'    runs the entire ecosystem showing herbivores and plant moving, eating, dying, and reproducing.
#' @param herbivores An array randomly seeded with a herbivore
#' @param plants An array randomly seeded with plants which all have a probability of surviving or dying
#' @param info Contains the info parameters of the plants
#' @return a matrix; showing the distribution of herbivores after eating/dying, killing the plants they eat, and reproducing
#' @export
herbivore.timestep <- function(herbivores, plants, info){
  #wrapper functions to do the work
  new.herb <- matrix("", dim(plants)[1], dim(plants)[2])
  for(i in seq(1, nrow(new.herb))){
    for(j in seq(1, ncol(new.herb))){
      new.herb[i,j] <- eat(herbivore[i,j], info)
    }
    for(i in seq(1, nrow(new.plants))){
      for(j in seq(1, ncol(new.plants))){
        if (new.plants[i,j] != ''){
          new.plants <- reproduce(i, j, new.plants, info)
          
        }
      }
    }
  }
  return(new.herb)
  
  new.loc <- function(row, col, herbivores, plants){
    possible.locations <- as.matrix(expand.grid(row+c(-1, 0, 1),
                                                col+c(-1, 0, 1)))
    possible.locations <- possible.locations[possible.locations[,1] > 0,]
    possible.locations <- possible.locations[possible.locations[,2] > 0,]
    
    possible.locations <- possible.locations[possible.locations[,1] < nrow(plants)+1,]
    possible.locations <- possible.locations[possible.locations[,2] < ncol(plants)+1,]
    
    
    location <- sample(row.names(possible.locations), 1)
    return(location)
  }
  
  #survive funtions?
  herbivore <- function(row, col, herbivores, plants, info){
    # what plant and herbivore are in cell
    plants <- plants[row, col]
    herbivore <- herbivores[row, col]
    
    #move to another cell
    if(herbivores == 0){
      return(herbivores)
    }
    #move somewhere else
    # if(is.na[c]){
    #   return(list(herbivores=herbivores, plants=plants))
    # }
    # see if herbivore eats
    # if it does, then herbivore should be sated
    if(plant[row, col]!=""){
      if(eats > runif(1)){
        return(sated)
      }
    }
    #if it does, then see if it kills plants
    #if it does, then kill the plant
    if(eats > runif(1) & kill > runif(1)){
      return("")
    }
    #if it does, then will it reproduce
    #if it does, then reproduce by finding a new.loc
    if (eats > runif(1)){
      new.loc <- herbivore
      return(new.loc)
    }
    #if it didnt eat, then sated value is reduced 1
    if(eats < runif(1)){
      return(sated - 1)
    }
    return(list(herbivore=herbivores, plants=plants))
  }
  
  # do work
  #loop over the herbivore matrix
  for(i in seq(2,timesteps)){
    herbivores[,,i] <- herbivore.timestep(herbivores[,,i-1], info)
  }
  return(herbivores)
  
  #do herb calcuations
  #propagate through the matrix
  for(i in seq_len(dim(herbivores)[1])){
    for(j in seq_len(dim(herbivores)[2])){
      herbivores[i, j, 1] <- sample(c(herbivore, ""), 1, replace = TRUE,
                                    prob = c(seed.fracs, 1-sum(seed.fracs)))
    }
  }
  return(herbivores)
  return(herbivore.timestep)
}
##PART 2 - R-WORLD
#'
#' Creates a model of an ecosystem that includes plants, herbivores,
#' and carnivores on a terrain.
#'
#' ##Group Work##
#' Scenario: Snakes, which must thermoregulate across the landscape, eat small herbivores (i.e., rabbits).
#' The plants that make up the diet of those herbivores must survive and reproduce, as do the herbivores
#' which act as prey to the surviving and reproducing snakes across a multi-temperature terrain.
#' TEAM
#' Hailey Wayment: Herbivores
#' Megen Kepas: Snakes - temperature regulation
#' Helen Plylar: Terrain - with a temperature variable
#' Stephanie Landry: Snakes - as carnivores
#'
#' To determine if your input data meets requirements of functions,
#' you can use or manipulate the wrappers setup.plants, setup.carnivore,
#' and setup.herbivore below.
#'
#' @param setup.plants wrapper function that includes all impertinent information
#' about the plant species being modeled.
#' @param repro vector of reproductive probability given to each plant species,
#' carnivore, or herbivore being modeled.
#' @param survive vector of survival probability given to each plant species.
#' @param comp.mat matrix of competition probabilities with species'
#' probabilities as columns and rows such that the matrix matches
#' the number of species^2.
#' @param names vector with names of species used in function.
#' @return list object of the reproductive, survival, and competition probabilities,
#' and character names given per species.
#' @param plants matrix of ecosystem with species, empty cells, or NAs
#' as columns and rows.
#' @param timestep array of ecosystem timesteps with species, empty cells, or NAs
#' as columns and rows.
#' @param survive function that determines if species survive each timestep.
#' @param reproduce function that determines if an indidivual is going to reproduce
#' and provide young for the next timestep.
#' @param disperse function that determines which cell the reproducing individual
#' will disperse into for the next timestep.
#' @param compete function that determines which individual wins a cell when
#' two individuals attempt to enter into the same cell after reproduction.
#' @param row row location of individual in matrix.
#' @param col column location of individual in matrix.
#' @param data inputs data set being used at the time, such as plants matrix.
#' @param info inputs the wrapper information that corresponds to the data called.
#' @param setup.carnivore wrapper function that includes all impertinent information
#' about the carnivore species being modeled.
#' @param eat vector sequence of integers that represent timesteps until death since
#' individual was last sated.
#' @param harvest vector of carnivore kill probability for a prey item.
#' @return list object
#' @param setup.herbivore wrapper function that includes all impertinent information
#' about the herbivore species being modeled.
#' @param kill vector of herbivore kill probability for a plant item.
#' @return list object
#' @param snake.matrix matrix of ecosystem with numeric inidivuals (snakes),
#' empty cells, or NAs as columns and rows.
#' @param new.loc.carn function that determines the next location that the carnivore
#' can move to for next timestep.
#' @export timestep matrices

#wrapper
setup.plants <- function(repro, survive, comp.mat, names=NULL){
  if(is.null(names))
    names <- letters[seq_along(repro)]
  #take the length from the length of repro argument,
  #use letters 'a' to letter of that length
  if(length(repro) != length(survive))
    stop("Reproduction and survival parameters needed for all species")
  if(nrow(comp.mat) != length(repro))
    stop("Competition matrix requires reproduction parameters for all species")
  if(ncol(comp.mat) != length(survive))
    stop("Competition matrix requires survival parameters for all species")
  repro <- setNames(repro, names)
  survive <- setNames(survive, names)
  rownames(comp.mat) <- names
  colnames(comp.mat) <- names
  #setNames is a convenience function that sets the names on an object and returns the object.
  #It is most useful at the end of a function definition where one is creating the object to be
  #returned and would prefer not to store it under a name just so the names can be assigned.
  return(list(repro=repro, survive=survive, comp.mat=comp.mat,
              names=names))
}

##Don't create an empty matrix in the function because it will just get thrown out and written over.

#example for how to run function
#info.plant<-setup.plants(repro, survive, comp.mat, names)

##Create plants matrix - example
#plants <- matrix(sample(c(NA, ""),8^2,replace=TRUE), nrow = 8, ncol = 8)
#plants[sample(1:64, 10)] <- info$names

##survive
survive <- function(row, col, data, info){
  if(data[row,col]=='')
    return('')
  if(data[row,col]=="NA")
    return("NA")
  if(runif(1) <= info$survive[data[row,col]]){
    return(data[row,col])
  } else(return(''))
}

#survive all cells in plants matrix (time1) - example
#for(i in 1:nrow(plants)){
#  for(j in 1:ncol(plants)){
#    plants2[i,j]<-survive(row = i, col = j, data = plants, info=info)
#  }
#}

######################################

#reproduction function
disperse<-function(row, col, data, info){    #row and col of matrix working with, call that matrix, and provide species information
  possible.locations<-as.matrix(expand.grid(row + c(-1,0,1), col + c(-1,0,1)))
  new.loc<-subset(possible.locations, possible.locations[,1] > 0 & possible.locations[,2] > 0 & possible.locations[,1] < 6 & possible.locations[,2] < 6)
  x<-sample(new.loc[,1], 1)
  y<-sample(new.loc[,2], 1)
  value<-data[row,col]
  return(data[row = x,col = y]<-value)
}

reproduce<-function(row, col, data, info){
  if(data[row, col]=="NA")
    return("NA")
  if(data[row, col]=='')
    return('')
  if(runif(1) > info$repro[data[row, col]]){
    return(data[row, col])
  } else(return(timestep[x,y,2]<-disperse(row = row, col = col, data = data, info = info)))
}

#run reproduce fxn through all cells in plants matrix (time1) - example
#for(i in 1:nrow(plants2)){
#  for(j in 1:ncol(plants2)){
#    timestep[i,j,2]<-reproduce(row = i, col = j, data = plants2, info = info)
#  }
#}

#competition
compete<-function(info, sp1, sp2) {
  sp<-sample(info$names, 1, prob=info$comp.mat[plants[row,col]])
  plants.disperse1[row, col]<-return(sp)
}

#Insert competition portion into reproduction function
#combine all in an ecosystem function

#####################################################################################

################################

#### CARNIVORE: Snake ####

#wrapper
setup.carnivore <- function(eat, repro, harvest){
  if(is.numeric(eat) != TRUE)
    stop("Carnivores need numeric data")
  if(length(repro) != 1)
    stop("Carnivores need one reproduction probability")
  if(length(harvest) != 1)
    stop("Carnivores need one harvest success probability")
  sated <- 10
  return(list(eat=eat, repro=repro, harvest=harvest, sated=sated))
}
#example for how to run function
#info.carn<-setup.carnivore(eat, repro, harvest)

#mock herbivore with sated = 5
setup.herbivore <- function(eat, repro, kill){
  if(is.numeric(eat) != TRUE)
    stop("Carnivores need numeric data")
  if(length(repro) != 1)
    stop("Carnivores need one reproduction probability")
  if(length(kill) != 1)
    stop("Carnivores need one plant kill probability")
  sated <- 5
  return(list(eat=eat, repro=repro, kill=kill, sated=sated))
}
#example for how to run function
#info.herb<-setup.herbivore(eat, repro, kill)

#Matrix setup - example
#snake.matrix <- matrix(sample(c(NA, ""),8^2,replace=TRUE), nrow = 8, ncol = 8)
#snake.matrix[sample(1:64, 2)] <- info2$sated       ##adding snakes to the landscape
#snake.matrix[sample(1:64, 4)] <- info.herb$sated    ##adding herbivores to the landscape

##CARNIVORES MOVE - but movement depends on herbivore movement...
new.loc.carn<- function(row, col, data, info){
  #potential new location for carnivore given row and col located in currently
  possible.locations<-as.matrix(expand.grid(row + c(-1,0,1), col + c(-1,0,1)))
  new.set<-subset(possible.locations, possible.locations[,1] > 0 & possible.locations[,2] > 0 & possible.locations[,1] < 9 & possible.locations[,2] < 9) #within matrix
  x<-sample(new.set[,1], 1)
  y<-sample(new.set[,2], 1)
  i<-x
  j<-y
  data[i,j]<-info$eat[data[row,col] - 1]
  return(data[i,j])
  #if(data[x, y] != is.na(data[x, y])|class(numeric))   #this is done in the code below
  #return(data[x,y]<-as.numeric(data[row,col])-1)
}

#Executing the function - example
#for(i in 1:nrow(snake.matrix)){
#  for(j in 1:ncol(snake.matrix)){
#    if(is.na(snake.matrix[i, j]))
#      return(NA)
#    if(snake.matrix[i, j] == "")
#      return("")
#    if(snake.matrix[i, j] == 0)
#      return("")
#    if(snake.matrix[i, j] == class(numeric))
#      return(new.loc.carn(row = i, col = j, data = snake.matrix, info = info2))
#  }
#}

#reproduction function
reproduce<-function(row, col, data, info){
  if(data[row, col]=="NA")
    return("NA")
  if(data[row, col]=='')
    return('')
  if(runif(1) > info$repro[data[row, col]]){
    return(data[row, col])
  } else(return(new.loc.carn(row = row, col = col, data = data, info = info)))
}

#Executing the function
#for(i in 1:nrow(snake.matrix)){
#  for(j in 1:ncol(snake.matrix)){
#    if(is.na(snake.matrix[i, j]))
#      return(NA)
#    if(snake.matrix[i, j] == "")
#      return("")
#    if(snake.matrix[i, j] <= 5)   #call herbivore somehow (choose diff number range? diff form of labeling?)
#      return(as.numeric(snake.matrix[i, j])-1)
#    if(snake.matrix[i, j] == 0)
#      return("")
#    if(runif(1) < info2$harvest){
#      return(snake.matrix[i,j]<-info2$sated)
#    }else(return(snake.matrix[i,j]))
#    if(snake.matrix[i,j] == info2$sated){
#      return(reproduce(row=i, col=j, data = snake.matrix, info = info2))
#    }else(return(snake.matrix[i,j]))
#  }
#}

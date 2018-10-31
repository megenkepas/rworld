#snake2 -movement proportional to temp each individual has a # and can't leave until 
#it hits a certain number 

#what I aim to do here is create a matrix of snakes already on the landscape. 

#before each snake moves it needs to check to see that the cells around it are warm enough.
#or if the cells are waterlogged 

#for this I am going to say every cell over 70 degrees is ok (our snake is a python or something)

#if the landscape temperature changes (ie.. the cell gets too cold in the next timestep) 
#the snake should retreat toward its den (the cell it started in)


setup.snakey <- function(snake.matrix, heat.matrix){
  if(dim(snake.matrix) != dim(heat.matrix))
    stop("matrices need to have the same dimensions")
  movement <- setNames(movement, names)
  return(list(movement=movement, names=names))
}
 
#SNAKE STARTING POSITION MATRIX 
#I don't want a whole "ton of snakes to randomly popluate the matrix as we did with plants
#I need 2 snakes to individually start in a single cell ("sn1" and "sn2") then attempt to move
#around. I don't want duplicate "sn1's" and "sn2's"!!!!


snake.matrix <-  matrix(sample(c(NA, ""),8^2,replace=TRUE), nrow = 8, ncol = 8)
snake.matrix[sample(1:64, 1)] <- "sn1"
snake.matrix[sample(1:64, 1)] <- "sn2"

snake.matrix


#HEAT MATRIX 
heat.matrix <- matrix(sample(c("55":"92"), 8^2,replace=TRUE), nrow = 8, ncol = 8)
heat.matrix

#FUNCTION FOR MOVEMENT ACCORDING TO THE HEAT GRADIENT
#I first need to check if the cell is blank, contains water, or contains a snake 

movement<- function(snake.matrix, heat.matrix){
  if(cell == ""){
    return("")
  if(is.na(cell))
      return(NA) 
  }else{
  #write a for loop that goes through the matrices 
    for(i in heat.matrix[i,i])
      runif(70:100, min = 70, max = 100)
#If snake 1 or snake 2 is in the cell we need to see if it can move to the next cell
#In order to do that we need to check if the cell is warm enough (70 or higher)
  if(cell == "sn1" | "sn2"){
    where.to.go <- as.matrix(expand.grid(row+c(-1, 0, 1), col+c(-1, 0, 1)))
    #Now check if the cell has water, and is the right temp
    if(is.na(cell))
      stop("cell is waterlogged")
    if(heat.matrix(cell) < 70)
      stop("cell is too cold")
    }else{
      return(cell)
    }
  }
}
?runif
#I need a bit of code that says "don't check the cell the snake is in before trying to move"

timestep <- function(snake.matrix, heat.matrix, movement){
  # Loop over the rows
  for(i in 1:nrow(snake.matrix)){
    for(j in 1:ncol(snake.matrix)){
      snake.matrix[i,j] <- movement(snake.matrix[i,j])
    }
  }
  return(snake.matrix)
}


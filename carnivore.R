##Group Work##
##Scenario: Snakes, which must thermoregulate across the landscape, eat small herbivores (i.e., rabbits).
## The plants that make up the diet of those herbivores must survive and reproduce, as do the herbivores
## which act as prey to the surviving and reproducing snakes across a multi-temperature terrain.
##TEAM
##Hailey Wayment: Herbivores
##Megen Kepas: Snakes - temperature regulation and movement
##Helen Plylar: Terrain - with a temperature variable
##Stephanie Landry: Snakes - as carnivores

#### CARNIVORE: Snake ####

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
info2<-setup.carnivore(eat = seq(0,10,1), repro = c(0.8), harvest = c(0.6))

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
info.herb<-setup.herbivore(eat = seq(0,5,1), repro = c(0.7), kill = c(0.6))

#from Megen's matrix
snake.matrix <-  matrix(sample(c(NA, ""),8^2,replace=TRUE), nrow = 8, ncol = 8)
snake.matrix[sample(1:64, 2)] <- info2$sated
snake.matrix[sample(1:64, 4)] <- info.herb$sated    ##adding herbivores to the landscape
snake.matrix


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
#Not sure how to return the data[x,y] to fill the x,y space rather than the i,j space
#the x<-x-1 isn't working either

for(i in 1:nrow(snake.matrix)){
  for(j in 1:ncol(snake.matrix)){
    if(is.na(snake.matrix[i, j]))
      return(NA)
    if(snake.matrix[i, j] == "")
      return("")
    if(snake.matrix[i, j] == 0)
      return("")
    if(snake.matrix[i, j] == class(numeric))
      return(new.loc.carn(row = i, col = j, data = snake.matrix, info = info2))
  }
}

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

for(i in 1:nrow(snake.matrix)){
  for(j in 1:ncol(snake.matrix)){
    if(is.na(snake.matrix[i, j]))
      return(NA)
    if(snake.matrix[i, j] == "")
      return("")
    if(snake.matrix[i, j] <= 5)   #call herbivore somehow (choose diff number range? diff form of labeling?)
      return(as.numeric(snake.matrix[i, j])-1)
    if(snake.matrix[i, j] == 0)
      return("")
    if(runif(1) < info2$harvest){
      return(snake.matrix[i,j]<-info2$sated)
    }else(return(snake.matrix[i,j]))
    if(snake.matrix[i,j] == info2$sated){
      return(reproduce(row=i, col=j, data = snake.matrix, info = info2))
    }else(return(snake.matrix[i,j]))
  }
}
snake.matrix

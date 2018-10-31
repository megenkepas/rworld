#' Make an elevational grid with optional lakes
#'
#' A light wrapper around \code{diamond.square}
#' @param n indicates the size of the matrix, which is 2^n+1,
#' and produces a square matrix with an odd numbers of rows
#' and columns (default n=6, a 65x65 matrix)
#' @param sd is a component of rnorm, and is used to generate noise; 
#' a value >0 is required (default sd=10)
#' @param lakes logical, used to determine whether or not values <0
#' should be considered "below water"; if "lakes=TRUE", a value of NA is assigned
#' to the cell, indicating that the cell is underwater.
#' @return a terrain matrix; numeric elements indicate heights, and
#' NAs indicate cells filled with water
#' @examples
#' terra.incognita <- wrap.it.up(6, 10, lakes=TRUE)
#' image(terra.incognita)
#' 
wrap.it.up<-function(n, sd, lakes){ #this wrapper wraps up all of the code generated
make.matrix <- function(n, sd){
  if(is.null(n))
    n<-4
  if(is.null(sd))
    sd<-10
#provides the dimensions for the terrain matrix (2^n+1)
  terr.dim<-2^n+1
  terrain <- matrix(nrow=terr.dim, ncol=terr.dim)
  #subset the corners, and assign them random numbers using rnorm (noise is generated based on value of sd).
  terrain[1,1] <- rnorm(1, 0, sd) #bottom left corner
  terrain[nrow(terrain), ncol(terrain)]<- rnorm(1, 0, sd) #top right corner
  terrain[nrow(terrain), 1]<-rnorm(1, 0, sd) #bottom right corner
  terrain[1, ncol(terrain)]<-rnorm(1, 0, sd) #top left corner
 return(terrain)} 
#maketerrain1<-make.matrix(6, 10)
#image(maketerrain1)  

  # Define the functions we'll use
  diamond.step <- function(terrain, sd){
    #identify the location of each corner
  left.b<-terrain[1,1] #bottom left corner
  left.t<-terrain[1, ncol(terrain)] #top left corner
  right.t<-terrain[nrow(terrain), ncol(terrain)] #top right corner
  right.b<-terrain[nrow(terrain), 1] #bottom right corner
  #calculate the average of all of the corners together, and add noise 
  average<-mean(c(left.b, left.t, right.t, right.b)) + rnorm(1, 0, sd)
  #identify the center of the matrix and set equal to the average of the corners
  terrain[(((nrow(terrain)-1)/2)+1), (((ncol(terrain)-1)/2)+1)]<-average
    return(terrain)
  }
#diamondstep1<-diamond.step(maketerrain1, 10)
#image(diamondstep1)

  square.step <- function(terrain, sd){
    ##Define the corners
    left.b<-terrain[1,1] #bottom left corner
    left.t<-terrain[1, ncol(terrain)] #top left corner
    right.t<-terrain[nrow(terrain), ncol(terrain)] #top right corner
    right.b<-terrain[nrow(terrain), 1] #bottom right corner
    # Find the center of the top edge of the matrix 
    ##So need to find the equation to describe the center of a row, and the center of a column
    cc<-(((ncol(terrain)-1)/2)+1)    #column center
    rc<-(((nrow(terrain)-1)/2)+1)    #row center
    ctr<-terrain[(((nrow(terrain)-1)/2)+1), (((ncol(terrain)-1)/2)+1)] #center of the matrix
    # Set that center to be the average of the:top-left, top-right, and center cells
    terrain[1, cc]<-mean(c(left.t, right.t, ctr)) + rnorm(1, 0, sd) #top edge
    # ...do it all again for the bottom, left, and right edges
    terrain[nrow(terrain), cc]<-mean(c(left.b, right.b, ctr)) + rnorm(1, 0, sd) #bottom edge
    terrain[rc, 1]<-mean(c(left.t, left.b, ctr)) + rnorm(1, 0, sd) #left edge
    terrain[rc, ncol(terrain)]<-mean(c(right.t, right.b, ctr)) + rnorm(1, 0, sd) #right edge
    return(terrain)
  }
  #squarestep1<-square.step(diamondstep1, 10)
  #image(squarestep1)
  
#Diamond square step; ties the diamond step and square step functions together
  diamond.square.step<-function(n, sd){
  terrain<-make.terrain(n, sd)
  terr.dim<-2^n+1 #dimensions; length of each side of matrix; number of rows and number of columns
  for (i in 2^(n:1)){ 
    #loops through all of the subsets of the matrix
    for(row in seq(1, terr.dim-1, by=i)){ #loop over rows
      for (col in seq(1, terr.dim-1, by=i)){ #loop over columns
        terrain[row:(row+i), col:(col+i)] <- diamond.step(terrain[row:(row+i), col:(col+i)], sd) #applies diamond step over matrix
        terrain[row:(row+i), col:(col+i)] <- square.step(terrain[row:(row+i), col:(col+i)], sd) #applies square step over matrix
      
        }
    }
  }
  return(terrain)}
  
#dss2<-diamond.square.step(n=6, sd=10)
#image(dss2)

terrain<-diamond.square.step(n=6, sd=10)
if(lakes==TRUE){ #are there lakes? default is no, but if lakes=TRUE, water added to matrix
  terrain[terrain<0]<-NA #..this indicates whether or not a cell is below water; below water if <0

return(terrain)
}}

terra.incognita<-wrap.it.up(6, 10, lakes=TRUE)
terra.incognita
image(terra.incognita)

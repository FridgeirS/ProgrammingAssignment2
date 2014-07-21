## Put comments here that give an overall description of what your
## functions do
# These functions are designed to store and recall a matrix and its inverse to or from memory

## Write a short comment describing this function
# For this assignment I used similar or same names as in the provided example functions
# makeCacheMatrix lets me store the inverse of a matrix object in memory
# set stores the matrix in memory (cache) while get recovers it
# setInverse and getInverse do similar things with the inverse of the original matrix.
  makeCacheMatrix <- function(x = matrix()){
      m <- NULL
      set <- function(y){
        x <<- y
        m <<- NULL
        }
      get <- function() x
      setInverse <- function(solve) m<<- solve
      getInverse <- function() m
      list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
    }




## Write a short comment describing this function
# cacheSolve is a function that takes a matrix created by the makeCacheMatrix function and determines the inverse matrix of it but what makes it so slick is that it first checks if it has been done before, if so it recalls the relevant infromation from memory, if not it goes through the calculations and then stores it in memory.
  cacheSolve <- function(x, ...) {
        m <- x$getInverse() 
      if(!is.null(m)){ 
        message("retrieving data from memory") # this message lets you know that the function found info in memory and is getting it
        return(m) # this is to return said info
        }
      #This part is applicable if the function doesn't find any data in memory
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
    }
## The following two functions makes it possible to cache the result of a 
## calculated inverted matrix

## makeCacheMatrix takes an invertible matrix as its input, and creates
## a list of functions for that matrix

makeCacheMatrix <- function(x = matrix()) {
  
  #inverse matrix = im
  im <- NULL
  
  ##  set() : sets the input matrix and clears any pre-existing inverse matrix
  set <- function(y){
    x <<- y
    im <<- NULL
  }
  
  ##  get() : returns the matrix from set()
  get <- function() x 
    
  
  ##  setinverse() : set the input matrix to its inverse and cache it in the 
  ##  varialbe im
  setinverse <- function(z) im <<- z
  
  ##  getinverse() : returns the inverse matrix from setinverse()
  getinverse <- function() im
  
  ##  The functions are stored in a list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## cacheSolve takes a function of an invertible matrix x as input 
## (in this particular case assignment, tailored for 'makeCacheMatrix')
## and returns a matrix that is the inverse of 'x'
## it first checks if the inverse has already been computed and cached,
## if it has, it stops and prints the cached matrix
## if it hasn't, it computes the inverse, caches it, and prints the result

cacheSolve <- function(x, ...) {
  
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
   
}


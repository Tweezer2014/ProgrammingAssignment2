## There are 2 functions here:
## makeCacheMatrix will create a matrix object that can cache its inverse
## CacheSolve will compute the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed)
## the the cacheSolve function will retrieve the inverse from the cache and
## print "getting cached data".
## If it has not already calculated the inverse it will calculate it and
## print "calculating inverse"
## Sample inputs are:
## mp<-makeCacheMatrix(2:2)   must be a square matrix
## cacheSolve(mp)


## makeCacheMatrix will create a matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x  ## get the value of the matrix
  
  ## set the inverse of the matrix
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  
  list(set = set, get = get,   ## get the inverse of the matrix
       setinverse = setinverse,
       getinverse = getinverse)
}


## CacheSolve will compute the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed)
## the the cacheSolve function will retrieve the inverse from the cache and
## print "getting cached data".
## If it has not already calculated the inverse it will calculate it and
## print "calculating inverse"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse of the matrix        
  m <- x$getinverse()
  
  ## check if there is the matrix   
  if(!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  ## if not: get the inverse of the matrix   
	print("calculating inverse")
  	data <- x$get()
  	m <- solve(data, ...)
  
  x$setinverse(m)   ## set the inverse of the matrix 
  m
}


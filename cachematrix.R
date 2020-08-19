## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function takes a matrix as an argument and creates functions set, get,
## setinvmatrix and getinvmatrix for that matrix variable and return list of funtions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinvmatrix <- function(inverse) m <<- inverse
  getinvmatrix <- function() m
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
  
}


## cacheSolve take an argument which is the return value of makeCacheMatrix function and computes inverse
## of the given matrix and return the inverse of the matrix

cacheSolve <- function(x, ...) {
  m <- x$getinvmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinvmatrix(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
ma <- matrix(rnorm(16),4,4)
cacheSolve(makeCacheMatrix(ma))
##solve(ma) 

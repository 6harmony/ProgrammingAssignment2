
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. The pair of functions below cache and computes the inverse 
## of a matrix:
## 
## 1.  makeCacheMatrix : This function creates a special "matrix" object 
##     that can cache its inverse.
##
## 2.  cacheSolve : This function computes the inverse of the special 
##     "matrix" returned by  makeCacheMatrix  above. If the inverse has 
##     already been calculated (and the matrix has not changed), then
##     cacheSolve  should retrieve the inverse from the cache.


## makeCacheMatrix creates a special "matrix", which is really a list 
## containing a function to:
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
  
}


## cacheSolve calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data and sets the
## value of the inverse in the cache via the  setmatrix  function.

cacheSolve <- function(x, ...) {
        
    ## Return a matrix that is the inverse of 'x'
  
    m <- x$getmatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setmatrix(m)
    m
  
}



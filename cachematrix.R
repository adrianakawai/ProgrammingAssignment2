##
## The makeCacheMatrix creates a special "matrix" object which is a 
## list containing functions to:
## 1. set the value of the matrix (set function)
## 2. get the value of the matrix (get function)
## 3. set the value of the inverse (setinverse)
## 4. get the value of the inverse (getinverse)
## The argument x should be an invertible matrix
## The function returns the special "matrix" object
##
makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize the inv variable to return the inverse of the matrix x
    inv <- NULL
    
    ## store the value of a new matrix and initialize the inverse 
    ## the <<- operator is used to assign a value to the objects x and inv
    ## in an environment that is different from the current environment
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## retrieve the value of the matrix
    get <- function() x
    
    ## assuming that solve funtion in R calculates the inverse of a matrix,
    ## this function stores the result of solve funtion to inv variable
    ## assume that the matrix supplied is always invertible
    setinverse <- function(solve) inv <<- solve
    
    ## retrieve the inverse of the matrix
    getinverse <- function() inv
    
    ## define a list of functions for the special object: 
    ## 1. set, 2. get, 3. setinverse, 4. getinverse
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
    
}

##
## The cacheSolve function calculates the inverse of a matrix.
## The x argument should be the special matrix object returned by the
## function makeCacheMatrix. If the inverse matrix has been calculated 
## before, the inverse is retrieved using getinverse function and the
## computation is skipped. Otherwise, the solve function is called
## to calculate the inversion. The result is cached using setinverse
## funtion
##
cacheSolve <- function(x, ...) {
    
    ## retrieve a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    ## check if the inverse of the matrix has been calculated before (it is cached)
    ## if so, return the inverse of the matrix
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## if the inverse matrix is not cached, calculate it below
    
    ## get the matrix to invert
    data <- x$get()
    
    ## call solve R function to calculate the inverse of the matrix
    inv <- solve(data, ...)
    
    ## cache the inverse matrix using setinverse function
    x$setinverse(inv)
    
    ## return the inverse matrix
    inv
}
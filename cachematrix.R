## Contains functions that are able to cache potentially time-consuming computation of matrix inverse
## 

## makeCacheMatrix creates and returns a special "matrix" object that can cache its inverse.
## a special "matrix" object is a list containing the following functions
##   set() - set the matrix data
##   get() - get the matrix data
##   getCachedInverse() - returns cached inverse (if it has been calculated by cacheSolve function)
##   setCachedInverse() - stores calculated inverse in cache

## Usage examples:
##   mymatrix <- matrix(rnorm(100), nrow=10, ncol = 10) ## random 10x10 matrix 
##
##   myobj <- makeCacheMatrix(mymatrix)         
## or
##   myobj <- makeCacheMatrix()
##   myobj$set(mymatrix)


makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y           # store matrix data
		inverse <<- NULL  # clear cached inverse because matrix is changed
	}
	get <- function() x
	setCachedInverse <- function(inv) inverse <<- inv
	getCachedInverse <- function() inverse
	list(set = set, get = get, 
		setCachedInverse = setCachedInverse,
		getCachedInverse = getCachedInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then 
##  the cacheSolve retrieves the inverse from the cache.

## Usage example:
##   mymatrix <- matrix(rnorm(2000*2000), nrow=2000, ncol = 2000) ## random 2000x2000 matrix 
##   myobj <- makeCacheMatrix(mymatrix)                           ## creates object with inverse cache
##
##   inverse <- cacheSolve(myobj)   # first call (slow) - calculates inverse
## ...
##   inverse2 <- cacheSolve(myobj)  # second call (fast) - returns inverse from cache

cacheSolve <- function(x, ...) {
	inv <- x$getCachedInverse() ## get chached value
	if (!is.null(inv)) {        ## check if inverse has already been cached.  
		message("inverse is returned from cache")
		return (inv)        ## return cached value
	}                     
	matrix <- x$get();          ## get matix data 
	inv <- solve(matrix, ...)   ## calculate inverse
	x$setCachedInverse(inv)     ## store inverse in cache
	inv
}

## These two functions compute the inverse of a matrix and cache it 
## so that the inverse of the same matrix does not need be computed repeatly

## Function makeCacheMatrix takes a matrix as input and defines a list of functions 
## to set/get the matrix and set/get the inverse of the matrix.
## By assigning the input matrix and its inverse to variables in the the parent environment (makeCacheMatrix)
## of the set/get and setinverse/getinverse functions the caching is realized.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function () i
	list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve take a list of functions returned from makeCacheMatrix and compute the inverse of the matrix if it is new
## or returns the inverse directly if it is already cached

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}

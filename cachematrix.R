## These two functions are almost identical to what's in the example of calculating
## mean of a vector and save the result

## The functions take an invertible matrix (there's no check in place to ensure the 
## matrix provided is invertible) and do the inversion, using R solve() function, and 
## save the result in a different environment as cache, for future references

## to call it, use 
## source("cachematrix.R")
## a<-matrix(c(1,2,-2,1),2,2)
## cacheSolve(makeCacheMatrix(a))
## while a is an invertible matrix

## Create a list of 4 functions, set, get, setsolve, getsolve

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
			x <<- y
			m <<- NULL
	}
	get <- function() x
	## store in a different environment
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
		 setsolve = setsolve,
		 getsolve = getsolve)
}


## inverse the matrix. If the inversion exists, use it, to save computation time

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x', that's cached
	m <- x$getsolve()
	if(!is.null(m)) {
			message("getting cached data")
			return(m)
	}
	## if not exists, inverse
	data <- x$get()
	m <- solve(data, ...)
	## save the inversion of 'x'
	x$setsolve(m)
	m
}

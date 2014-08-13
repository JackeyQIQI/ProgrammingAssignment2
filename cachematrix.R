## Matrix inversion is usually a costly computation and
## there may be some benefit to caching the inverse of
## a matrix rather than compute it repeatedly.


## This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL

	##set the value of the matrix, 
	##and set the inverse matrix as NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	##get the value of the matrix
	get <- function() x

	##set the value of the inverse matrix
	setinverse <- function(inverse) m <<- inverse

	##get the value of the inverse matrix
	getinverse <- function() m

	##return the list
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the
## special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated
## (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

	##get the value of the inverse matrix
	m <- x$getinverse()

	##check the value of the inverse matrix,
	##if it has already been calculated, then 
	##it get the value from the cache
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}

	##get the value of the matrix, calculate 
	##the inverse matrix and set the value of
	##the inverse matrix in the cache,
	##then return the inverse matrix
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m          
}



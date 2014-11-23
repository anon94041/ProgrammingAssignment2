## The two functions below (ultimately) provide an inverse of an
## assumed invertible matrix.  The inverse is only calculated once
## and then cached, for later retrieval if desired, in a special
## "matrix" which contains functions to set and get the matrix and
## its inverse.  To use, run makeCacheMatrix first on an original matrix,
## then call cacheSolve with the result returned by makeCacheMatrix.

## makeCacheMatrix() creates a special "matrix" which is really a list
## containing a function to set the value of a matrix, get the value of
## the matrix, set the value of the matrix inverse, and get the value
## of the matrix inverse.  It will save time if the matrix must be
## inverted repeatedly because it will only calculate the inverse the
## first time, and cache it for later use.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## cacheSolve() computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), cacheSolve will retrieve and
## return the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if (!is.null(inv)) {	# if inverse already cached
		message("getting cached data")
		return(inv)
	}
	data <- x$get()		# if not, compute inverse
	inv <- solve(data,...)
	x$setinv(inv)
	inv
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function set and get matrix and set and get inverse of this matrix

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	set_inverse <- function(x) inverse <<- x
	get_inverse <- function() inverse
	list( set = set, 
				get = get,
				set_inverse = set_inverse,
				get_inverse = get_inverse)
}


## Write a short comment describing this function
## This function check if matrix has cached inverse. If not it solve this inverse. At the end it return inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$get_inverse()
	if(!is.null(inverse)) {
		message("getting cached data.")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data)
	x$set_inverse(inverse)
	return(inverse)
}

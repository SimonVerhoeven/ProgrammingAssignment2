## R file useful for getting the inverse of a matrix.
## it returns a cached copy if available
##
## sample usage:
## cachedMatrix <- makeCacheMatrix(matrix(rnorm(25), nrow = 5, ncol = 5))
## cachedMatrix$get()
## cacheSolve(cachedMatrix) (needs to be called twice if you want to get a cached copy)


## makeCacheMatrix: returns a list of available functions:
## - Getter function for the inverse
## - Setter function for the inverse
## - Setter function for the inverse
## - Getter function for the inverse
makeCacheMatrix <- function(x = matrix()) {
	#holder object for the inverse
	inv <- NULL

	#Setter function for thr matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	#Getter function for the matrix
	get <- function() {
		x
	}

	#Setter function for the inverse
	setmatrix <- function(solve) {
		inv <<- solve
	}

	#Getter function for the inverse
	getmatrix <- function() {
		inv
	}

	#Returns a matrix with our defined functions
 	list(set = set, 
		get = get,
		setmatrix = setmatrix,
		getmatrix = getmatrix)
}


## cacheSolve: Gives us the inverse of the passed matrix.
##	It returns a cache copy if available, and otherwise determines it
cacheSolve <- function(x, ...) {
	# tries to fetch the cached matrix
	inv <- x$getmatrix()
	# if available immediately exits the function and informs us that the cached data was used
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}

	#determine the inverse
	data <- x$get()
	inv <- solve(data, ...)

	#cache it
	x$setmatrix(inv)

	#return the newly cached inverse
	inv
}

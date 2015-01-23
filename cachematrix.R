
## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	myMatrix <- NULL
	set <- function(y) {
		x <<- y
		myMatrix <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) myMatrix <<- solve
	getinverse <- function() myMatrix
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## calculates the inverse of the matrix created with the above function

cacheSolve <- function(x, ...) {
	myMatrix <- x$getinverse()
	##check to see if the inverse has already been compute
	if(!is.null(myMatrix)) {
		return(myMatrix)  #getting cached data & skip computation
	}
	data <- x$get()
	myMatrix <- solve(data, ...) #otherwise compute the inverse
	x$setinverse(myMatrix) # and "cache" it
	myMatrix
}

# The function try to create a class which contains original and 
# cached inverse matrix

# makeCacheMatrix create a class with
# four methods: get/set/setinverse/getinverse
# use varname <- makeCacheMatrix(amatrix) to assign the class

makeCacheMatrix <- function(x = matrix()) {
	I <- NULL
	set <- function(y) {
		x <<- y
		I <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) I <<- solve
	getinverse <- function() I
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve try to cache the inverse of the matrix
## it will give the inverse matrix by using setinverse
## and return the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
	        message("getting cached data")
	        return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}

# Example
# creation of a matrix for test purpose
m <- matrix(rnorm(16), nrow=4, ncol=4)
myCachableMatrix <- makeCacheMatrix(m)
cacheSolve(myCachableMatrix)
myCachableMatrix$get()
myCachableMatrix$getinverse()
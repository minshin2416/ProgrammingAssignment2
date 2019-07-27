## Mincheol Shin

## This function sets and gets the value of the matrix (x in this case), then sets and gets the value of its inverse (a in this case)
makeCacheMatrix <- function(x = matrix()) {
	a <- NULL
	
	set <- function(b) {
		x <<- b
		a <<- NULL
	}
	
	setinverse <- function(inverse) {a <<- inverse}
	
	get <- function() {x}
	
	getinverse <- function() {a}
	
	list(set = set, setinverse = setinverse, get = get, getinverse = getinverse)
}


## This function solves the inverse of the matrix given above. If function is added to obtain the inverse from the cache when the inverse has already been calculated.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	a <- x$getinverse()
        
	if(is.null(a) == FALSE) {
		return(a)
	}
        
	D <-x$get()
        
	a <- solve(D,...)
        
	x$setinverse(a)
        
	print(a)
}

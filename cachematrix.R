## Create and invert CacheMatrices. These are special psuedo matrices
## (really closures) That hold the a matrix and either its inverse or
## `NULL`.  A CacheMatrix is created from a matrix using `aCacheMatrix =
## makeCacheMatrix(aMatrix)`. The matrix and its inverse can be queried
## using `aCacheMatrix$get()` and`aCacheMatrix$getInverse()`.  These values
## can also be set, although I'm not sure that's a good idea outside of
## cacheSolve,  using `set(aMatrix)` and `setInverse(anInverse)`.
##
## The function cacheSolve takes a cacheMatrix and returns it's inverse.
## If the inverse has been computed previously, it is retrieved from the
## cacheMatrix and returned. If not is is computed, stored in the original
## cacheMatrix and then returned. If cacheSolve is invoked with `debug=TRUE`,
## it will emit a message when retrieving cached data. 
##
## cacheSolve makes at least two, perhaps dubious, assumptions. Among them:
## * matrix is invertble
## * any optional arguments are the same between the calls
##
## Example Usage:
## > m = array(1:4, c(2,2))
## > m = array(1:4, c(2,2))
## > cm = makeCacheMatrix(m)
## > cacheSolve(cm, debug=TRUE)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(cm, debug=TRUE)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > m2 = array(7:10, c(2,2))
## > cm$set(m2)
## > cacheSolve(cm, debug=T)   # Note that cache was correctly cleared here
##      [,1] [,2]
## [1,]   -5  4.5
## [2,]    4 -3.5
## > cacheSolve(cm, debug=T)
## getting cached data
##      [,1] [,2]
## [1,]   -5  4.5
## [2,]    4 -3.5
## > solve(m2)
##      [,1] [,2]
## [1,]   -5  4.5
## [2,]    4 -3.5
## 
## > cm$get() %*% cacheSolve(cm) 
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1


## Make special psuedo matrix that can hold both the original matrix and a cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
			x <<- y
			inv <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function() {inv}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return a new, or cached inverse to the psuedo-matrix `x`. Psuedo matrices are computed
## using the above `makeCacheMatrix`.

cacheSolve <- function(x, ..., debug=FALSE) {
	inv <- x$getinverse()
	if (!is.null(inv)) {
		if(debug) {
		    message("getting cached data")
		}
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}


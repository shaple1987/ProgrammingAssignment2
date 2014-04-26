## The pair of "makeCacheMatrix" and "cacheSolve" functions below caches the computation result of the inverse of a matrix to avoid unnecessary re-computations. 

## "makeCacheMatrix" creates a special "matrix", which is a list containing a function to
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the inverse of the matrix
## (4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL  # When the matrix is reset, the cached inverse matrix is set to NULL so that it will be re-computed.
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## "cacheSolve" computes and returns the inverse of an matrix if it has not been previously computed.  After the computation, the inverse matrix will be cached.
## If the inverse matrix has already been previously computed and cached, cacheSolve retrieves the cached inverse matrix and returns it.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
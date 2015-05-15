## The following two R functions are used to cache matrix inverse
## which is potentially time-consuming computation. For example,
## taking the inverse of a matrix is typically a fast operation.
## However, for a very large matrix, it may take too long to compute
## the inverse, especially if it has to be computed repeatedly (e.g.
## in a loop). If the contents of a matrix are not changing, it may
## make sense to cache the value of the inverse so that when we need
## it again, it can be looked up in the cache rather than recomputed.
##
## The two R functions can be used as following:
##   > m <- makeCacheMatrix(matrix(c(1,3,2,4,1,2,6,4,4), 3, 3))
##   > cacheSolve(m)

## makeCacheMatrix function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

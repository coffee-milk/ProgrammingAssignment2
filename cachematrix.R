##Matrix inversion is usually a costly computation and there may be some 
##benefit to caching the inverse of a matrix rather than compute it repeatedly.
##This pair of functions compute and cache the inverse of a matrix.


##The first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    } 
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         getinverse = getinverse, 
         setinverse = setinverse)
} 


##The second function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above, and retrieve the inverse from the cache if the inverse 
##has already been calculated (and the matrix has not changed).

cacheSolve <- function(x, ...) {
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
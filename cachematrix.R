## This 2 functions cache and calculate the inverse of given matrix

## "makeCacheMatrix" creates a special matrix object that can cache its inverse
## set function - sets the value of the matrix
## get function - gets the value of matrix
## setinverse -  sets the inverse matrix
## getinverse - gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Function cacheSolve calculates the inverse of the matrix returned by makeCacheMatrix
## If cash is already calculated it gets the inverse from the cache
## If no, it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
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

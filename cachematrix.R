## mackCacheMatrix creates a matrix and stores 'special' functions in a list for reference later
## cacheSolve grabs the 'cached' matrix if not null and if null, calculates the inverse

## makeCacheMatrix creates 4 functions and stores them in a list

makeCacheMatrix <- function(x = matrix()) {
    invert_x <- NULL
    set <- function(y) {
        x <<- y
        invert_x <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) invert_x <<- solve
    getinverse <- function() invert_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve checks if the inverse is already calculated and if not, calculates it

cacheSolve <- function(x, ...) {
    invert_x <- x$getinverse()
    if(!is.null(invert_x)) {
        message("getting cached data")
        return(invert_x)
    }
    data <- x$get()
    invert_x <- solve(data)
    x$setinverse(invert_x)
    invert_x
}

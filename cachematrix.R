## Functions that cache a matrix and its inverse and avoid unnecessary
## inverse calculation (calculate the same matrix inverse two times)

## Store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    setinv <- function(inverse) {inv <<- inverse}
    
    get <- function() x
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function calculates the inverse if it isn't calculate yet

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    if(!is.null(inv)) {
        print("Getting cache data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}

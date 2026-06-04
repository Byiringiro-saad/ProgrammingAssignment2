## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes (or retrieves from cache) the inverse of that object.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y) {
        x   <<- y
        inv <<- NULL
    }

    get <- function() x

    setInverse <- function(inverse) inv <<- inverse

    getInverse <- function() inv

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns the inverse of the special matrix created by makeCacheMatrix.
## Uses the cached value if available; otherwise computes and caches it.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()

    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    inv <- solve(x$get(), ...)
    x$setInverse(inv)
    inv
}

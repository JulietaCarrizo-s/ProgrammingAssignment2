## These two functions compute the inverse of a matrix and cache it, so the inverse is not recalculated if the matrix has not changed

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse. It returns a list of four functions:
##   set - stores a new matrix and clears the cached inverse
##   get - returns the matrix
##   setinverse - stores the inverse in the cache
##   getinverse - returns the cached inverse (NULL if not yet computed)
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() x

        setinverse <- function(inverse) inv <<- inverse

        getinverse <- function() inv

        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), it retrieves the inverse 
## from the cache and prints "getting cached data". Otherwise, it computes the inverse with
## solve(), stores it in the cache, and returns it.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()

        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

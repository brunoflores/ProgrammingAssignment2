## R functions to cache a matrix inversion (a potentially time-consuming computation).
## It assumes that the matrix supplied is always invertible.
## "solve" function is used to compute the matrix inversion.

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(
                set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## Computes and returns the inverse of the special "matrix" returned by "makeCacheMatrix".
cacheSolve <- function(x) {
        m <- x$getinverse()
        if( ! is.null(m)) {
                message('getting cached data')
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

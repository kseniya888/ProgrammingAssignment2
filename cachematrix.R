## Computing and storing the inverse of a square matrix

## Creates a matrix that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes an invesrse of a matrix and caches the result
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        matrix <- x$get()
        return(i)
    }
    matrix <- x$get()
    i <- solve(matrix, ...)
    x$setinverse(i)
    i
}

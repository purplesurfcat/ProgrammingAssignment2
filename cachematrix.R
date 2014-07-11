## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        matinverse <- NULL
        set <- function(y) {
                x <<- y
                matinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) matinverse <<- solve
        getinverse <- function() matinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x[getinverse()]
        if(!is.null(matinverse)) {
                message("getting cached data")
                return(matinverse)
        }
        data <- x[get()]
        matinverse <- solve(data, ...)
        x[setinverse(matinverse)]
        matinverse
}

## These functions allow a matrix and it's matrix inverse to be cached
## so that the inverse does not need to be calculated more than once.
## For these functison, the input matrix is assumed to be invertible.

## makeCachMatrix creates a special matrix that allows it to cache its inverse,
## and allows retrieval of that cached value.

makeCacheMatrix <- function(x = matrix()) {
        matinverse <- NULL
        
        ##create set function that sets input matrix y as the cached matrix x
        ##sets matinverse to null since inverse has not been calculated for
        ##new matrix
        set <- function(y) {
                x <<- y
                matinverse <<- NULL
        }
        
        ##create get function that returns cached matrix x
        get <- function() x
        
        ##create setinverse function that solves inverse of cached
        ##matrix x and caches the inverse in matinverse
        setinverse <- function(x) matinverse <<- solve(x)
        
        ##create getinverse function that retrieves cached matinverse value
        getinverse <- function() matinverse
        
        ##return a list of these four functions that operate on the 
        ##"special" matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix above. For a new matrix, cachesolve caches the inverse;
## for an existing cached matrix, it returns the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ##set matinverse to matrix inverse of x -- using solve via getinverse
        ##function defined in makeCacheMatrix
        matinverse <- x$getinverse()
        
        ##if matinverse is not null, that means the inverse is cached
        ##thus retrieve the matinverse value and return it
        if(!is.null(matinverse)) {
                message("getting cached data")
                return(matinverse)
        }
        
        ##get the cached matrix defined in makeCacheMatrix
        temp_matrix <- x$get()
        
        ##set matinverse to matrix inverse of cached matrix using solve
        matinverse <- solve(temp_matrix, ...)
        
        ##set the cached matrix inverse using setinverse function
        x$setinverse(matinverse)
        
        ##return matinverse
        matinverse
}

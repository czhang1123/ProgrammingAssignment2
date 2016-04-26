## Matrix inversion is a costly computation. The functions below can cache the inverse of a matrix

## Example of use: 
##	mx <- makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
##	cacheSolve(mx) 
## Note: 
##	first time run cacheSolve(mx) will compute and show the inverse of the matrix, and cache it
##	second time run will show the cached inverse


## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inv) m <<- inv
        
        getinv <- function() m
        
        ## return a list of functions
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
}

## Function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
        
        ## get cached inverse if existed
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## otherwise, compute the inverse and cache it
        data <- x$get()                                                                              
        m <- solve(data, ...)
        x$setinv(m)
        
        ## return the inverse
        m
}


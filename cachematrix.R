## Matrix inversion can be a costly computation.  There are two functions in the script.
## The first function create a matrix "object" that can cache its inverse.  The second
## function can compute and store the inverse in the object created in the first function.


## This function creates a special "matrix" object that can
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                        x <<- y
                        inv <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) inv <<- inverse
            getinverse <- function() inv
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)  
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix.  If the inverse has already been calculated
## and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) {
            inv <- x$getinverse()
            if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
            }
            data <- x$get()  
            
            ## Check that matrix is square and that it is invertable (even though
            ## this wasn't a requirement). If not square, or not invertable
            ## store NA as inverse.
            inv = NA
            if ((dim(data)[1] == dim(data)[2]) 
                && (det(data) != 0)) {
                        inv <- solve(data, ...)
            } 
            x$setinverse(inv)
            inv
}  


## Function makeCacheMatrix creates a special "matrix" object that can cache
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                     # initialize inverse matrix
        
        set <- function(y){             # function to cache input matrix
		x <<- y                 
		inv <<- NULL            # clear inverse cache as matrix changed
        }
        get <- function() x             # function to get input matrix
        setinv <- function(inverse){    # function to cache inverse matrix
                inv <<- inverse         
        }
        getinv <- function() inv        # function to get inverse matrix
        
        # return the list of functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Function cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated and the matrix has 
## not changed, then the cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        inv = x$getinv()                # try to retrieve cached inverse
        
        if (!is.null(inv)){             # if already cached the return it
                message("getting cached data")
                return(inv)
        }
        inv = solve(x$get(), ...)       # otherwise inverse the matrix 
	x$setinv(inv)                   # cache the inverse for future use

        # return a matrix that is the inverse of 'x'
        inv  
}

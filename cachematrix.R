## These functions handle a matrx object that can cache its inverse.

## This is the constructor function to create the matrix:
## argument is defaulted to null matrix (M=0x0)
makeCacheMatrix <- function(x = matrix(numeric(),0.0)) {
        inv <- NULL                    # no inverse matrix computed yet
        
        # methods for getting/setting the value of the matrix:        
        set <- function(y) {
                x <<- y                # set the matrix to the new value
                inv <<- NULL           # reset the cache (matrix has changed)
        }
        get <- function() x            # return the matrix
        
        # methods for getting/setting the inverse:
        setinv <- function(n_inv) inv <<- n_inv  # set the inverse in the cache
        
        getinv <- function() inv       # get the inverse
        
        # return the vector of methods for this matrix object:        
        list(set = set, get = get, 
             setinv = setinv, getinv = getinv)
}


## This function handles the cache: it either returns the cached inverse
## or computes it and stores it for the next usage. 
## NOTE:this function accepts "extra" further arguments to be passed
## to the function computing the inverse matrix.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()              # get the stored inverse, if any
        if (is.null(inv)) {
                # if there was not inverse matrix stored yet,
                # then compute it now:
                data <- x$get()
                inv <- solve(data, ...)   # use solve() and pass further args
                x$setinv(inv)             # store the inverse in the cache
        }
        else {
                # notify that we're using cached data
                message("getting cached data")
        }
        
        # return the inverse matrix:
        # invisible() is used to avoid useless output (the value of the inverse
        # matrix is supposed to be obtained using the "getinv" method of the
        # makeCacheMatrix object)
        invisible(inv)              
}        

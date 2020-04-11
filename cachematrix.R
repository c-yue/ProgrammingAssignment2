## a pair of functions that cache the inverse of a matrix



## This function creates a special "matrix" object that can cache its inverse.
## it creates a list containing multiple special matrix
## object that allow for:
##      - storing of a matrix
##      - setting of a matrix
##      - caculation & caching of the inverse of the matrix
##      - retrieval of the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix())
{
        v <- NULL
        set <- function(y) 
        {x <<- y
        v <<- NULL }
        get <- function() x
        set_inv_mtrx <- function(inv_mtrx) v <<- solve
        get_inv_mtrx <- function() v
        list(set = set, get = get,
             set_inv_mtrx = set_inv_mtrx,
             get_inv_mtrx = get_inv_mtrx)
}



## The function cacheSolve calculates the inverse of the "matrix"
## object from makeCacheMatrix. If the inverse has already been
## calculated, it returns this value. Otherwise, it operates additional calculation.

cacheSolve <- function(x, ...) {
        v <- x$get_inv_mtrx()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data, ...)
        x$set_inv_mtrx(v)
        v
}
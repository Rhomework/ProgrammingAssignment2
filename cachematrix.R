## functions for computing and storing the inverse of matrices

## makeCacheMatrix:
## takes a matrix (or defaults to an empty matrix) and returns
## a list that represents a special matrix object which can
## store the matrix's inverse in the cache

## cacheSolve:
## takes a special matrix object (see above) and returns the
## inverse of the matrix:
## if the inverse has not been computed before, it computes and
## then stores and returns the inverse
## if the inverse has been computed before, it returns the
## pre-computed inverse

## input:  matrix object (defaults to empty matrix)
## output: special matrix object (in list form) which can store
##         the matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
    # store matrix to the cache and set inv to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # call set(x) to store the original matrix and to set inv to NULL
    set(x)
    
    # get the matrix (from the cache)
    get <- function() x
    
    # store the inverse to the cache
    setinverse <- function(inverse) inv <<- inverse
    
    # get the inverse (from the cache)
    getinverse <- function() inv
    
    # return the special matrix object, i.e. a list of its functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## input:  special matrix object
## output: inverse of the matrix
cacheSolve <- function(x, ...) {
    # try to get the stored inverse of the matrix
    inv <- x$getinverse()
    
    # inverse has been computed before: return it
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    
    # inverse has not been computed before:
    
    # - get the matrix
    mat <- x$get()
    
    # - compute its inverse
    inv <- solve(mat, ...)
    
    # - store the inverse
    x$setinverse(inv)
    
    # - return the inverse
    inv
}
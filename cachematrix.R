# Put comments here that give an overall description of what your
# functions do

# This function creates a special "matrix" object that can
# cache its inverse.

makeCacheMatrix <- function(x = matrix(),...) {
    inversematrix <- NULL
    # set a new matrix
    set <- function(y) {
        x <<- y
        inversematrix <<- NULL
    }
    # get the matrix
    get <- function() x
    
    # set 'inverse' matrix (Note: this is not an actual inverse of x)
    setinverse <- function(inverse = matrix()) inversematrix <<- inverse

    # get inverse of matrix
    getinverse <- function () inversematrix
    
    # return list of functions of x-matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already
# been calculated (and the matrix has not changed), then
# cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Get current inverse of matrix
    inversematrix <- x$getinverse()
    
    # Check if current inverse exists; if so, return it
    if(!is.null(inversematrix)) {
        message("getting cached data")
        return(inversematrix)
    }
    
    # Otherise, compute matrix inverse
    inversematrix <- solve(x$get(), ...)
    
    # Cache matrix inverse
    x$setinverse(inversematrix)
    
    # Return matrix inverse
    inversematrix
}
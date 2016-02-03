## Functions below cache the inverse of a matrix and look it up from cache
## if it exists and if not, compute it and save it to the cache.
## Computing the inverse of a matrix is an expensive operation so it is good
## to cache the results when possible.

## makeCacheMatrix takes as input a matrix and creates a list containing
## functions to:
## -- set the value of the matix
## -- get the value of the matrix
## -- set the inverse of the matrix 
## -- get the inverse of the matrix
## To test, try:
## > a <- matrix(rnorm(100),10,10)
## > b <- cacheMatrix(a)
## > b$get()
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


## Write a short comment describing this function
## cacheSolve takes as input the special "matrix" created by makeCacheMatrix
## function and returns the inverse of the matrix obtained using get function
## defined in makeCacheMatrix.
## First, it checks if inverse exists already and if it does, it returns it.
## If not, it computes it and saves it for future calls.
## To test, assuming you have b <- makeCacheMatrix(a)
## To test, try:
## > a <- matrix(rnorm(100),10,10)
## > b <- cacheMatrix(a)
## > b$get()
## > c <- cacheSolve(b)     # this will compute the inverse since first call
## > d <- cacheSolve(b)     # this will look up inverse from cache and return it
## getting cached data      # output from function call if inverse is in cache
## > c-d                    # should get 10x10 matrix of zeros
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    # assuming input matrix is invertible and square
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
## A pair of functions to cache the inverse of a matrix

## The function makeCacheMatrix creates a special "matrix" object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setcache <- function(solve) i <<- solve
    getcache <- function() i
    list(set = set, get = get,
         setcache = setcache,
         getcache = getcache)  
}


## The function cacheSolve computes the inverse of the special "matrix" created by makeCacheMatrix
## If the inverse has already been computed, cacheSolve will retrieve it from the cache
## If the inverse has not been calculated, cacheSolve will compute the inverse using the solve function and set the inverse in cache

cacheSolve <- function(x, ...) {
    i <- x$getcache()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setcache(i)
    i
}


## The below test matrix test.mat was used to test whether the two functions produced
## an inverse matrix, test.inv, and whether the product of test.mat and test.inv 
## produced the identity matrix

# test.mat <- matrix(c(4,3,3,2), nrow=2)
# test.inv <- cacheSolve(makeCacheMatrix(test.mat))
# test.mat %*% test.inv
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1

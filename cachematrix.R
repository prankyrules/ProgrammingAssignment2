# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# CacheSolve function returns the inverse of the matrix. It first checks whether the inverse
# of matrix has already been calculated, if it is previously calculated then skips the
# computation. If not, it computes the inverse, sets the value in the cache using setinverse 
# function defined in makeCacheatrix function.

cacheSolve <- function(x, ...) {
    #get inverse from makeCacheMatrix created
    inv <- x$getinverse()
    #check whether a inverse matrix is cached
    if(!is.null(inv)) {
        message("getting cached inverse matrix.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    #solve function will return the inverse of matrix
    x$setinverse(inv)
    inv
    
    ## Return a matrix that is the inverse of 
}

#####TESTS AND OUTPUT############
##Created a 2-D matrix
# > x <- matrix(1:4,2,2)
# > x
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > m = makeCacheMatrix(x)
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m)
# getting cached data.
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

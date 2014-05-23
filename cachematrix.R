## These two functions are used to create a special object that stores
## an invertible matrix and caches its inverse matrix.

## makeCacheMatrix function creates a special "matrix" object that can
## cache the inverse Matrix. It is really a list containing a function
## to...
## 1. set the value of the Matrix
## 2. get the value of the Matrix
## 3. set the value of the inverse Matrix
## 4. get the value of the inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinv <- function(inv) invx <<- inv
        getinv <- function() invx
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##  cacheSolve function computes the inverse of the special "matrix" returned
##  by makeCacheMatrix above. If the inverse has already been calculated
##  (and the matrix has not changed), then the cachesolve should retrieve
##  the inverse from the cache.

cacheSolve <- function(x, ...) {
        invx <- x$getinv()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data, ...)
        x$setinv(invx)
        invx
}

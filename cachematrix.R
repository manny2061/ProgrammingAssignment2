## MakeCacheMatrix function is created and takes a matrix as an argument.  Also, sets mean to NULL 
## The set function is created in order for x and m to reset. x and m revert to y and NULL in parent environment.
## get function returns the matrix x
## Setinverse function takes solve function as argument and then stores the result to the m variable in the parent environment.(cached)
## getinverse function retruns m in the parent environment 
## list is created containing all 4 functions.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cachesolve function takes makeCacheMatrix list,x, as an argument.
## m is equal to makeCacheMatrix list,x, and subset by getinverse function which returns m
## if m has a value it will populate a message and return the m value.
## data will take makeCacheMatrix list, x and subset it by function that returns x (original matrix input)
## m will be modified to equal the inverse of the original matrix input.
## makeCacheMatrix list,x, will be subset by setinverse function that takes the inverse matrix and stores it into cache memory.
## m is returned and inverse of matrix is displayed.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


ooo <- matrix(1:4, nrow= 2,ncol = 2) 
ppp <- makeCacheMatrix(matrix(1:4, nrow= 2,ncol = 2))

cacheSolve(ppp) #run twice



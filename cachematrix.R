## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function (y) {
                x<<- y
                minv <<- NULL
        }
        get <- function() x
        setinverse <- function(invesre) minv <<- inverse
        getinverse <- function() minv
        list(set=set, get=get,
             setinvesre=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getinverse
        if (!is.null(minv)){
                message ("getting cached matrix")
                return (minv)
          }
        mtx <- x$get()
        minv <- solve(mtx, ...)
        x$setinverse (minv)
        minv
}

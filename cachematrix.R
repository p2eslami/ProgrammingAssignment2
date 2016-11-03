## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a spaecial matrix than can cache its inverse

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


## cacheSolve function solves for the inverse of the matrix if the information is already NOT available

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

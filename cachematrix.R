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
                
## Testing the Code:
 mat = diag(3,5)
> mat
     [,1] [,2] [,3] [,4] [,5]
[1,]    3    0    0    0    0
[2,]    0    3    0    0    0
[3,]    0    0    3    0    0
[4,]    0    0    0    3    0
[5,]    0    0    0    0    3
> my_mat <- makeCacheMatrix(mat)
> my_mat$get()
     [,1] [,2] [,3] [,4] [,5]
[1,]    3    0    0    0    0
[2,]    0    3    0    0    0
[3,]    0    0    3    0    0
[4,]    0    0    0    3    0
[5,]    0    0    0    0    3
## First time it shows that there is no Cache
 my_mat$getinverse()
NULL
## When solve for the inverse next time using cacheSolve:
> cacheSolve(my_mat)
          [,1]      [,2]      [,3]      [,4]      [,5]
[1,] 0.3333333 0.0000000 0.0000000 0.0000000 0.0000000
[2,] 0.0000000 0.3333333 0.0000000 0.0000000 0.0000000
[3,] 0.0000000 0.0000000 0.3333333 0.0000000 

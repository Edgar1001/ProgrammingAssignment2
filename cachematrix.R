## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y){
                x <<-y
                inv <<-NULL
                
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Compute and cache the inverse of a matrix
## @param x the result of a previous makeCacheMatrix call
## @param ... additional arguments to pass to solve function
## examples
## x = makeCacheMatrix(matrix(rnorm(9), 3, 3))
## cacheSolve(x)

cacheSolve <- function(x, ...) {
        
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }        
        mat <-x$get()
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}


##        > source("makeCacheMatrix.R")
##     > pmatrix <- makeCacheMatrix(matrix(1:16,nrow=4,ncol=4))
##  > pmatrix$get()
##[,1] [,2] [,3] [,4]
## [1,]    1    5    9   13
## [2,]    2    6   10   14
## [3,]    3    7   11   15
## [4,]    4    8   12   16
## > pmatrix$getInverse()
## NULL
## > pmatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))

##> pmatrix$get()
##  [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(pmatrix)
## [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(pmatrix)
##getting cached data
##   [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> pmatrix$getInverse()
## [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

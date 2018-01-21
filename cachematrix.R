## These 2 functions cache the inverse of a matrix as this is typically
## a costly action from computation power perspective. 
## These 2 functions cache it for repeatedly use without computation

## makeCacheMatrix takes a matrix as an input and store its information
## it also creates a list containing a function to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
                inverse <- NULL
                set <- function(y) { 
                        x <<- y
                        inverse <<- NULL
                }
                get <- function() x 
                setinverse <- function(solve) inverse <<- solve
                getinverse <- function () inverse
                
                list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
                
}


## cacheSolve function return the inverse of a matrix.
## it assumes that the input is an invertible matrix.
## input should be a former function type. 
## it checks whether the inverse was previously computed. 
## if not it computes the inverse, sets the value in cache via
## setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)){ 
                message("getting inverse cached data") 
                return(inverse)
        }
        mat <- x$get() 
                inverse <- solve(mat, ...)
        x$setinverse(inverse) 
        inverse
}


## example: 
## > h <- matrix(1:4, 2, 2)
## > h
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## > mat <- makeCacheMatrix(h)

## no cache: 

## > cacheSolve(mat)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## second time

## > cacheSolve(mat)
## getting inverse cache data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

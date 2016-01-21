## The idea is to find the inverse of a matrix and store it in a cache
## so that it can be retrieved if required later. This will reduce the
## computational cost of having to repeatedly compute the inverse - which
## can be significant in the case of large matrices.
## This module contains two functions: makeCacheMatrix and cacheSolve.


## makeCacheMatrix returns a list of four functions to:
##      1. set the matrix 
##      2. get the matrix
##      3. set the inverse of the matrix
##      4. get the inverse of the matrix

## Inputs 
##              x: a square, invertible matrix
## Outputs
##              a list containing functions


makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                
# 1.Set the matrix
# Allows user to set a new matrix in the cache; stores it as 'x', resets the
# value of the inverse to NULL
                set <- function(y){
                        x <<- y
                        inv <<- NULL
                }
                
# 2.Get the matrix stored in cache and return it to its calling environment.
                get <- function() x
                
# 3.Set the inverse to be a variable called 'inv' in cache.
                setInverse <- function(inverse) inv <<- inverse

# 4.Get the value of 'inv' from the cache and return it to the caller.
                getInverse <- function() inv
                
# Return a list of the above functions.
                list(set = set, get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
        
}


## cacheSolve uses the functions passed to it to get the inverted matrix of the 
## matrix stored in cache. It finds out if the inverse has already been computed
## - if so, return the inverted matrix; if not, get the current matrix in cache,
## compute the inverse matrix, set the inverted matrix in cache, and return the
## inverted matrix to the caller. 

## Inputs 
##              x: a list of functions from makeCacheMatrix
## Outputs
##              the inverse of the matrix stored in the cache

cacheSolve <- function(x, ...) {
        # The getInverse() function is used to get the value of the 
        # inverse matrix of the matrix 'x' stored in cache.
        inv = x$getInverse()
        
        # If the inverse matrix has been previously calculated, it is already 
        # stored in cache and its value will not be NULL. In this case, the  
        # inverse is returned and the function exits.
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        # If the inverse matrix is not stored in cache, it is calculated now.
        # First, the original matrix is retrieved through the get() function:
        data = x$get()
        
        # Next, the inverse of the matrix is found using the inbuilt solve()
        # function.
        inv = solve(data, ...)

        # The inverse matrix in the cache is set using the setinv() 
        # function.
        x$setInverse(inv)
        
        # The inverted matrix is returned
        inv
        
}

## Useage:
# > A     <- matrix(c(4,2,7,6),nrow=2,ncol=2)
# > CACHE <- makeCacheMatrix(A)
# > Ainv  <- cacheSolve(CACHE)
# > Ainv
#      [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
# > CACHE$set(Ainv)
# > Ainv  <- cacheSolve(CACHE)
# > Ainv
#      [,1] [,2]
# [1,]    4    7
# [2,]    2    6
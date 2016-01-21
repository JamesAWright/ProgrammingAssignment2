## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Inputs 
##              x: a square, invertible matrix
## Outputs
##              a list containing a function to:
##                      1. set the matrix
##                      2. get the matrix
##                      3. set the inverse of the matrix
##                      4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                
                set <- function(y){
                        x <<- y
                        inv <<- NULL
                }
        
                get <- function() x
                setInverse <- function(inverse) inv <<- inverse
                getInverse <- function() inv
                
                list(set = set, get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # The getInverse() function is used to get the value of the 
        # inverse matrix of 'x' stored in cache.
        inv = x$getInverse()
        
        # If the inverse matrix has been previously calculated, it is already 
        # stored in cache it will not be NULL. In this case, the inverse is 
        # returned and the function exits.
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        # If the inverse matrix is not stored in cache, it is calculated now.
        # First, the matrix is retrieved through the get() function:
        data = x$get()
        
        # Next, the inverse of the matrix is found using the inbuilt solve()
        # function.
        inv = solve(data, ...)

        # The value of the inverse in the cache is set using the setinv() 
        # function.
        x$setInverse(inv)
        
        # The inverse is returned
        inv
        
}
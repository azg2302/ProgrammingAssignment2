## The functions functions allow you to load a matrix and its inverse into local environment and view these matrices
## The inverse of the matrix is then calculated if it is not already assigned in the local environment

## This function loads a matrix into the cache i.e. the local enviroment and allows you to view it
## It also loads the inverse into the cache and allows you to view it
## The output is a list containing 4 functions as elements and is used in the cacheSolve function
## The argument of the function is a matrix for which the inverse will be calculated
## However, the set function in the makeCacheMatrix can be used to change this matrix afterwards

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function checks whether an inverse is already calculated and stored in the inv variable in the local environment
## It it is it tells you that it will return this value
## Otherwise it reads the matrix from the local environment, calculates inverse and saves it back to cache
## It returns the invers

cacheSolve <- function(x, ...) {
       inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}

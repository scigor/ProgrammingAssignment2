## This file contains two functions makeCacheMatrix and cacheSolve.
##
## The makeCacheMatrix function creates a special matrix, which allows the caching of the inverse of the matrix.
## It makes heavy use of the <<- operator. It causes a search to made through parent environments for an existing definition of the variable being assigned.
## For more information on the <<- operator enter ?assignOps into your R console.
##
## The cacheSolve function uses the matrices created by makeCacheMatrix and returns the inverse of the matrix. 
## The cacheSolve function first checks if the matrix inverse has been cached and if so returns the chached inverse.
## If no cached inverse could be found (inverse equals NULL) the inverse of the matrix is computed. 
## The cacheSolve does not checks if the matrix is invertable (as required by the assignment).
##
## Example:
## create a new random 3x3 matrix with cachable inverse, compute inverse, compute matrix %*% inverseOfMatrix to get the identity matrix
## theMatrix <- makeCacheMatrix(matrix(runif(9), 3, 3));
## cacheSolve(theMatrix);
## theMatrix$getInverse() %*% theMatrix$get();




## The function creates a special cachematrix,
## which allows the caching of the inverse of the matrix to avoid repeated computation.
## It is actually returns a list of four functions:
## set, get, setInverse, getInverse.
makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    
    ## this function sets m to y and the matrix inverse to NULL, this is the initial state of the matrix
    set <- function(y) 
    {
        x <<- y
        matrixInverse <<- NULL
    }
    
    # returns the value of the matrix
    get <- function() x
    
    ## allows to set the matrix inverse
    setInverse <- function(inverse) matrixInverse <<- inverse
    
    ## returns the inverse of a matrix
    getInverse <- function() matrixInverse
    
    ## combines the previously defined functions into a list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The function uses matrices created by makeCacheMatrix function.
## It first checks if a cached inverse is available (not NULL). 
## If it is, the cached inverse is returned. If the cached inverse is NULL
## then it is computed by the solve() function, cached and returned.
cacheSolve <- function(x, ...) {
    
    ## Check if the inverse of the matrix inverse has been priviously computed. If so return the cached matrix inverse.
    matrixInverse <- x$getInverse()
    if(!is.null(matrixInverse)) {
        message("getting cached data")
        return(matrixInverse)
    }
    
    ## compute inverse of the matrix
    data <- x$get()
    matrixInverse <- solve(data)
    ## cache matrix inverse
    x$setInverse(matrixInverse)
    
    ## Return a matrix that is the inverse of 'x'
    matrixInverse
}

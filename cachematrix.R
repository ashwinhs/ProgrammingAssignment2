##There are 2 functions
##makeCacheMatrix : Creating a "Special" matrix
##cachesolve: Check and Cache the Matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(inMatrix = matrix()) {
    inverseMatrix <- NULL
    
    ##Method to set the Matrix
    ##to the matrix passed as input by the User
    set <- function(setMatrix) {
        cacheMatrix <<- setMatrix
        i <<- NULL
    }
    
    ## Method to Return the Matrix
    ## This method basically is used to view what the Matrix that was used as input
    get <- function() {
        inMatrix
    }
    
    ## Method to Set the Inverse of the Matrix
    ## This method basically is used to set the Inverse of the Matrix in the "Cache"
    ## Or send it to the User's Env which can be accessed outside this function
    setInverse <- function(inverse) {
        inverseMatrix <<- inverse
    }
    
    ## Method to Return the Inverse of the Matrix
    ## This method basically is used to displat or get the Inverse of the Matrix from the "Cache"
    ## so that it can be access/used from outside the function
    getInverse <- function() {
        inverseMatrix
    }
    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated , 
## then the "cachesolve" should retrieve the inverse from the cache.
## If the Inverse is not solved, then calculate it and add to the cache
cachesolve <- function(inputMatrix, ...) {
    ## Check if the inverse is already calcuated and available in cache
    m <- inputMatrix$getInverse()
    
    ##If the inverse is available, the value from the getInverse will not be NULL
    ##Means, its calculated and available. So return the Inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ##If the inverse is not available
    ##Calculate the Inverse and store it in the matrix
    ##Get the Matrix from the object
    dataMatrix <- inputMatrix$get()
    
    ##Calculate Inverse using Matrix Multiplication
    m <- solve(dataMatrix) %*% dataMatrix

    ## Set the inverse to the object
    inputMatrix$setInverse(m)
    
    ##Return the Matrix
    m
}


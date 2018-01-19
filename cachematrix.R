## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(sqMatrix = matrix()) {
    
    inverseMatrixChache <- NULL
    
    # set a new matrix
    setMatrix <- function(newSqMatrix) {
        sqMatrix <<- newSqMatrix
        #clear the cached inverse matrix
        inverseMatrixChache <<- NULL
    }
    
    # returns current matrix
    getMatrix <- function() sqMatrix
    
    # cache the inverse matrix    
    setInverseMatrix <- function(inverseMatrix) inverseMatrixChache <<- inverseMatrix
    
    #returns current inverse matrix
    getInverseMatrix <- function() inverseMatrixChache
    
    list(setMatrix = setMatrix, getMatrix =getMatrix, setInverseMatrix = setInverseMatrix,  getInverseMatrix = getInverseMatrix)
}

#This function computes the inverse of a matrix returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed),then this
#function will retrieve the inverse from the cache.
cacheSolve <- function(sqMatrix, ...) {

    ## Return a matrix that is the inverse of 'sqMatrix'
    inverseMatrix <- sqMatrix$getInverseMatrix()
    # inverse matrix is already cached
    if (!is.null(inverseMatrix)) {
        message("Getting cached data...")
    }
    #calculates inverse matrix and cache it
    else {
        sqMat <- sqMatrix$getMatrix()
        inverseMatrix <- solve(sqMat)
        sqMatrix$setInverseMatrix(inverseMatrix)
    }
    return(inverseMatrix)
}

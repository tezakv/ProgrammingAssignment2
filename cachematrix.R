## Solution for fetching the inverse of a matrix from cache

## makeCacheMatrix creates 'matrix' that stores its inversion and provides it upon request

makeCacheMatrix <- function(mat = matrix()) {

    inverseMatrix <- NULL
    
    set <- function(applyMatrix) {
        mat <<- applyMatrix
        inverseMatrix <<- NULL
    }
    
    get <- function() mat
    
    setInverse <- function(inv) inverseMatrix <<- inv
    
    getInverse <- function() inverseMatrix
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve calculates the inverse of a matrix if the matrix is 'new' or 'changed', else fetches the inverse from cache

cacheSolve <- function(invMatClass, ...) {
    
	## Return a matrix that is the inverse of 'mat'
		
	inverseMatrix <- invMatClass$getInverse()
    
    if (!is.null(inverseMatrix)) {
        message("Fetching inverse from cache...")
        return(inverseMatrix)       
    }
    
    matrix <- invMatClass$get()
    
    inverseMatrix <- solve(matrix, ...)
    
    invMatClass$setInverse(inverseMatrix)
    
    inverseMatrix    
}
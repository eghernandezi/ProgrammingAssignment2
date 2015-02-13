## These functions calculate the matrix inversion.
## This calculation is an expensive one. To save resouces, these functions
## handle a cache to store the inverse matrix. If the original matrix has not 
## changed or the inverse has been already calculated, 
## calculation is not done again and the stored inverse is returned.

## This function creates a cached matrix based on the given one. 
## The cached matrix has the following methods:
##      - set: It sets the original matrix. If the new value is the same as the 
##             previous one, the original matrix is not changed
##      - get: It returns the original matrix
##      - calculateInverse: It calculates and sets the inverse matrix based on 
##                          the original one
##      - getInverse: It returns the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    
    set <- function(mtxPar) {
        if(!identical(x, mtxPar)) {
            print("The matrices are differente, changing the original matrix")
            x <<- mtxPar
            inverse <<- NULL
        } else {
            print("Matrix is the same as the previous one, the value and 
                  the inverse are not changed")
        }
    }
    
    get <- function() x
    
    calculateInverse <- function() {
        inverse <<- solve(x)
    }
    
    getInverse <- function() {
        inverse
    }
    
    list(
        set = set, 
        get = get, 
        getInverse = getInverse,
        calculateInverse = calculateInverse)
}


## This function calculates the inverse for the given cached matrix
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    if(is.null(inverse)) {
        print("The inverse matrix has not been calculated yet")
        x$calculateInverse()
        inverse <- x$getInverse()
    } else {
        print("The inverse matrix is retrieved from cache")
    }
    
    inverse
}
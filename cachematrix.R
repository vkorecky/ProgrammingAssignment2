## Functions that can be used to calculate of matrix invwersion.
## Function "makeCacheMatrix" creates object, where is stored matrix and their inverse.
## Function "cacheSolve" checks if matrix inversion exists in the cache, if no the inverse matrix is created and stored in cache for feature use.

## Creates object which contains matrix and their inversion (if is calculated).
## Function works like a cache.
makeCacheMatrix <- function(x = matrix()) {      
    i <- NULL    
    
    ## Sets the metrix
    set <- function( matrix ) {
        m <<- matrix
        i <<- NULL
    }
    
    ## Gets the matrix
    get <- function() {
        m
    }
    
    ## Sets inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
    ## Gets inverse of the matrix
    getInverse <- function() {        
        i
    }
    
    ## List of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function checks if object "x" contains inversion of the matrix, 
## If inversion of matrix exists in cache, cached object is returned.
## If inversion of materix doesn't exists, inversion of matrix is calculated and stored in cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## Checks if inverse exists, if yes returns inverse from cache
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    
    ## Gets the matrix form object
    data <- x$get()
    
    ## Calculate the inverse of matrix
    m <- solve(data) %*% data
    
    ## Sets the inverse to the object (cache)
    x$setInverse(m)
    
    ## Returns the matrix
    m
}

## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

 # m will store the cached inverse matrix 
 m <- NULL 
  
 #Setter for the matrix	
  set <- function(y) {
        x <<- y
        m <<- NULL
    }

    #Getter for the matrix  
    get <- function() x
 
    # Setter for the inverse
    setInverse <- function(inverse) m <<-inverse 

    # Getter for the inverse
    getInverse <- function() m

    # Returns the matrix with the newly defined functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   m <- x$getInverse()
   # Check if the inverse is already calculated. If so, return it.	
    if ( ! is.null(m)) {
        print("getting cached data")
        return(m)
    }
# Calculate the inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m     
}


## cachematrix.R creates an R object that caches a matrix and its
## inverse and subsequently retrieve the inverse from the cached
## value that is stored to avoid recomputing the inverse repeatedly.

## creates a cache for a matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(z) {
                x <<- z
                inv <<- NULL
        }
 ## get .... get the matrix x
 ## setInverse .... set inverse of matrix
 ## getinverse .... gets inverse of matrix
        
           get <- function() x
          setInverse <- function(inverse) inv <<- inverse
           getInverse <- function() inv

 ## creates list of vectors with methods as set / get 
 ## and their inverses, returns to the parent environment. 
 ## this is done so that $ can be used subsequently
           
           list(set = set, get = get,
             setInverse = setInverse,
                getInverse = getInverse)
}
 ## Calculates the Inverse of the "matrix" created above . First
 ## checks to see if the inverse has already been calculated. 
 ## If yes, retrieve inverse from the cache and skips the computation. 

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
           }

  ## Otherwise, calculate the inverse of the data and set the 
  ## value of the inverse  in the cache using the setInverse function.
           data <- x$get()
           inv <- solve(data, ...)
              x$setInverse(inv)
                      inv
}
## `cachmatrix.R`: This program essentially performs two major functions.
## The first function, `makeCacheMatrix` creates a special "matrix",
## which is really a list (as it should be) containing a function to
## 1. set the value of the "matrix" 
## 2. get the value of the "matrix" 
## 3. set the inverse of the "matrix" 
## 4. get the inverse of the "matrix" 

## ----------------------------------------------------------
## NOTE: As per guidelines of the assignment, we have
## assumed that the matrix supplied is always "invertible".
## ----------------------------------------------------------

## `makeCacheMatrix`: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL              ## Set the initial value of the inverse of the matrix to NULL
     set <- function(y) {
          x <<- y             ## Set the parent environment variable to the value of the matrix
          inv <<- NULL        ## Set the parent environment variable inverse of the matrix to NULL
     }
     get <- function() x      ## Return the value of the matrix (parent environment)   
     setinv <- function(matinv) inv <<- matinv   ## Set the value of the inverse of the matrix (in the parent environment) that was passed as "matinv"
     getinv <- function() inv ## Return the value of the inverse of the matrix
     
     ## Return the list of getter/setter functions as part of the function definition upon calling (encapsulation)
     list(set = set, get = get,  
          setinv = setinv,
          getinv = getinv)
}


## `cacheSolve`: This following function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. However, it first checks to
## see if the inverse has already been calculated  (and the matrix has not 
## changed). If so, it retrieves the inverse from the cache and skips the 
## computation. Otherwise, it solves the "matrix" to calculate the inverse
## of the "matrix" passed and sets the value of the inverse in the cache 
## via the setinv function.

cacheSolve <- function(x, ...) {
        
     inv <- x$getinv()            ## Call the inverse getter function of the object 'x'
     if(!is.null(inv)) {          ## If value of inverse is already set i.e. not NULL, return the cached value
          message("getting cached data")
          return(inv)
     }
     
     ## Else compute the value of the inverse of 'x' using SOLVE function and set it within the object using setter methods
     data <- x$get()              ## Call the getter function to get the value of the matrix 'x'
     matinv <- solve(data, ...)   ## Compute the inverse of the matrix using SOLVE function
     x$setinv(matinv)             ## Call the inverse setter function of the object to set the value of the  inverse 
     matinv                       ## Return a matrix that is the inverse of 'x'
}

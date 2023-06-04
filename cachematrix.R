## LGlover Cache Matrix (R). Make these two functions:
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##     If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##     retrieve the inverse from the cache.
## When submit: create 40 character SHA-1 hash

## Overall idea: Taking the mean of a long numeric vector can require much computing power & time. In order to
## save us this hassle, cache the value of the mean so it can be found fast. This task will cache the inverse of matrix.

## ******************************

##Note: we will assume the matrix given is always invertible.

# Function to cache matrix

makeCacheMatrix <- function(x = matrix()) {
  # Create a list to store the matrix, its inverse, and a flag indicating whether the inverse has been calculated.
  cache <- list(
    matrix = x,
    inverse = NULL,
    is_inverse_calculated = FALSE
  )
  
  # Create functions to set the matrix, get the matrix, set the inverse, and get the inverse.
  set <- function(y) {
    cache$matrix <<- y
    cache$is_inverse_calculated <<- FALSE
  }
  
  get <- function() {
    cache$matrix
  }
  
  setInverse <- function(inverse) {
    cache$inverse <<- inverse
    cache$is_inverse_calculated <<- TRUE
  }
  
  getInverse <- function() {
    cache$inverse
  }
  
  # Return the list of functions.
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## ****************************************



## Continuig from the last function, this 'cacheSolve' function will see if the inverse of the matrix 
## is already cached. If so, the function returns the cached inverse. Otherwise,the function re-calculates
## the inverse of the matrix and caches the new value.

cacheSolve <- function(x, ...) {
  
  # Check if inverse is already cached
  inv <- x$getInverse()
  
  # if cache has been done, return it
  if (!is.null(inv)) {
    message("R is retrieving the cached data")
    return(inv)
  }
  
  # Else, calculate the inverse & cache 
  inv <- solve(x$get(), ...)
  x$setInverse(inv)
  
  # return that inverse
  inv
}

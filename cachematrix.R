## Cache the inverse of a matrix

## makeCacheMatrix
## list of functions to:
## 1- set the value of the matrix (set)
## 2- get the value of the matrix (get)
## 3- set the value of the inverse (setinv)
## 4- get the value of the inverse (getinv)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve:
## computes the inverse of the matrix returned by makeCacheMatrix (get).
## If the inverse has already been calculated (and the matrix has not changed), 
## then the inverse is retrived from the cache (getinv).

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  M <- x$get()
  inv <- solve(M)
  x$setinv(inv)
  inv
}

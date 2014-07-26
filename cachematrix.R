## These two functions can be used to store (cache) a matrix (x) and the computed inverse
## of that matrix (m) in a special type of matrix that contains variables to hold
## both matrices and a list of sub-functions that can be used as methods
## to get and set both the matrix and its inverse.

## makeCacheMatrix(): This function creates the special type of matrix.  
## The initial matrix should be passed as an argument.  If missing an empty matrix will be created.
## The initial matrix is held in variable x.
## The cached inverse of the matrix if computed is held in m.  m will initially be null.
## There is also a list of sub-functions to set and get the values of x and m
## Once an instance of the special matrix has been created cacheSolve should be used to compute the inverse.

makeCacheMatrix <- function(x = matrix()) { 
  
  m <- NULL 
  
  # sub-functions to set and get the stored standard matrix
  
  set <- function(y) { 
      x <<- y 
      m <<- NULL 
    }
  get <- function() x 
  
  # sub-functions to set and get the stored inverse matrix cache
  
  setinverse <- function(inverse) m <<- inverse 
  getinverse <- function() m 
  
  # puts the sub-functions into a list so they can be easily accessed
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}

## cacheSolve(): This function will return the inverse of a provided special matrix x.
## x cannot be a standard R matrix but must be a special matrix create using the makeCacheMatrix() function.
## If the inverse has already been computed and stored in cache (m) then this function will return the cached value.
## If m is null then this function will compute the inverse, cahce it in m and return it.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  # If there is a cached inverse matrix stored in variable m of the special matrix then return the cached value
  
  m <- x$getinverse() 
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  
  # If variable m of the special matrix is empty then compute the inverse, store it in the cache then return it
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
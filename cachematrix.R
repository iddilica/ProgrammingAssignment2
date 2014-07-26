## These two functions can be used to store (cache) a matrix (x) and the computed inverse
## of that matrix (m) in a special type of matrix that contains variables to hold
## both matrices and a list of sub-functions taht can be used as methods
## to get and set both the matrix and its inverse.

## This function creates the special type of matrix.  
## The initial matrix should be passed as an argument.  If missing an empty matrix will be created.
## The initial matrix is held in variable x.
## The cached inverse of the matrix if computed is held in m.  m will initially be null.
## There is also a list of sub-functions to set and get the values of x and m
## Once an instance of the special matrix has been created cacheSolve should be used to
## compute the inverse.

makeCacheMatrix <- function(x = matrix()) { # initialise the special matrix providing a standard matrix as an argument
  m <- NULL # sets inverse to null initially as hasn't been computed
  set <- function(y) { # sub-function to set the value of x to a new matrix
    x <<- y # will set the parent function x variable to the value of y passed to sub-function
    m <<- NULL # will set parent function m to Null to clear cache as matrix has changed
  }
  get <- function() x # just returns the value of x i.e the standard matrix
  setinverse <- function(inverse) m <<- inverse # sets the value of m to the inverse of x (used by cacheSolve function)
  getinverse <- function() m # just returns the value of m i.e the cached inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) # puts the sub-functions into a list
}

## This function will return the inverse of a provided matrix x.
## x cannot be a standard R matrix but must be a special matrix create using the 
## makeCacheMatrix function.
## If the inverse has already been computed and stored in cache (m) then this function
## will return the cached value.
## If m is null then this function will compute the inverse, cahce it in m and return it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse() # get the value of the cached inverse variable, m, from the provided special matrix
  if(!is.null(m)) { # if m has a value return the cached value for m
    message("getting cached data")
    return(m)
  }
  
  data <- x$get() # if m is null get the standard matrix stored in x variable of the special matrix
  m <- solve(data, ...) # compute the inverse
  x$setinverse(m) # cache the computed inverse in the m variable of the special matrix
  m # return the computed inverse
}

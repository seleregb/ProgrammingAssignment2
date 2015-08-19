# This R file contains 2 functions used for calculating the inverse of a matrix
# and saves it to the cache index, so that the user can easily access it later 
# instead of re-calculating the inverse again

# This first function makeCacheMatrix builds on the makeVector function for creating
# a special "vector", by creating a special "matrix" which is actually a list for:
# 1. Setting the value of the matrix
# 2. Getting the value of the matrix
# 3. Setting the value of the inverse
# 4. Getting the value of the inverse


# create a matrix object x 
makeCacheMatrix <- function(x = matrix()) {
  
  # define the cache m and set to NULL
  m <- NULL
  
  # create a set function for assigning the input matrix 'y' to the variable 'x'
  # in the parent environment then;
  # re-initialize 'm' in the parent environment to NULL
  set <- function(y) {
    x <<- y 
    m <<- NULL 
  }
  
  # creating a get function to return the matrix x
  get <- function() x 
  
  # creating a setInverse function to set the cache object 'm' equal
  # to the inverse of the matrix 'x'
  setInverse <- function(inverse) m <<- inverse
  
  # creating a getInverse function to return the cached inverse of x
  getInverse <- function() m 
  
  # The function makeCacheMatrix ultimately returns a list of four elements which
  # calls on the four functions created
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# The following function calculates the inverse of the special "matrix" created with 
# the above function. However, it first checks to see if the inverse has already been 
# calculated. If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in 
# the cache via the setInverse function.

# calling the matrix object 'x' created in the makeCacheMatrix function
# this would return an inverse matrix of 'x'
cacheSolve <- function(x, ...) {
  
  # calling the getInverse function from the makeCacheMatrix function
  m <- x$getInverse()
  
  # checking if the value is not null and returning the output
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # calling the get function from the makeCacheMatrix function to access the matrix
  data <- x$get()
  
  # calculating the inverse of the matrix
  m <- solve(data, ...)
  
  # x already contains the cache results, so there would be no need to recalculate
  x$setInverse(m)
  
  # return the result
  m
}

# Sample Output

# y <- matrix(seq(1:4),2,2) # create a square matrix
# y
# matrix.sample <- makeCacheMatrix(y) # create a cache matrix
# cacheSolve(matrix.sample) # return the inverse
# cacheSolve(matrix.sample) # checks for the cache object and returns the result


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Placeholder for the cached inverse
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse
  getInverse <- function() inv
  
  # Return a list of functions to interact with the matrix and its inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse
  inv <- x$getInverse()
  
  # return cached inverse
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not, calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)  # Solve to compute the inverse
  
  # Cache the calculated inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}





test_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)

# Create the special matrix object
cache_matrix <- makeCacheMatrix(test_matrix)

# calculate and cache the inverse
inverse1 <- cacheSolve(cache_matrix)
print(inverse1)

# retrieve the cached inverse
inverse2 <- cacheSolve(cache_matrix)
print(inverse2)

# Change the matrix and recalculate
new_matrix <- matrix(c(2, 3, 2, 5), nrow = 2, ncol = 2)
cache_matrix$set(new_matrix)

# Calculate the new inverse
inverse3 <- cacheSolve(cache_matrix)
print(inverse3)


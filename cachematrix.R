### Function 1 - Create an R object that stores a matrix and the inverse of the matrix ###
# -> The first function "makeCacheMatrix" takes a matrix as input
# -> It initializes the object "inv" which is later assigned to the parent environment
# -> With the two functions "setinv" and "getinv", the inverse of the matrix is assigned 
#       to the parent environment and then fetched from this environment
# -> In the last step, a list with all objects is prepared

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize x and inv ##
  # -> matrix x given as argument
  inv <- NULL
  
  
        
  ## Define functions ##
  # Set and get x #
  # -> assign x and inv to parent environment
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  # -> get x from the parent environment
  get <- function() x
  
  
  # Set and get the inverse of the matrix #
  # -> assign inv and solve to parent environment
  setinv <- function(solve) inv <<- solve
  
  # -> get correct value from parent environment
  getinv <- function() inv  
  
  
  
  ## Prepare the list in the parent environment ##
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}




### Function 2 - Check if inverse of matrix is available and retrieve it ###
# -> The second function takes the inverse of the matrix from the input object
# -> It then checks if the result == NULL
# -> If "inv" != NULL, then the function takes the cached inverse of the matrix
#       --> no need to calculate it again
# -> If "inv" == NULL, then the function takes the data and calculates the inverse
#       of the matrix
# -> In the last step, the inverse of the matrix is printed

cacheSolve <- function(x, ...) {
  ## Get the inverse of a matrix from the input object ##
  inv <- input$getinv()
  
  
  ## Check if the result is NULL and display message ##
  # -> if inv != NULL --> if != NULL, then use the cached inverse of the matrix
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  
  # -> if inv == NULL, then get the data and calculate a new inverse of the matrix
  data <- input$get()
  inv <- solve(data, ...)
  
  
  ## Print the inverse of the matrix ##
  input$setinv(inv)
  inv
}

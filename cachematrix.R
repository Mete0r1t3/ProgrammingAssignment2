## Caches the inverse of a matrix when it is first computed
## then retrieves the inverse from cache when needed


makeCacheMatrix <- function(a = matrix()) {
      ## Creates functions which allow a matrix and its inverse to be 
      ## cached or retrieved from cache
      ##
      ## Args:
      ##    a: The matrix to be inverted. Default is the empty matrix: matrix()
      ##       
      ## Returns:
      ##    A List of functions which allow caching and 
      ##    retrieving of a matrix and its inverse
      
      
      a_inv <- NULL
      
      ## Caches the given matrix, b, as the matrix to be inverted
      set <- function(b) {
            a <<- b
            a_inv <<- NULL
      }
      
      ## Returns the original matrix
      get <- function() a
      
      ## Caches the given matrix, inv, as the inverse matrix
      setInv <- function(inv) a_inv <<- inv
      
      ## Returns the inverse matrix
      getInv <- function() a_inv
      
      ## Returns a list containing getter and setter functions
      list(set = set, 
           get = get,
           setInv = setInv,
           getInv = getInv)
}



cacheSolve <- function(x) {
      ## Checks if the inverse matrix has already been cached, if not, 
      ## computes the inverse, stores it in cache, then returns it.
      ##
      ## Args:
      ##    x: A List of getter and setter functions in the same form as 
      ##       makeCacheMatrix's output.
      ##
      ## Returns:
      ##    A matrix that is the inverse of the matrix passed to 
      ##    makeCacheMatrix
      
      inv <- x$getInv() # Attempts to retrieve the inverse matrix from cache
      if(!is.null(inv)) { # Checks if the retrieved inverse matrix exists
            message("Retrieving inverse from cache")
            return(inv)
      } else if(det(x$get()) == 0){
            message("The given matrix is singular (it does not have an inverse)")
      } else {
            matrix <- x$get()
            inv <- solve(matrix) # Finds the inverse matrix
            x$setInv(inv) 
            return(inv) 
      }
}

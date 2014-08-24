## makeCacheMatrix creates a special matrix object
## and returns the list of various functions associated
## cacheSolve sees if the inverse has been calculated before and returns it, 
## otherwise, it calculates the inverse from scratch

## creates a special matrix object that can cache its inverse
## if no argument passed, creates an empty 1x1 matrix object

makeCacheMatrix <- function(x = matrix()) { ##in my opinion, analogous to a class in C++ 
  ##where data member is x and functions are as defined below
  inverse <- NULL
  getMatrix <- function() ##returns the matrix
  {
    x
  }
  setMatrix <- function(y) ##sets the matrix object to the one passed in the call
  {
    x<<-y
    inverse <<-NULL
  }
  
  ## returns the inverse
  getInverseOfMatrix <- function()
  {
    inverse
  }
  
  ##sets the inverse
  setInverseOfMatrix <- function(inv)
  {
    inverse <<- inv
  }
  
  ##Returns a list of functions associated with the matrix
  list(getMatrix = getMatrix,setMatrix = setMatrix,getInverseOfMatrix = getInverseOfMatrix,setInverseOfMatrix = setInverseOfMatrix)

}


##checks if inverse already present, otherwise calculates inverse and returns it

cacheSolve <- function(x, ...) {
        inv <- x$getInverseOfMatrix()
        if(!is.null(inv))
        {
          message("getting cached inverse")
          return (inv) ##note that () are required with return in R
        }
        ##else calculate inverse using solve
        ##extract the underlying matrix from the special matrix object using its method
        m <- x$getMatrix()
        ##calculate the inverse of this matrix
        inv <- solve(m)
        ##set the inverse of the special matrix object
        x$setInverseOfMatrix(inv)
        inv
}

## Put comments here that give an overall description of what your
## functions do

## Create a "special" matrix that can cache the solved
makeCacheMatrix <- function(theMatrix = matrix()) 
{
  ## inversed matrix
  inversed <- NULL
  ## set a new matrix 
  set <- function(newMartrix) 
  {
    theMatrix <<- newMartrix
    inversed <<- NULL
  }
  ## get the matrix
  get <- function() 
  {
    theMatrix
  }
  ## set the inversed matrix
  setInversed <- function(pInversed)
  {
    inversed <<- pInversed
  }
  ## return the Inversed matrix
  getInversed <- function()
  {
    inversed
  }
  ## list of functions
  list(set = set, get = get, setInversed = setInversed, getInversed = getInversed)
}


## receive a matrix if not existis a inversed version calculated the inversed version and cache
cacheSolve <- function(pMatrix, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  solved <- pMatrix$getInversed()
  if(!is.null(solved))
  {
    print("get the inversed matrix from cache!")
    return(solved)
  }
  theMatrix <- pMatrix$get()
  inversed <- solve(theMatrix)
  pMatrix$setInversed(solved)
  inversed
}
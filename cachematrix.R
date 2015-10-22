#makeCacheMatrix takes x (matrix) as the input, the matrix whose inverse needs to be calculated
#and returns a list containing functions -> setMat, getMat => to set/get the input matrix and 
#setInverse, getInverse => to set/get the inverse of input matrix.
makeCacheMatrix <- function(x = matrix()) {
  matInverse <- NULL
  setMat <- function(y){
    x <<- y
    matInverse <<-NULL
  }
  getMat <- function() x
  setInverse <- function(inverse) matInverse <<- inverse
  getInverse <- function() matInverse
  list(setMat = setMat, getMat = getMat, setInverse = setInverse, getInverse = getInverse)
}

#cacheSolve takes funcVector, a special list containing functions, created by makeCacheMatrix
#and returns the inverse of the matrix which was provided to makeCacheMatrix
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    print("Using already cached inverse.")
    return(inverse)
  }
  ipMatrix <- x$getMat()
  inverse <- solve(ipMatrix)
  x$setInverse(inverse)
  inverse
}
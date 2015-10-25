## This function sets and gets the matrix

makeCacheMatrix <- function(x = matrix()) {
  invs = NULL
  set = function(y) {
    x <<- y
    invs <<- NULL
  }
  get = function() x
  setInverse = function(inverse) invs <<- inverse 
  getInverse = function() invs
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function returns the inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invs = x$getInverse()
  if (!is.null(invs)){
    message("getting cached data of inverse")
    return(invs)
  }
  GetInverseData = x$get()
  invs = solve(GetInverseData, ...)
  x$setInverse(invs)
  return(invs)
}

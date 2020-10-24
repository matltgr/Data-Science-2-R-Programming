## This function is similar to makeVector
## It enables to get/set a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y){
    x <<- y 
    invMatrix <<- NULL
  }
  get <- function() x
  setInv <- function(mat) invMatrix <<- solve(mat)
  getInv <- function() invMatrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve enables to retrieve 

cacheSolve <- function(x, ...) {
        ## Checks for cache data
        inv <- x$getInv()
        if(!is.null(inv)){
          message("data was cached")
          return(inv)
        }
        ## Else compute and stores inverse
        message("No cached data found, result will be stored")
        mat <- x$get()
        x$setInv(mat)
        x$getInv()
}

## This Function hopefully should cache a Matrix, for optimizing computing power

## it saves the Matrix in a Function

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


#For Testing

test = function(mat){
  temp = makeCacheMatrix(mat)
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}

#This Function resolves the cache

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)){
    message("getting the cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}
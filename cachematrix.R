##This Function is used for caching the inverse of a matrix...

##How to Test Functions: 
##Run makeCacheMatrix.R ; Run cacheSolve.R
##Enter the Following....

##> myMatrix <- (matrix(c(3,2,1,6,5,4,9,7,8), 3, 3))     #Create a funciton and save to myMatrix
##> solve(myMatrix)                                      #run a regular solve to check the inverse matrix
##[,1]       [,2]       [,3]
##[1,]  1.3333333 -1.3333333 -0.3333333
##[2,] -1.0000000  1.6666667 -0.3333333
##[3,]  0.3333333 -0.6666667  0.3333333
##> catchMatrix <- makeCacheMatrix(myMatrix)             #Invoke makeCacheMatrix function
##> cacheSolve(catchMatrix)                              #Invoke cacheSolve funciton
##[,1]       [,2]       [,3]
##[1,]  1.3333333 -1.3333333 -0.3333333
##[2,] -1.0000000  1.6666667 -0.3333333
##[3,]  0.3333333 -0.6666667  0.3333333
#> cacheSolve(catchMatrix)                              #Invoke cachSolve function again 
##getting cached data                                    #and validate data is from cached data
##[,1]       [,2]       [,3]
##[1,]  1.3333333 -1.3333333 -0.3333333
##[2,] -1.0000000  1.6666667 -0.3333333
##[3,]  0.3333333 -0.6666667  0.3333333


makeCacheMatrix <- function(x = matrix()){
  s <- NULL
  set <- function(y) {
    x<<- y
    s <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) s <<- solve
  getInverse <- function() s
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function is used to in conjunction with makeCacheMatrix to return the inverse of a catch matrix.
## if called more than once it will retrun the catch values...see above comments to test test the function

cacheSolve <- function(x, ...) {
  v <- x$getInverse()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data, ...)
  x$setInverse(v)
  v
}
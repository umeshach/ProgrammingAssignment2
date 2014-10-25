## This function makes cache matirx and get inverse of the matric with catcheSolve command
## the functions create a Cache Matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## create a sample matrix and save as b
b <- makeCacheMatrix( matrix(c(1,2,8,9), nrow = 2, ncol = 2) )



## this function creats a matix that is inverse of x

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get() 
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

cacheSolve(b)## returns inverse of matrix b from cache

##      [,1]       [,2]
##[1,] -1.2857143  1.1428571
##[2,]  0.2857143 -0.1428571

# 1. makeCacheMatrix: This function creates a special "matrix" 
#    object that can cache its inverse 
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

# 2. cacheSolve: This function computes the inverse of the special
#    "matrix" returned by makeCacheMatrix above. If the inverse has 
#     already been calculated (and the matrix has not changed), then 
#    the cachesolve should retrieve the inverse from the cache


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
## Exemple of run to veirfy if it work
# a0 <- makeCacheMatrix(matrix(c(0,2,2,13,-2,-4,6,7,-10), nrow = 3, ncol = 3))
#> cacheSolve(a0)
#             [,1]        [,2]        [,3]
#[1,]  0.114832536  0.25358852  0.24641148
#[2,]  0.081339713 -0.02870813  0.02870813
#[3,] -0.009569378  0.06220096 -0.06220096











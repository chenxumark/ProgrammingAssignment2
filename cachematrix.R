## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly
## This assignment is to write a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix: This function creates a special "matrix" object that can cache 
## its inverse, which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of inverse of the matrix
## 4.get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    Inv<-NULL
    set<-function(y){
      x<<-y
      Inv<<-NULL
    }
    get<-function() x
    setIver <- function(Iver) Inv<<-Iver
    getIver <- function() Inv
    list(set =set, get =get, 
         setIver=setIver,getIver=getIver
    )
}


## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

## Assumption: matrix is invertible (n*n Matricx).

cacheSolve <- function(x, ...) {
  Inv<-x$getIver()
  if(!is.null(Inv)){
    message("getting caced data")
    return(Inv)
  }
  data<-x$get()
  Inv<-solve(data,...)
  x$setIver(Inv)
  Inv      
}

# Experiment to try if it works (3*3 Matrix)
# >x = rbind(c(1, -1/2,-1/4), c(-1/4,-1/2, 1),c(-1/2,1,-1/4))
# >make_cache<-makeCacheMatrix(x)
# >make_cache$get()
# [,1] [,2]  [,3]
# [1,]  1.00 -0.5 -0.25
# [2,] -0.25 -0.5  1.00
# [3,] -0.50  1.0 -0.25


# Scenario 1: No Cache
# > cacheSolve(make_cache)
# [,1] [,2]     [,3]
# [1,] 1.866667  0.8 1.333333
# [2,] 1.200000  0.8 2.000000
# [3,] 1.066667  1.6 1.333333

# Scenario 2: Cache Stored
# > cacheSolve(make_cache)
# getting caced data
# [,1] [,2]     [,3]
# [1,] 1.866667  0.8 1.333333
# [2,] 1.200000  0.8 2.000000
# [3,] 1.066667  1.6 1.333333

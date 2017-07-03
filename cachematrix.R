## This code creates invertible matrices that can cache their inverses 
#and computes the inverse of these special matrices. If the inverse is calculated and the matrix has not changed,
#the inverse is retrieved from the cache. Else, it is computed. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set<- function(y){
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInv <- function(Inv_given) Inv <<- Inv_given
  getInv <- function() Inv
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above, either by 
#retrieving it from the cache or setting it to the cache. 

cacheSolve <- function(x, ...) { #The input to this function is the output of makeCacheMatrix
  Inv <- x$getInv()
  if(!is.null(Inv)){
    message("getting cached data")
    return(Inv)
  }
  matrix_wanted <- x$get()
  Inv <- solve(matrix_wanted)
  x$setInv(Inv)
  Inv
}

 
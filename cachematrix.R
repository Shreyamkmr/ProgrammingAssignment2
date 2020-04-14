## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
 
   set <- function(y){
    x <<- y
    j <<- NULL
  }

     get <- function()x   ## Method the get the matrix
  
     setInverse <- function(inverse) j <<- inverse  ## Way to set the inverse of the matrix
  
     getInverse <- function() j  ## Way to get the inverse of the matrix
  
     list(set = set, get = get, setInverse = setInverse,getInverse = getInverse)
  }
  ## Back a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()  ##geting inverse value(can be NULL or calculated inverse)
  
  if(!is.null(j)){      ##if inverse is already calculated return j from cache
    message("From cached data")
    return(j)
  }
 
   mat <- x$get()       ##getting matrix x
  
   j <- solve(mat,...)  ##for NULL J , calculating the inverse
  
   x$setInverse(j)      ##setting the inverse to the cache
  
   j                    ##returning the inverse
}

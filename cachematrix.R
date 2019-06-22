## There are 2 functions namely makeCacheMatrix and cacheSolve that is used to  
## cache the inverse of a matrix. 

## The makeCacheMatrix function sets and gets the value of a vector and also
## sets and gets the value of a inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
inv_mat <- NULL
set <- function(y){
          x <<- y
    inv_mat <<- NULL
}

get <- function() x
setinverse <- function(inverse) inv_mat <<- inverse
getinverse <- function() inv_mat
list (set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}

## The following function gets the inverse of the matrix from the cache

cacheSolve <- function(x, ...) {
        inv_matrix <- x$getinverse()
        if(!is.null(inv_mat)){
            message("getting cached data")
            return(inv_mat)
        }
        mat <- x$get()
        inv_mat <- solve(mat, ...)
        x$setinverse(inv_mat)
        inv_mat
        }

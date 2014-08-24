## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## setting matrix value   
    im <- NULL
    set <- function(y) {
    x <<- y
    im <<- NULL
  }
  
  ## getting matrix value 
  get <- function() x
  
  ## setting inverse of matrix
  set_inv_matrix <- function(solve) im <<- solve
  get_inv_matrix <- function() im
  
  ## getting inverse of matrix
  list(set = set,  
       get = get, 
       set_inv_matrix = set_inv_matrix,
       get_inv_matrix = get_inv_matrix)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not 
##changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
  ## getting inverse of the matrix        
  im <- x$get_inv_matrix()
  
  ## checking if there is the matrix already 
  if(!is.null(im)) {
    message("getting matrix from cache.....")
    return(im)
  }
  else{
    ## if not in cache , then get the inverse of the matrix   
    mdata <- x$get()
    im <- solve(mdata, ...)
    ## set the inverse of the matrix 
    x$set_inv_matrix(im)
    return(im)
  }
  
}

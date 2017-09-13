##################
## week 3
## programming Assignment 2
##################

makeCacheMatrix <- function(x=matrix()){
      # creates a special "matrix" object that can cache its inverse
      
      # if the matrix is not sqared redefine dimensions
      if (!(ncol(x) == nrow(x))){
            print("nrow ncol will be redefined to ge a squared matrix")
            n_row_col=length(x)/2 # if missing nrow and ncol we assume a square matrix, calc ncol=nrow
            x <- matrix(x, ncol=n_row_col, nrow=n_row_col)#define matrix including number of rows and columns
      }
      
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      
      list(set = set,
           get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}

cacheSolve <- function(x, ...){
      # computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
      # If the inverse has already been calculated (and the matrix has not changed), 
      # then the cachesolve should retrieve the inverse from the cache
      
      m <- x$getsolve()             # get inverse from makeCacheMatrix
      if(!is.null(m)) {             # if inverse was already calculated, use cached data instead of recalculation
            message("getting cached data")
            return(m)
      }
      data <- x$get()               # if inverse not yet calculated, get matrix from makeCacheMatrix
      m <- solve(data)              # calculate inverse of the matrix
      x$setsolve(m) 
      m
}
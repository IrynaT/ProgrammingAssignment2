makeCacheMatrix <- function( x = matrix() )
{
  i  <- NULL    # set up LOCAL i as NULL for sure it will hold inverse matrix
  set  <- function(y)
  {
    x <<- y    ## copy data from entry matrix "y" into GLOBAL martix "x"
    i <<- NULL ## set up GLOBAL i as NULL for sure we did not invert the GLOBAL matrix "x" yet
  }
  get  <- function() { x }  # returns original matrix
  setinverse  <- function(inverse) 
  {
    i  <<- inverse # "inverse" is an entry matrix that we calculate as inversed 
                   # and copy its data into GLOBAL matrix "i"
  }
  
  getinverse  <- function() { i } # returns inversed matrix 
  
  list(set= set, get = get, 
       getinverse = setinverse, 
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) # same matrix (GLOBAL object) comes into the function to be processed
{
  copy_of_i  <- x$getinverse() # gets data from GLOBAL matrix "i" into local varaible
  
  if ( !is.null( copy_of_i ) ) # if it has not NULL data, so it is already processed  
  {
    message("getting cached data")
    return(i)
  }
  original_entry_x  <- x$get()                 # gets data from GLOBAL original matrix "x" into local matrix "i"
  inversed_entry_x  <- solve(original_entry_x,...) # solve() function inverses matrix
  x$setinverse(inversed_entry_x)               # sets up GLOBAL inversed matrix "i"
  x$getinverse()
}

## makeCacheMatrix() and cacheSolve() explore cacheing and lexical scoping in R.
## The code structure and many object names in this file were layed out by Dr. R Peng.
## Major thanks to classmate Randeep Grewall for helping to understand how components of Dr. Peng's code actually work.
## I hope this file doesn't contain too many comments. I need them (and any forthcoming edits) for learning.

## makeCacheMatrix performs many tasks, including these key tasks:
##   Makes methods (i.e. functions) available to another function, cacheSolve()
##   Receives a cached value of a matrix inverse from cacheSolve() via the global environment

makeCacheMatrix <-function(x = matrix()) {
  m <- NULL                   		## initialize "m" as NULL 
  				## "m" eventually receives the cached inverse
  set <- function(y) {        		## "set" passes a matrix to "x" in the parent environment
    x <<- y                   		## "y" is initialized by the first argument in makeCacheMatrix()
    m <<- NULL
  }
  get <- function() x         		## get is a function that enables 
                              			## cacheSolve() to retrieve the value of "x" from  
                              			## the calling environment in makeCacheMatrix()   
  setinverse <- function(solve) m <<- solve        
                              			## setinverse is a function to be used by cacheSolve() 
                              			## takes "m" from parent env. wherever setinverse is called
                              			## calls the solve() function to find the inverse of "m"
  getinverse <- function() m        	## enables cacheSolve to retrieve m from parent environment
  list(set = set, get = get,        		## returns a list of four methods (i.e. functions)
       setinverse = setinverse,
       getinverse = getinverse)
                              			## Call class(makeCacheMatrix(Z)) and press Enter to confirm a list is returned.
}


## cacheSolve() performs many tasks, including these key tasks:
##   Receives methods and the value of matrix "x" from makeCacheMatrix
##   Returns the matrix inverse to the global environment as "m"

cacheSolve <- function(x, ...) {
  m <- x$getinverse()         		## a local variable "m" (thanks to Randeep Grewal for that insight)
                                     		## receives getinverse method; retrieves "m" from parent 
                              			## env., which is the global environment in this case
  if(!is.null(m)) {           		## checks whether "m" is NULL
    message("getting cached data")        ## if "m" is NOT NULL, a message is printed
    return(m)                             	## and the cached "m" is returned
  }
  temp <- x$get()             		## creates local storage for matrix "x"
                              			## "get" method assigns matrix "x" to "temp"
  m <- solve(temp, ...)       		## calls solve() to find inverse of "temp" 
                              			## assigns inverse of "temp" to "m" in calling environment
  x$setinverse(m)             		## copies inverse from "m" to "x"
  
  m                           		## returns inverse to "m" in the global environment
}

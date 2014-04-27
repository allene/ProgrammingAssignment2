##
##  Matrix inversion is usually a costly computation these following two utility functions
##  can be used to cache a matrix and its inverse. 
##
##
##  makeCacheMatrix: This function creates a special "matrix" object that can store 
##  information about its inverse.
##

makeCacheMatrix <- function(x = matrix()) {
       #for first time set matrix to NULL
       invMtrix <- NULL
	   #store original matrix 
       set <- function( y ) {
			#set global value	
              x <<- y
			#clear 	
              invMtrix <<- NULL
       }
	   #get the value if yes get the value
       get <- function() x
	   #set inverse matrix
       setinverse <- function(inverse) invMtrix <<- inverse
	   #get inverse matrix
       getinverse <- function() invMtrix
       list(set = set, get = get,  setinverse = setinverse,  getinverse = getinverse)

}

##  cacheSolve: This function computes the inverse of the special "matrix" returned by
## another utility function makeCacheMatrix . If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache.
## It will not re calculate inverse.
##  

cacheSolve <- function(x, ...) {
       #get inverse
	   invMtrix  <- x$getinverse()
	   #check if inverse already exists
       if(!is.null(invMtrix )) {
			#no need to calculate again
              message("Inverse caluculation skipped. returning previously cached inverse")
              return(invMtrix )
       }
	   #for first time, calculate inverse
       newInvMtrix <- x$get()
       invMtrix <- solve(newInvMtrix)
       x$setinverse(invMtrix)
	   #return inverse
       invMtrix
}

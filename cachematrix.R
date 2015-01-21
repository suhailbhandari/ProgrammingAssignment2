# ‘makeCacheMatrix’ creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
m <- NULL # sets the value of ‘m’ to NULL. It provides a default
set <- function(y) { 
 x <<- y 
 m <<- NULL # initializes the value of ‘m’ to NULL
 } 
   get <- function() x 
   setinverse<- function(inverse) m <<-inverse 
   getinverse <- function() m 
   list(set = set, get = get, 
     setinverse = setinverse, 
     getinverse = getinverse) 
} 
# ‘cacheSolve’ computes the inverse of the matrix returned by ‘makeCacheMatrix’ above. 
# If the inverse has already been calculated and no changes are made then ‘cacheSolve’ # will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) { 
  m <- x$getinverse() # if an inverse has already been created, it fetches it
  if (!is.null(m)) { # if we have an inversion result
   message("getting cached inverse matrix") 
   return(m) 
} else { 
   m <- solve(x$get()) 
   x$setinverse(m) # set inverse to ‘m’
   return(m) 
 } 
}


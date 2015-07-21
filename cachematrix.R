## This is the answer to Assignment 2 of the course "R programming" on Coursera.org
## Author - Bo Fan



## The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse
## This function creates a special "vector", which is really a list containing a function to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse


makeCacheMatrix <- function(x = matrix()) { 
    m <- NULL
    set <- function(y) {
      ##set the value of the matrix
      x <<- y
      m <<- NULL
    }
    get <- function() x   ##get the value of the matri
    setinv <- function(inv) m <<- inv   ##set the value of the inverse
    getinv <- function() m    ##get the value of the inverse
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
} 




## The function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
## Return a matrix that is the inverse of 'x' 
    m <- x$getinv()
    if(!is.null(m)) {
    ##if the inverse is saved in the cache, it will be retrieved from the cache
        message("getting cached data")
    return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
} 

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                        #initialize the inverse to NULL at every call of makeCacheMatrix
        set <- function(y) {                  
                x <<- y                                    #setting the value of x
                inv <<- NULL                               #as a new object x is ed, the value of inv has to be reset to NULL
        }
        get <- function() x                                #return the value of the matrix x that we want to inverse
        setinverse <- function(inverse) inv <<- inverse    #store the value of the inverse in cache
        getinverse <- function() inv                       #return the value of the inverse stored in the cache
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()                         #get the value of the inverse from the object x
        if(!is.null(inv)) {                           #if the inverse is not NULL then we get it from cache
                message("getting cached data")
                return(inv)                           #the inverse stored in cache is returned
        }                                        
        data <- x$get()                               #if the inverse is not in the cache (inv=NULL), then we get the value of the matrix
        inv <- solve(data, ...)                       #and the inverse is calculated
        x$setinverse(inv)                             #then inverse is stored in cache
        inv                                           #finally the inverse is returned
}



#converts a matrix into a cached matrix. Stores original matrix in parent frame in a variable called x
#also attaches helper functions. 
makeCacheMatrix <- function(x = matrix()) {

    inv = NULL
    set = function(y) {
         x <<- y # cache the matrix in parent frame or global
        inv <<- NULL #null out the inverse in parent/global just as a precaution
    }
    get = function() x # return the cached matrix
    setinv = function(inverse) inv <<- inverse # cache the inverse
    getinv = function() inv # return the inverse from parent/global frame
    list(set=set, get=get, setinv=setinv, getinv=getinv) # named value list to access objects
}

#return the inverse of the input matrix. 
#First, check the cache. If it does not exist, calculate
#the inverse, cache it and return it. 
cacheSolve <- function(x, ...) {
    inv = x$getinv()
    
    # If cached, return from cache
    if (!is.null(inv)){
        return(inv)
    }
    
    # not in cache, calculate inverse
    mat = x$get()
    inv = solve(mat, ...)
    
    #cache inverse
    x$setinv(inv)
    
    #return it
    return(inv)
}

##commented test code
#x = matrix(c(1,20,34,41,5,66,71,8,9), nrow=3, ncol=3)
#mt_x<-makeCacheMatrix(x)
#minv<-cacheSolve(mt_x)
#print(minv)
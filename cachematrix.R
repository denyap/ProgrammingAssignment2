## The 2 functions, makeCacheMatrix and cacheSolve make a Cache Matrix and solve a Cache Matrix (find the inverse of the matrix)
## This functions work because of the <<- symbol, which stores the values in a different environment, unlike the <- assign symbol
## which os based on lexical scoping

## makeCacheMatrix makes a list of 4 functions, taking a (square) matrix as a formal argument
makeCacheMatrix <- function(x = matrix()) {
    
    ## 1 the inv variable is initialized, inv stores the solution (inverse) of the matrix
    inv <- NULL
    
    ##2 the set() function is defined, set() takes the matrix value and stores it in a different environment, and sets inv to NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    ##3 the get() function is defined, get() function out puts the stored value of x
    get <- function() x
    
    ##4 the setinverse() function is defined, setinverse() function takes the solution and stores in inv
    setinverse <- function(inverse) inv <<- inverse
    
    ##5 the getinverse() function is defined, the getinverse() function outputs the inv value
    getinverse <- function() inv
    
    ##6 the list of functions is created, when makeCacheMatrix is called, set stores the value of matrix in x,
    ## and sets value of inv to NULL
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
    
}


## cacheSolve will solve a makeCacheMatrix matrix, and will return the cache value if it has already been solved previously

cacheSolve <- function(x, ...) {
    
    ##1 The inv in this function is obtaineed, by calling the $getinverse() function
    inv <- x$getinverse()
    
    
    ##2 If previously solved, the function does not solve the solution instead, and instead returns and prints the stored value
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ##3 If not solved, the variable data is assigned the value of x
    data <- x$get()
    
    ##4 the inv variable is assigned the solved value
    inv <- solve(data)
    
    ##5 the solved value is stored in the external environment
    x$setinverse(inv)
    
    ##6 the solved value is also returned
    inv
}
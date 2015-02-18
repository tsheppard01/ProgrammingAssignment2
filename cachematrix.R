## The function 'makeCacheMatrix' creates an object 
## which contains a matrix 'x' and can cache the inverse 
## of 'x'.
## The function 'cacheSolve' returns the cached inverse of 
## 'x' or creates the cached value if is has not yet been 
## calculated


## This function takes a matrix 'x' and creates an
## object which can cache the inverse of 'x'.  

makeCacheMatrix <- function(x = matrix()) {

        ## Initialise a variable to hold the inverse of 'x'
        ## to NULL.  
        inv <- NULL

        ## Define a function which allows us to set the matrix 'x'
        set <- function(y){

                ## Set matrix 'x' equal to 'y'
                x <<- y

                ## Reset inverse of 'x' to NULL  - as x has 
                ## changed the inverse will also have changed
                inv <<- NULL
        }

        ## Define a function to return the matrix 'x'
        get <- function() x
    
        ## Define a function which sets the cached inverse of 'x'
        setinverse <- function(inverse) inv <<- inverse
    
        ## Define a function to return the cached inverse of 'x'
        getinverse <- function() inv

        ## Return our object - in R lists can be used as objects
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This function returns the inverse of the matrix 'x' 
## using the object created by the function 'makeCachMatrix'.
## We first check if the inverse of the matrix has
## been cached. If yes it returns the cache, else we
## calculates the inverse, cache it then return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        ## Get the cached value of the inverse
        inv <- x$getinverse()

        ## If inv is not null then cache exists, return cached value
        if(!is.null(inv)){

                message("Getting the cached matrix inverse...")
                return(inv)

        } else {

                message("Matrix inversed not cached, calculating...")

                ## If inv is null then get the matrix
                data <- x$get()

                ## Find inverse of the matrix
                inv <- solve(data,...)

                ## Cache the calculated inverse
                x$setinverse(inv)
 
                ## Return the calculated inverse
                return(inv)
        }
}

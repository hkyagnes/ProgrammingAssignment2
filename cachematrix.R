## The purpose of the following functions are to cache the inverse of a Matrix. There are two main functions here, 1) 
## makeCacheMatrix and 2) cacheSolve. 


## makeCacheMatrix is a function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {            ### ‘x’ is the input argument, and it is the matrix being inverted
i <- NULL                                              ### ‘i’ is variable that will store the inverse of the matrix, it is set to be ‘NULL’

        set <- function(y) {                           ### The ’set’ function is inside the makeCacheMatrix function, ‘y’ will take the argument passed from ‘makeCacheMatrix’
                x <<- y                                ### Inside ‘set’ function, the value of both ‘x’ and ‘i’ will be assigned to the parent environment (with <<-)  
                i <<- NULL
        }
        
        get <- function() x                            ### ‘get’ is an anonymous function which assign the matrix to it
        
        setinverse <- function(inverse) i <<- inverse  ### The inverse of the matrix is assigned to ’i’ to the parent environment, (the inverse is calculated from cacheSolve function)
        
        getinverse <- function() i                     ### 'getinverse’ is an anonymous function which return the stored inverse of the matrix        
        
list(set = set, get = get,                             ###Lists of variables inside ‘makeCacheMatrix’
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve is a function computes the inverse of the special "matrix" returned by makeCacheMatrix above. The function 
## first check if the inverse is already calculated, if so it will return the stored inverse of the matrix. If not, the 
## second part of the function will calculate the inverse of the matrix, return the value and store it in the parent 
## environment, so the next time when a new matrix come in, it can be checked.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()                            ### Set ‘i’ value from x environment, ‘getinverse’ function
         if(!is.null(i)) {                             ### If ‘i’ is not NULL, then return ‘i’
                message("getting cached data")
                return(i)
        }
                                                       ### If the inverse is not calculate:
        data <- x$get()                                ### 'data' is defined to look for the matrix(from makeCacheMatrix function)
        i <- solve(data, ...)                          ### Calculate the inverse of the matrix
        x$setinverse(i)                                ### Set the value of the inverse matrix to the parent environment
        i                                              ### Print out the result “i”
}

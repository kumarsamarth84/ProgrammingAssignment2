## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to store and retrieve the original matrix and inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
# Store the value of Matrix (in variables "set" and "get") passed as a variable in makeCacheMatrix function
    set <- function(y) {                
      x <<- y
      i <<- NULL
    }
    get <- function() x
        
 # Store the inverse value ("inverse" variable which is coming from variable "i" of cacheSolve function) of Matrix (in variables "setinverse" and "getinverse")
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
        
 # Create a list of the value of the Matrix and Inverse Matrix
 # a list is created to retrieve the seperate values (subset)(such as, set, get, setinverse, getinverse variable separately) 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

# Function is to identify whether inverse value of the matrix exist in the makeCacheMatrix
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
# x is the list of the values of Matrix and inverse Matrix        
# search for the Inverse Matrix from the list created in makeCacheMatrix
    i <- x$getinverse()
        
# Identify whether a Inverse Matrix (i) has been retrieved from the list created in makeCacheMatrix function
# If inverse of a Matrix is retrieved, return the inverse value ("i") as an output and exit the cacheSolve function 
    if(!is.null(i)) {
      return(i)
    }
 # If inverse of a Matrix is NOT retrieved then create an inverse of a matrix using solve function and store in the list of makeCacheMatrix function
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i                   # print the output i.e. inverse matrix
}

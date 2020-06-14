# Function to create the inverse of a input matrix and cache its results

# Takes a matrix as an input
makeCacheMatrix <- function(x = matrix()) { 
        Matrix_inverse <- NULL               
        set <- function(y) {                 
                x <<- y                     
                Matrix_inverse <<- NULL     
        }
        get <- function() x                 
      # Set value of matrix and get the inverse   
        setinverse <- function(inverse) Matrix_inverse <<- inverse  
        getinverse <- function() Matrix_inverse                     
        list(set = set, 
             get = get, 
             setinverse = setinverse,  
             getinverse = getinverse)   
        
}


# Function to retrive the inverse matrix if already calculated and stored in cache
# If not it will calculate the inverse matrix and return the results
cacheSolve <- function(x, ...) {
        # Calculate a matrix that is the inverse of 'x' 
        Matrix_inverse <- x$getinverse()
        if(!is.null(inv)) {   
                message("retrieving cached inverse matrix") # Print message if matrix already exist   
                return(Matrix_inverse)
        }
        # If data is NULL use the input matrix and calculate the inverse matrix
        data <- x$get()
        Matrix_inverse <- solve(data, ...)
        x$setinverse(Matrix_inverse)
        Matrix_inverse
}
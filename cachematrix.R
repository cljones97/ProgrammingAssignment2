## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
#Initialize i so that it can be the inverse property 
        inverse_i <- NULL
       
        #Set the matrix 
        set <- function(y) {
                x <<- y
                inverse_i <<- NULL
        }
       
        #Gets the matrix
        #Which is already a parameter so we just call m
        get <- function() {
                x
        }
       
        #Set the inverse of the matrix
        #Since i is the inverse property we just set inverse equal to i
        setInverse <- function(inverse) {
                inverse_i <<- inverse
        }
       
        #Get the inverse of the matrix
        #Since we already set the inverse we just get the value that 
        #stores the inverse matrix, aka i
        getInverse <- function() {
                inverse_i
        }
       
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return a matrix that is the inverse of 'x'
        inverse_i <- x$getInverse()
       
        ##Return inverse if it is already set
        if (!is.null(inverse_i)) {
                message("Getting cached Data")
                return(inverse_i)
        }
       
        data <-x$get()
        ##Calculates inverse
        inverse_i <- solve(data)
        ##Sets inverse
        x$setInverse(inverse_i)
        inverse_i
}

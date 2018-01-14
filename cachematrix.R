
rm(list=ls())

# ------------------------- Programming Assignment 2: Lexical Scoping ------------------------------------------------------------------

#       The first function makeCacheMatrix defines the individual functions used to store the original matrix and its inverse.
#       Each function is saved as an element of a list.

#       The second function cacheSolve first checks whether there the list of functions called dat exist to avoid errors later in the code.
#       If dat does not exist, then the function uses the input matrix x to create it. The function then retrieves the original data stored
#       in dat and compares it to the input matrix x. If they are the same, then the inverse matrix is retrieved and a message saying 
#       "getting cached data" is printed to the console. If the x has different dimensions or has any different elements, then the inverse
#       matrix is created from x and cached using the first function.

#       To test: run both functions, then run the 2x2 matrix test twice. You will notice the results are the same both times except the
#       second time will also say "getting cached data". This shows that the functions are working as intended. The 3x3 example does the same
#       but proves that the functions are capable of dealing with different sized matrices.

# --------------------------------------------------------------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        #               Caches the input matrix x from cacheSolve to makeCacheMatrix environment
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        #               Retreives original matrix x to pass to cacheSolve
        get <- function() x
        
        #              Caches the inverse matrix calculated by cacheSolve
        setinverse <- function(inverse) inv <<- inverse
        
        #              Retrieves the inverse matrix inv to pass to cacheSolve
        getinverse <- function() inv
        
        #               Store the above functions as a list
        list(get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x) {
        
        # Check whether dat exists, if not then create it from input matrix x
        if( sum(grepl("dat", ls(environment(cacheSolve)))) == 0) {
                dat <<- makeCacheMatrix(matrix(x))
                dat$set(x)
        }
        
        #               Check the cached matrix has same dimensions and elements are identical. If not then create with x.       
        old <- dat$get()
        if(dim(old)[1] != dim(x)[1] | dim(old)[2] != dim(x)[2]) {dat <<- makeCacheMatrix(x);
        } else {
                if(sum(old!=x) > 0) {dat <<- makeCacheMatrix(x)}            
        }   
        
        #               Retrieve inverse matrix. If exists return and exit, otherwise continue function
        inv <- dat$getinverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        #               Calculate inverse then cache and return results
        inv <- solve(x)
        dat$setinverse(inv)
        inv
}
# ------------------------- Run example ------------------------------------------------------------------
#       Run either of these examples twice. The first time will calculate the inverse matrix and cache it
#       while the second time will just retrieve it. You know its being retrieved when the function returns
#       the message "getting cached data". 
#       Do the same with the 3x3 example to show that the function works regardless of the matrix dimensions
#       as long as its invertible.

#       2 x 2 matrix test
cacheSolve(x = matrix(1:4,2,2))

#       3 x 3 matrix test
cacheSolve(x = matrix(c(1,5,6,3,3,6,2,9,64),3,3))
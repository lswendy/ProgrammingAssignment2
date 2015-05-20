
# makeCacheMatrix generates a special list containing a function to
# set the value of a matrix (set),
# get the value of a matrix (get),
# set inverse of a matrix (setmatrix),
# get inverse of a matrix (getmatrix)

# makeCacheMatrix = a matrix, saved in x
makeCacheMatrix <- function(x = matrix()) {
        
        # initial condition: inverse of a matrix = NULL,
        # used later in cacheSolve to test if(!is.null(m)) (result=getting cached inverse or calculating it) 
        m <- NULL
        
        # to set a new matrix (y) to x. With the new matrix its inverse matrix should be initialized
        # A funciton to set a new value for the underlying matrix
        # <<- operator is used to assign a new matrix to an object in an environment that is different from 
        # the current environment. makeCacheMatrix and cacheSolve below are used to create a special object 
        # that stores a matrix and its inverse matrix.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # retrieve the stored matrix
        get <- function() {
                x
        }
        
        # set the inverse matrix of x upon the call by cacheSolve   
        # <<- operator is used to create a special object that stores inverse matrix of x (calculated through the cacheSolve)
        setmatrix <- function(solve) {
                m <<- solve
        }
        
        # obtain cached inverse matrix of x
        getmatrix <- function() {
                m
        }
        
        # return a list functions. thanks to this declaration the $ operator below allows an access to the functions
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


# cacheSolve returns saved inverse matrix if it was already calculated (calculate inverse matrix otherwise)
cacheSolve <- function(x = matrix(), ...) {
        
        # Obtain the inverse matrix of x (specially defined in x)
        m <- x$getmatrix()
        
        # load saved inverse matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # if no inverse matrix stored already,
        # Use get() to obtain the underlying matrix
        matrix <- x$get()
        
        # calculate the inverse of the underlying matrix
        m <- solve(matrix, ...)
        
        # and save it
        x$setmatrix(m)
        
        # return the invese matrix
        m
}



# test
# > mat <- matrix(c(2,4,3,1), nrow=2, ncol=2, byrow=TRUE)
# > y <- makeCacheMatrix(mat)
# > cacheSolve(y)
# [,1] [,2]
# [1,] -0.1  0.4
# [2,]  0.3 -0.2
# > cacheSolve(y)
# getting cached data
# [,1] [,2]
# [1,] -0.1  0.4
# [2,]  0.3 -0.2




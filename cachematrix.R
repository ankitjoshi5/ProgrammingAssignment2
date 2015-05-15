## The makeCacheMatrix() function gives the existing matrix capabilities like getting and setting
## values for the original matrix and getting and setting values for the inverse of the matrix.
## The cacheSolve() function computes the inverse of a particular matrix if it has not yet been
## computed. If the inverse of a matrix has been computed then it returns the cached value.

## The makeCacheMatrix() function gives the existing matrix capabilities like getting and setting
## values for the original matrix and getting and setting values for the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        #Initializing the Inverse Matrix to NULL
        matInv <- NULL
        
        ## Setter Method for the Original Matrix
        setMatrix <- function(currMatrix)
        {
                x <<- currMatrix
                matInv <<- NULL
        }
        
        ## Getter Method for the Original Matrix
        getMatrix <- function()
        {
                x
        }
        
        ## Setter Method for the Inverse Matrix
        setInvMatrix <- function(invMatrix)
        {
                matInv <<- invMatrix
        }
        
        ## Getter Method for the Inverse Matrix
        getInvMatrix <- function()
        {
                matInv
        }
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## The cacheSolve() function computes the inverse of a particular matrix if it has not yet been
## computed. If the inverse of a matrix has been computed then it returns the cached value.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        ## Try to get the inverse of the input matrix if it exists
        invMat <- x$getInvMatrix()
        
        ## If the Inverse is present, return it.
        if(!is.null(invMat))
        {
                message("Getting Cached Inverse of this matrix")
                return (invMat)
        }
        
        ## If the Inverse is not present, compute it.
        mat <- x$getMatrix()
        invMat <- solve(mat)
        x$setInvMatrix(invMat)
        
        ## Return the result.
        invMat
}
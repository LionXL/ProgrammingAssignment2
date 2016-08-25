## R Programming asigment 2: Lexical scoping
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## 
## This project will create two funtions:
## 		1. makeCacheMatrix: This function creates a special "matrix" object that 
##			 can cache its inverse.
##
##    2. cacheSolve: This function computes the inverse of the special "matrix" 
##       returned by makeCacheMatrix above. If the inverse has already been calculated 
##       (and the matrix has not changed), then retrieve the inverse from the cache.
##       will use the solve function to create the inverse matrix. 
##			 Will assume that the matrix supplied is always invertible


## Function: makeCacheMatrix
## creates function based on list
## 1. set matrix
## 2. get matrix
## 3. set the inverse matrix
## 4. get the inverse matrix

makeCacheMatrix <- function(varMatrix = matrix()) {

        varCacheMatrix <- NULL
        set <- function(varSetMatrix) {
                varMatrix <<- varSetMatrix
                varCacheMatrix <<- NULL
        }
        get <- function() varMatrix
        setinverse <- function(inverse) varCacheMatrix <<- inverse
        getInverse <- function() varCacheMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getInverse = getInverse)

}


## Function: cacheSolve 
## receives matrix into varMatrix
## test if varInverse (inverted matrix) has been created and cached
## if varInverse is null, invert varMatrix solve() Function
## return VarInverse (inverted matrix)

cacheSolve <- function(varMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## retrieve matrix from passed in object variable
        varInverse = varMatrix$getInverse()
        
        ## If the varInverse is not null, then the inverse matrix has been created and cached.
        ## Then we will only retrieve the stored matrix and then return it. 
        if (!is.null(varInverse)){
            return(varInverse)
        }
  
        ## If varInvere is null then inverse matrix has not been created.
        ## we will then create the inverted matrix and cache it.       
        MatrixData.data = varMatrix$get()
        varInverse = solve(MatrixData.data, ...)
        
        ##update cached matrix
        varMatrix$set(varInverse)
        
        ## return the inverted matrix
        return(varInverse)
}
   
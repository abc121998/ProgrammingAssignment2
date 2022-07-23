## This program caches a given matrix and its inverse

##builds a special kind of matrix with function to get/set the matrix and 
##get/set its inverse (caches to improve performance)
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        #changes the original matrix to a new one and nulls the set inverse
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        
        #returns the original matrix
        get <- function() x
        
        #sets the inverse of the matrix and caches it
        setinverse <- function(solve) i <<- solve
        
        #returns the matrix's inverse
        getinverse <- function() i
        
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


##if not already cached, calculate the inverse of a CacheMatrix and sets it
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        
        #ends function if the inverse matrix exists
        if (!is.null(i)){
                message('getting cached data')
                return(i)
        }
        
        #gets matrix, solves it to find the inverse, and the sets the inverse
        matrix <- x$get()
        i <- solve(matrix)
        x$setinverse(i)
        
        ## Return a matrix that is the inverse of 'x'
        i
}


#TEST OUTPUT
###########################################################

#> # A simple matrix m1 with a simple matrix inverse n1
#> m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# > #actual inverse of m1
#         > n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
#
# > #second test matrix
#         > m2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
# 
# > #actual inverse of m2
#         > n2 <- matrix(c(3,1,7,5), nrow = 2, ncol = 2)
# 
# > #make, inverse, and cache
#         > myMatrix_object <- makeCacheMatrix(m1)
# 
# > #Get matrix
# > myMatrix_object$get()
# [,1]  [,2]
# [1,]  0.50 -1.00
# [2,] -0.25  0.75
# 
# > #Correct Inverse Matrix
# > n1
# [,1] [,2]
# [1,]    6    8
# [2,]    2    4
# 
# > #Calculated Inverse Matrix
# > cacheSolve(myMatrix_object)
# [,1] [,2]
# [1,]    6    8
# [2,]    2    4
# 
# > #Get cached inverse
# > cacheSolve(myMatrix_object)
# getting cached data
# [,1] [,2]
# [1,]    6    8
# [2,]    2    4
# 
# > #Set to new matrix (m2)
# > myMatrix_object$set(m2)
# 
# > myMatrix_object$get()
# [,1]   [,2]
# [1,]  0.625 -0.875
# [2,] -0.125  0.375
# 
# > #Correct Inverse Matrix
# > n2
# [,1] [,2]
# [1,]    3    7
# [2,]    1    5
# 
# > #Calculated Inverse Matrix
# > cacheSolve(myMatrix_object)
# [,1] [,2]
# [1,]    3    7
# [2,]    1    5
# 
# > #Get cached inverse
# > cacheSolve(myMatrix_object)
# getting cached data
# [,1] [,2]
# [1,]    3    7
# [2,]    1    5

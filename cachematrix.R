## The two functions are designed to cache the contents of an inverted matrix
## rather than calculate it repeatedly
	

## makeCacheMatrix does the following. Note that it is a list of four components:
# 1. Sets myinv to null
# 2. Function code for set which sets up input matrix
# 3. Function code for get which extracts x
# 4. Function code for setinverse which sets myinv to inverse (setting value of inverse of a matrix)
# 5. Function code for getinverse which extracts myinv
	

makeCacheMatrix <- function(x = matrix()) {
    myinv <- NULL
    set <- function(y) {
        x <<- y
        myinv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) myinv <<- inverse
    getinverse <- function() myinv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
	

## cacheSolve does the following:
# 1. Extracts the getinverse list component into inv
# 2. Checks to see if it is null or not (ie whether or not it has been cached already)
# 3. If it is not null, then the cached matrix is returned
# 4. If it is null (ie has not been cached), then the get list component is invoked
#   which feeds input x into data, then creates inv which is the inverse of it (invoked using
#   the solve function). Lastly, setinverse list component is called which sets myinv to 
#   the matrix that is passed - inv in this case
# 5. Outputs the value of inv at the end. Either the cached value or if not cached, the
#   newly calculated value
	

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
	

## Examples of applying these two functions
	

## First example of using
	

# myMat <- cbind(c(2, 3), c(3, 2))     # create a simple 2x2 matrix
# myCache = makeCacheMatrix(myMat)     # invoke makeCacheMatrix
	

# myCache$get()       
	

# returns:
# [,1] [,2]
# [1,]    2    3
# [2,]    3    2
	

# cacheSolve(myCache)         
	

# returns:
# [,1] [,2]
# [1,] -0.4  0.6
# [2,]  0.6 -0.4
	

# cacheSolve(myCache)
	

# returns:
# getting cached data.
# [,1] [,2]
# [1,] -0.4  0.6
# [2,]  0.6 -0.4
	

## Second example of using. Use the set list component to change the input matrix and run through again
	

# NewMat <- cbind(c(2, 4), c(4, 2))     # create another simple 2x2 matrix
# myCache$set(NewMat)                   # feeds in the new matrix using the set list component
# myCache$get()
	

# returns:
# [,1] [,2]
# [1,]    2    4
# [2,]    4    2
	

# cacheSolve(myCache)
	

#returns:
# [1]       [,2]
# [1,] -0.1666667  0.3333333
# [2,]  0.3333333 -0.1666667
	

#cacheSolve(myCache)
	

# returns:
# getting cached data.
	

# [1]       [,2]
# [1,] -0.1666667  0.3333333
# [2,]  0.3333333 -0.1666667


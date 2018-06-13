## makeCacheMatrix define a matrix and finds the Inverse of the Matrix
## cacheSolve checks the Environment to see if Inverse of a passes Matrix is already passed or not
## If it finds a cache value it doesnt calculate the Inverse it directly prints the Inverse Value


## makeCacheMatrix defines a Matrix and calculates its Inverse Value
## Steps to Use:
## Step 1:
## mat<-makeCacheMatrix()
## Step 2: Set the Value of the Matrix calls upon set()
## mat$set(matrix(1:4, nrow=2, ncol=2))
## Step 3: Prints the Matrix on the console calls get()
## mat$get()
## Step 4: Calculate the Inverse by calling Solve() on the matrix using setInverse()
## mat$setInverse(inverseFunc = solve(mat$get()))
## Step 5: Prints the Calculated Inverse value using getInverse()
## mat$getInverse()

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function(){
        x
    }
    setInverse <- function(inverseFunc){
        inverse <<- inverseFunc
    }
    getInverse <- function(){
        inverse
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Checks the Environment to see if the Inverse for the Matrix has already been calculated or not
## If not then calculates the Inverse
## cacheSolve(mat) to run the problem with the matrix as mat

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$getMatrix()
    inverse <- inverseFunc(data, ...)
    x$setInverse(inverseFunc)
    inverse
}

dir.create(tempdir())
## Calculate and cache the inverse of a square matrix

# Define a matrix m1 with a simple matrix inverse n1
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

## Cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<-y
          inv <<-NULL
     }
     get <- function()x
     setinv <- function(inverse) inv <<- inverse
     getinv <- function()inv
     list(set = set, get = get, 
          setinv = setinv,
          getinv = getinv)
}


## Solve the inverse of a matrix x

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv ## Return a matrix that's the inverse of x
}

## Test if the code works to take the inverse of a matrix
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)







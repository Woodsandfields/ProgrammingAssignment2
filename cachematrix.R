## These two functions combined will help you save computational time when working on large datasets. Here those two
## are specifically intended for matrices when you want to compute their inverse (only works if an inverse exists) in case you
## need this inverse several times. In other words, it's all about caching.

## First function is makeCacheMatrix. Pass your matrix to the following function makeCacheMatrix as an argument 
## and store the result in a separate variable which will be then the argument of the second function when needed. 
## The result of this function takes the form of a list which can be extended for further kinds of computations on the matrix. 

        
makeCacheMatrix <- function(x = matrix()) {
m_inverse <- NULL
        set <- function(y){
                x <<- y
                m_inverse <<- NULL
        }
        setinverse <- function(solve) m_inverse <<- solve
        get <- function()x
        getinverse <- function() m_inverse 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}

## The second function is cacheSolve. This is the one you call everytime you need the inverse again. If you call it for the first time, 
## it will both compute the inverse and store it in the cache, indeed inside the vector previously created when you called makeCacheMatrix.
## The argument has to be the separately stored result of makeCacheMatrix that you called before. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inverse <- x$getinverse()
        if (!is.null(m_inverse)){
                 message("getting cached data")
                 return(m_inverse)
         }
        data <- x$get()
        m_inverse <- solve(data, ...)
        message("first time computing it")
        x$setinverse(m_inverse)
        m_inverse
}

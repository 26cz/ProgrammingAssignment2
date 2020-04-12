#Following code taken from Disscusionthread (Simple test matrices for lexical scoping) can be used to create a inverteable matrix then pass it to te cache function and afterwards create an inverse of the matrix
#m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
#myMatrix_object <- makeCacheMatrix(m1)
#cacheSolve(myMatrix_object)

#n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
#myMatrix_object$set(n2)
#cacheSolve(myMatrix_object)
#Following two functions work together to finally return an inversed matrix. 
#In order to reduce computation time, the input matrix is first cached by function one, so if the matrix is the same like before, the inverse does not need to be calculated by the second function, but it can just retrieve the cached inverse of the matrix
#Create Matrix object that can cache its inverse matrix which is created by the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL         #variable initialization for the first call
    set <- function(y){ # function that can be called to set a new matrix, without initializing another instance of the object
        if(identical(x,y)){}#Check if the new matrix is the same like the old matrix, so recomputation would not be necessary and old matrix will be kept
        else{
            x <<- y         #Overwrites the x input variable for the makeCacheMatrix with the newly set matrix
            inv <<- NULL    #Re-initialization is also necessery for newly set Matrix, so recomputation can take place
        }

    }
    get <- function() x #get the current matrix object, either from the first call(as argument to the function makeCacheMatrx), or a later call by argument to the set() function
    setinverse <- function(inverse) inv <<- inverse #function that sets the new inverse matrix to the object setinverse, will be called by the cacheSolve function and overwrites the inv variable
    getinverse <- function() inv #required by the cacheSolve function to get a cached inverse matrix 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)#The list makes all the functions callable by names on the object x and there with calling with x$NAME is possible and more save than x[[POSITION]]
}

#Compute the the inverse of the matrix, but if inverse of matrix has already been computed and matrix itself is the same, then return cached matrix
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()   #Try to get a cached inverted matrix 
    if(!is.null(inv)) {     #Is there a cached inverted matrix?, If yes then the cached matrix is loaded
        message("getting cached data")
        return(inv)
    }
    else {                  #If there is no cached data, then compute the inverse of the matrix and assign it to the variable inv 
        message("computing inversion of matrix")
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv) # overwrite the setinverse object with the newly computed matrix
    }
    inv     #Return/print the inverted matrix
}

## Inversion of matrices can be a costly computation for a very large matrix.
## Instead of performing the inversion on the same matrix again and again, we can perform the inversion onve and  implement caching using "<<-" operator
## The <<- operator can be used to assign a value to an object in an environment that is different from the current environment. 
## Below are two functions that are used to create a special object that stores a matrix and cache's its matrix inverse


## This function creates a special vector or list of functions that can:
# get and set the matrix value
# get and set the inverse matrix value


makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y)
        {
                x<<-y
                inv<<-NULL
        }
        get<- function()x
        getInverse<-function()inv
        setInverse<-function(inv_mat) inv <<- inv_mat
        

     list(set = set, get = get,getInverse = getInverse,setInverse = setInverse)
}

## This function computes the inverse of the given input matrix. 
## If the inverse matrix is already calculated in our environnment(s), the same value (that is cached) is returned to the user. 
## Else a new inverse matrix is calculated and and the new value is returned to the user.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if(!is.null(inv)){
                message("Returning inverse matrix using cached data")
                return(inv)
        }
        input_matrix=x$get()
        inverse_matrix<-solve(input_matrix)
        x$setInverse(inverse_matrix)
        inverse_matrix
}


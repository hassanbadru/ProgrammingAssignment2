
## FUNCTION 'makeCacheMatrix' TAKES IN ORIGINAL MATRIX AS WELL AS COMPUTED INVERSES AND STORES THEM IN CACHE/MEMORY
makeCacheMatrix <- function(x = matrix()) {

        ##INITIALIZES INVERSE MATRIX AS EMPTY
        inv <- NULL
        
        ##SET ORIGINAL MATRIX
        set <- function(y){
                
                ##STORE MATRIX IN "x" AS A GLOBAL VARIABLE
                x <<- y
                inv <<- NULL
        }
        
        ##GET ORIGINAL MATRIX
        get <- function(){
               
                 ##RETURN STORED MATRIX
                x    
        }
       
        ##SET COMPUTED INVERSE MATRIX      
        setInv <- function(inverse){
                
                ##STORE VALUE OF INVERSE IN "inv" AS A GLOBAL VARIABLE
                inv <<- inverse
                
        }
        
        ##GET STORED INVERSE MATRIX
        getInv <- function(){
              
                ##RETURN STORED INVERSE VALUE  
                inv  
        }
        
        ##RETURNING LIST
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}



## cacheSolve returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
       
        
        ##FETCHING EXISTING INVERSE
        inv <- x$getInv()
        
        ##CONDITION IF INVERSE CURRENTLY EXISTS IN CACHE/MEMORY
        if(!is.null(inv)){
                
                ##MESSAGE INDICATING THAT INVERSE ALREADY EXISTS IN CACHE/MEMORY
                message('...fetching existing data from cache...')
                
                return (inv)  ##LOAD THE EXISTING INVERSE MATRIX FROM CACHE/MEMORY & RETURN IT
        } 
        
        ##CONDITION IF INVERSE MATRIX DOESN'T EXIST IN CACHE/MEMORY
        else{
        
                ##FETCH ORIGINAL MATRIX & STORE IN VARIABLE 'data'
                data <- x$get()
        
                ##COMPUTE INVERSE OF THE ORIGINAL MATRIX
                inv <- solve(data)
        
                ##STORE THE COMPUTED INVERSE MATRIX IN CACHE/MEMORY
                x$setInv(inv)
                inv
        }
}


##TEST: BY CALLING ON FUNCTIONS
x <- matrix(c(3,2,0,0,0,1,2,-2,1), 3, 3)
a <- makeCacheMatrix(x)
cacheSolve(a) ##calling on cacheSolve() with matrix 'x' will load from memory after initial computation

y <- matrix(c(2,3,1,7), 2, 2)
b <- makeCacheMatrix(y) ##storing original matrix in 
cacheSolve(b) ##calling on cacheSolve with matrix 'y' will load from memory after initial computation
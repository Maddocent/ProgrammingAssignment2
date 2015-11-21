## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix <- function(x = matrix()) {

#}


## Write a short comment describing this function

#cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
#}


##### MY SOLUTION ####

#Assignment: Caching the Inverse of a Matrix

#Matrix inversion is usually a costly computation and there may be some benefit 
#to caching the inverse of a matrix rather than computing it repeatedly 
#(there are also alternatives to matrix inversion that we will not discuss here). 
#Your assignment is to write a pair of functions that cache the inverse of a matrix.

#Write the following functions:
  
#  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated 
#(and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
#Computing the inverse of a square matrix can be done with the solve function in R. For example, 
#if X is a square invertible matrix, then solve(X) returns its inverse.

#For this assignment, assume that the matrix supplied is always invertible.

#In order to complete this assignment, you must do the following:
  
#  Fork the GitHub repository containing the stub R files at https://github.com/rdpeng/ProgrammingAssignment2 to create a copy under your own account.
#Clone your forked GitHub repository to your computer so that you can edit the files locally on your own machine.
#Edit the R file contained in the git repository and place your solution in that file (please do not rename the file).
#Commit your completed R file into YOUR git repository and push your git branch to the GitHub repository under your account.
#Submit to Coursera the URL to your GitHub repository that contains the completed R code for the assignment.
#Grading

#This assignment will be graded via peer assessment.

## Put comments here that give an overall description of what your
## functions do (see above)

## Write a short comment describing this function

##I have used examples available on Stackoverflow.com to solve this assignment.

#creating 3 test random matirces: Test 1 is singular (not-invertable)

#non-sigular (det >0)
Test2 <- matrix(1:4, nrow = 2, ncol = 2)
det(Test2)

#singular: (det = 0)
Test1 <- matrix(1:64, nrow = 8, ncol = 8)
det(Test1)

# 13*13 samples = 
n <- 13*13
T3 <- sample(1:36,n ,replace=T)
T3 <- as.numeric(T3)

#non-singular: det(Test3) < 0
Test3 <- matrix(T3, nrow = 13, ncol = 13)
det(Test3)


#creating the function makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
#sets the value of the matrix to NULL (creates an empty matrix)
  
 Matrix <- NULL

#defines a function in the makeCacheMatrix function to push the matrix in the argument to the 
#emptymatrix (x = matrix (put in the argument of the function) and y is that matrix stored in the cache) 
#to the cache     
    set<-function(y){
    x<<-y
    Matrix <<- NULL
  }
#search for the empty matrix in the environment and caching the inverse (with solve) 
#of the empty matrix 
      
  get<-function() x
  setmatrix<-function(solve) Matrix <<- solve
  getmatrix<-function() Matrix

#creating a list of the named objects in the cache and setting the 
  #list
  
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
  
}

#generating a function to return a matrix pushed to the above function, 
#stored in the cache, 
#inverted and returned

cacheSolve <- function(x=matrix(), ...) {
  
  Matrix <- x$getmatrix()
  #If the inverse has already been calculated 
  #(and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache:
  # (NOT USED POSSIBLE ALTERNATIVE TO !is.null)
  #if (!exists("Matrix")){
  #message("getting cached data")
  # return(Matrix)}
  # have tried to including a way to check if the matrix is invertable by including 
  # the determinant function, but i failed to do so
  
  determinant <- det(matrix)
  if(determinant==0) stop("Solved Matrix is singular, determinant equals zero")  
  
    if(!is.null(Matrix)){
  message("getting cached data")
      #the matrix is returned from cache
    return(Matrix)
  }
  
  matrix<-x$get()
  #the inputed matrix in the cachSolve function is returned inverted by solve
  Matrix<-solve(matrix, ...)
  
  x$setmatrix(Matrix)
  Matrix
  
 }


###################### TESTING THE FUNCTION ON THE THREE EXAMPLES ########


#testing Matrix "Test1"
CacheMatrix1 <- makeCacheMatrix(Test1)
X <- cacheSolve(CacheMatrix1)
X

#testing Matrix "Test2"
CacheMatrix2 <- makeCacheMatrix(Test2)
Y <- cacheSolve(CacheMatrix2)
Y
#The error created by Test1 is because this matrix is not invertable: det(Matrix) = 0

#testing Matrix "Test3"
CacheMatrix3 <- makeCacheMatrix(Test3)
Z <- cacheSolve(CacheMatrix3)
Z



m1 <- matrix(c(4.0,7.0,2.0,6.0),nrow=2,ncol=2,byrow=TRUE)
cm1 <- makeCacheMatrix(m1)
#cm1$set(m1)
print(cacheSolve(cm1))
print(cacheSolve(cm1))
print(cacheSolve(cm1))

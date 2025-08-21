m <- matrix(1:9, nrow = 3, ncol = 3)
print(m)

# Matrix operations
t(m)        # Transpose
m %*% m     # Matrix multiplication

# Creating a vector
v <- c(1, 2, 3, 4, 5)

# Operations on vectors
v * 2         # Multiply each element by 2
v + c(10,20,30,40,50)  # Element-wise addition

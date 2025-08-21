# Create a data frame
students <- data.frame(
  Name = c("Rakib", "Sara", "Mahi"),
  Age = c(21, 22, 20),
  Score = c(85, 90, 88)
)

print(students)

# Accessing columns
students$Name
students$Score[2]

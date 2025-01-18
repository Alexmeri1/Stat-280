# @author Alexander Meriakri id: #40310155
#Assignment 1
#Just press CTRL+ENTER from now 

#Question 1

#P = 5000, r = 0.05, t = 1 to 20
# I = P*(1+r)^t-P
answer <- c(5000*(1+ 0.05)^(1:20) - 5000)
answer #to print the answer
cat("\014") # Clears the console 

#Question 2

#area formula is a = pi*r^2, r is 3 to 20
areas <- pi * (3:20)^2
areas

cat("\014") # Clears the console 

#Question 3

#initialize the variables
n <- seq(10,40, by = 10)
r <- 1.08
#initialize them with 0's to feel them up later
sigma_sum_exposant <- numeric(length(n))
formula_sum <- numeric(length(n))

#for loop from 1 to 4
for(i in 1:length(n)) {
  
  #At position i we calculate both equation and store them in the appropriate index
  sigma_sum_exposant[i] <- sum(r^( 0:n[i] ) )
  formula_sum[i] <- (1-r^(n[i]+1))/(1-r)
  
}

#Calculates the difference_q3 between the two
difference_q3 <- abs(sigma_sum_exposant - formula_sum)
difference_q3

#Output the comparison_q3 of the two formulas
comparison_q3 <- sprintf("\nWhen n is %f\nThe first sum is: %f\nThe other one is: %f\nThey have a difference_q3 of %d\n",n,sigma_sum_exposant,formula_sum,difference_q3)
cat(comparison_q3) # Prints the comparison_q3 with the \n

#They output the same answer meaning that they are equivalent formulas

cat("\014") # Clears the console 

#Question 4

#initialize the vector with 10 positon's 
vector_for_sigma <- c(numeric(10))

#for loop to calculate each sum
for (y in 1:10){
  vector_for_sigma[y] <- sum(r^( 0:y ) )
}

#print the sum's
vector_for_sigma

cat("\014") # Clears the console 

#Question 5

N <- c(500,1000,2000,4000,8000)

sigma_sum_division <- numeric(length(N))
log_function <- numeric(length(N))

for(j in 1:5){
  
  sigma_sum_division[j] <- sum(1/(1:N[j]))
  log_function[j] <- log(N[j]) + 0.6
}

difference_q5 <- abs(sigma_sum_division - log_function)

comparison_q5 <- sprintf(
  "\nWhen N is %f\nThe sigma function outputs: %f\nThe log function outputs: %f\nThey differ by: %f\n"
  ,N,sigma_sum_division,log_function,difference_q5)

cat(comparison_q5)
#The two equations are roughly the same, with a small error of arround 0.02 on average which is around 1%

#Question 6

#Here we do each, since we want the each of the numbers in the sequence to be printed 5 times
the_same_five_times <- c(rep(1:5, each = 5))
the_same_five_times

#Here we do times, since we want the sequence 5 times 
seq_one_to_five <- c(rep(1:5, times = 5))
seq_one_to_five

#Question 7

#initialize the vector with 25 positions
vector_for_sequence <- numeric(25)

#To add me in the for loop 
add <- c(0,5,10,15,20)

for(b in 1:5){
  
  #those are the values that the function creates
  #from 1 * b to 4+b, which creates 5 values from 1 to 5 in the first iteration 
  #from 5 to 9 in the last
  the_values <- seq(1*b,4+b,by = 1)
  
  #take the values uptained above and put them in the right position
  #here the add vector allows me to move from 1:5 to 6:10 to 11:15 
  #to 16:20 to 21:25
  vector_for_sequence[(1:5) + add[b]] <- the_values
}

vector_for_sequence

#new comment


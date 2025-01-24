# @author Alexander Meriakri 
# ID: #40310155
# Assignment 1

#Question 1

#P = 5000, r = 0.05, t = 1 to 20
# I = P*(1+r)^t-P
answer <- c(5000*(1+ 0.05)^(1:20) - 5000)

#ANSWER
#[1]  250.000  512.500  788.125 1077.531 1381.408 1700.478 2035.502 2387.277 2756.641
#[10] 3144.473 3551.697 3979.282 4428.246 4899.658 5394.641 5914.373 6460.092 7033.096
#[19] 7634.751 8266.489

#Question 2

#area formula is a = pi*r^2, r is 3 to 20
areas <- pi * (3:20)^2
areas

#ANSWER
#[1]   28.27433   50.26548   78.53982  113.09734  153.93804  201.06193  254.46900
#[8]  314.15927  380.13271  452.38934  530.92916  615.75216  706.85835  804.24772
#[15]  907.92028 1017.87602 1134.11495 1256.63706

#Question 3

#initialize the variables
n <- seq(10,40, by = 10)
r <- 1.08

#initialize them with 0's to feel them up later
sigma_sum_exposant <- c(sum(r^( 0:n[1] ) ),sum(r^( 0:n[2] ) ),sum(r^( 0:n[3] ) ),sum(r^( 0:n[4] ) ))
formula_sum <- c((1-r^((n[1:4])+1))/(1-r))

#Calculates the difference_q3 between the two
difference_q3 <- abs(sigma_sum_exposant - formula_sum)
difference_q3

#Output the comparison_q3 of the two formulas
comparison_q3 <- sprintf("\nWhen n is %f\nThe first sum is: %f\nThe other one is: %f\nThey have a difference of %d\n",n,sigma_sum_exposant,formula_sum,difference_q3)
cat(comparison_q3) # Prints the comparison_q3 with the \n
# ANSWER
# When n is 10.000000
# The first sum is: 16.645487
# The other one is: 16.645487
# They have a difference of 0
# 
# When n is 20.000000
# The first sum is: 50.422921
# The other one is: 50.422921
# They have a difference of 0
# 
# When n is 30.000000
# The first sum is: 123.345868
# The other one is: 123.345868
# They have a difference of 0
# 
# When n is 40.000000
# The first sum is: 280.781040
# The other one is: 280.781040
# They have a difference of 0

#They output the same answer meaning that they are equivalent formulas

#Question 4

#initialize the vector with 10 positon's 
vector_for_sigma <- c((1-r^((seq(1,10,by=1))+1))/(1-r))

#print the sum's
vector_for_sigma
# ANSWER
# [1]  2.080000  3.246400  4.506112  5.866601  7.335929  8.922803 10.636628 12.487558
# [9] 14.486562 16.645487

#Question 5

N <- c(500,1000,2000,4000,8000)

sigma_sum_division <- c(sum(1/(1:N[1])),sum(1/(1:N[2])),sum(1/(1:N[3])),sum(1/(1:N[4])),sum(1/(1:N[5])))
log_function <- c(log(N[1:length(N)]) + 0.6)

difference_q5 <- abs(sigma_sum_division - log_function)

comparison_q5 <- sprintf(
  "\nWhen N is %f\nThe sigma function outputs: %f\nThe log function outputs: %f\nThey differ by: %f\n"
  ,N,sigma_sum_division,log_function,difference_q5)

cat(comparison_q5)
# ANSWER
# When N is 500.000000
# The sigma function outputs: 6.792823
# The log function outputs: 6.814608
# They differ by: 0.021785
# 
# When N is 1000.000000
# The sigma function outputs: 7.485471
# The log function outputs: 7.507755
# They differ by: 0.022284
# 
# When N is 2000.000000
# The sigma function outputs: 8.178368
# The log function outputs: 8.200902
# They differ by: 0.022534
# 
# When N is 4000.000000
# The sigma function outputs: 8.871390
# The log function outputs: 8.894050
# They differ by: 0.022659
# 
# When N is 8000.000000
# The sigma function outputs: 9.564475
# The log function outputs: 9.587197
# They differ by: 0.022722
#The two equations are roughly the same, with a small error of arround 0.02 on average which is around 1%

#Question 6

#Here we do each, since we want the each of the numbers in the sequence to be printed 5 times
the_same_five_times <- c(rep(1:4, each = 5))
the_same_five_times
#ANSWER
#[1] 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4


#Here we do times, since we want the sequence 5 times 
seq_one_to_five <- c(rep(1:5, times = 5))
seq_one_to_five

#ANSWER
#[1] 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5

#Question 7

#From 1 to 5 then add 0 to each, then 1 to 4
vector_for_sequence<- rep(1:5,by =1) + rep((0:4), each =5)
vector_for_sequence

#ANSWER
#[1] 1 2 3 4 5 2 3 4 5 6 3 4 5 6 7 4 5 6 7 8 5 6 7 8 9


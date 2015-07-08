# Question 1
# Find the sum of all the multiples of 3 or 5 below 1000.
Q1 <- sum(which((1:999 %% 3)==0)) + sum(which((1:999 %% 5)==0)) - sum(which((1:999 %% 15)==0))

# Question 2
# Find the sum of the even-valued terms that do not exceed 4 million in the Fibonacci sequence.
fib <- function(lim){
  fibs <- 1:2
  sfibs <- 0
  evens <- numeric()
  while(sfibs < lim){
    
    new <- fibs[length(fibs)]+fibs[length(fibs)-1]
    
    if(new < lim){
      fibs <- c(fibs,new)
      if(new %% 2 == 0){
        evens <- c(evens,new)
      }
      sfibs <- fibs[length(fibs)]
    }
    else
    {
      sfibs <- lim+1
    }
  }
  print(sum(evens)+2)
  return(sum(evens)+2)
}

Q2 <- fib(4000000)


# Question 3
# What is the largest prime factor of the number 600851475143 ?

bign <- 600851475143

findfac <- function(bign){
  fac <- 0
  pcheck <- 2
  bigfs <- numeric()
  while(pcheck<bign){
    while(fac!=1){ 
      if(bign %% pcheck == 0){
        fac <- 1
        bign <- bign/pcheck
        bigfs <- c(bigfs,pcheck)
        pcheck <- 2
        }
        else
        {pcheck <- pcheck+1}
    }
    fac <- 0
  }
  return(max(bigfs))
}

Q3 <- findfac(bign)

# Question 4
# Find the largest palindrome made from the product of two 3-digit numbers.
isp <- function(n){
  Cn <- as.character(n)
  Vn <- unlist(strsplit(Cn, ""))
  if(all(Vn[1:ceiling(length(Vn)/2)]==rev(Vn)[1:ceiling(length(Vn)/2)])){
    isp <- 1}
  else{
    isp <- 0}
  return(isp)
}

bigp <- 0
for(i in seq(999,100,-1)){
  for(j in seq(999,100,-1)){
    if(isp(i*j)==1 & i*j>bigp){
      bigp <- i*j
    }
  }
} 

print(bigp)

# Question 5
# What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

n <- 20
v <- 21/(1:20)
while(all(v==floor(v))==0){
  n <- n + 20
  v <- n/(1:20)
}
print(n)


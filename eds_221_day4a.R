
# nested for loops. a for loop within a for loop


file_prefix <- c("temp", "pH", "salinity")
file_suffix <- c(1,2,3,4)


for (i in 1:length(file_prefix)){
  for(j in 1:length(file_suffix))
  print(paste0(file_prefix[i], "_", file_suffix[j]))
}

# now switching gears, working on building functions
# function example

exclaim_age <- function(age){
  print(paste("I am", age, "years old!"))
}
exclaim_age(age=29) # console output = I am 29 years old!

# making some functions

birddog_sum <- function(bird, dog){
  pets <- bird+dog
  return(pets)
}

birddog_sum(bird=2, dog=5) # console output is 7

# now we can assign that to a variable

x <- birddog_sum(bird=2, dog=5)

# console input 'x' gives output from the function, 7

# anotha one

double_it <- function(x){
  print(2*x)
}

double_it(4) # output is 8

double_it(1:4) # now we can apply the function to a vector, where the function runs on all values in a vector. output is 2 4 6 8. 

# anotha one 

exclaim_age <- function(age){
  paste("I am", age, "years old!")
}
exclaim_age(1000)

# a new one

find_max <- function(val1, val2){
  if(val1>val2){
    return(val1)
    }else if (val2>val1){
      return(val2)
    }
}

find_max(7,9)  # output is 9, as it is greater than 7

# now an exercise

quarter_splits <- c(1.0, 1.1, 1.2, 1.1, 1.4, 1.5, 1.6, 1.4)


for(i in seq_along(quarter_splits)){
    print(paste(quarter_splits[i]+quarter_splits[i+1]))
}

# another one

animal_age <- function(animal, age){
  if (animal=="dog"){
    print(age*7)
  } else if (animal=="goat"){
    print(age*4.7)
  }
}
animal_age(animal="dog", age=8) # output is 56 (7*8)
animal_age(animal="cow", age=7) # cow isn't in the function, so we get no result

# now let's say we have a data frame 
# creating a data frame...

dog_choice <- data.frame(dog_name = c("Khora", "Teddy", "Waffle", "Banjo"),
                         food = c("everything", "salmon", "pancakes", "chicken"))
library(tidyverse)

dog_menu <- function(name){
  my_sub <- dog_choice %>%
    filter(dog_name == name)
  
   print(paste("My name is", my_sub$dog_name, "and I like to eat", my_sub$food))
}

dog_menu("Khora")
dog_menu("Teddy")
dog_menu("Waffle")
dog_menu("Banjo")

# moving on..
# we can create our own warning messages!
# going back and modifying an old example for this..


animal_age <- function(animal, age){
  
  if(!animal %in% c("dog", "goat")){     # required IF statement to launch custom error
    stop("Oops! Animal must be a dog or a goat!") # here is our custom error message
  }
  
  if(is.numeric(age)==FALSE){
    stop("Age must be a number!")  # anotha one
  }
  
  if(age<=0){
    stop("Age must be greater than 0!")
  }
  
  if (animal=="dog"){
    print(age*7)
  } else if (animal=="goat"){
    print(age*4.7)
  }
}

animal_age("dog", age=5)
animal_age("cow", age=5) # and here we received our custom error message in the console

animal_age("dog", age="old") # custom error message, age must be a number

animal_age("dog", age=0) # custom error, age must be greater than 0


# a new example, about wind turbines or something

calc_winds <- function(rho, radius, windspeed){
  
  if(windspeed>130){
    warning("wow, that's fast! are you sure?")
  }
  
  if(rho>1.225){
    warning("that air density is suspicious. are you sure?")
  }
  
  if(radius<0){
    warning("rotor radius must be a positive value in meters.")
  }
  print(0.3*rho*pi*(radius^2)*(windspeed^3))
}

calc_winds(1,7,50)


#####


# afternoon session... another function example


gw_rate <- function(site){
  
  if(!site %in% c("mountain", "prairie", "desert", "beach")){
    warning("site not included")
  }
  
  gw_depths <- data.frame(sitename = c("mountain",
                                       "prairie",
                                       "desert",
                                       "beach"),
                          depth = c(32, 41, 63, 2),
                          slope = c(11.2, 0.4, 0.8, 2.6))
  site_select <- filter(gw_depths, sitename == site)
  
  transport_rate <- 1.4*site_select$slope + 3.6 * site_select$depth
  
  return(transport_rate)
  
}
gw_rate(site="beach")


# a new example

# N0 is initial starting population, K is carrying capacity, r is growth rate, and time is time.

logistic_growth <- function(N0, K, r, time){
  Nt <- K / (1+((K-N0) / N0) * exp(-r*time))
  return(Nt)
}
logistic_growth(N0=100, K=6000, r=.27, time=40)

time_vec <- seq(from=0, to=50, by= 0.1)

pop_1 <- logistic_growth(N0=100, K=6000, r=.27, time=time_vec)

pop_1_vec <- vector(mode="numeric", length=length(time_vec))

for (i in seq_along(time_vec)){
  population <- logistic_growth(N0=100, K=6000, r=.27, time=time_vec[i])
  pop_1_vec[i] <- population
}
pop_1_vec

pop_time_1 <- data.frame(time_vec, pop_1_vec)

ggplot(data=pop_time_1, aes(x=time_vec, y=pop_1_vec)) + geom_line()

# woof, that was a lot! 
# now graphing multiple growth rates together

logistic_growth <- function(N0, K, r, time){
  Nt <- K / (1+((K-N0) / N0) * exp(-r*time))
  return(Nt)
}

time_vec <- seq(from=0, to=50, by= 0.1)

r_seq <- seq(from=0.2, to=0.4, by=0.01)

out_matrix <- matrix(nrow=length(time_vec), ncol=length(r_seq))

for (i in seq_along(r_seq)){ # outer loop of growth rates
  for(j in seq_along(time_vec)){ # inner loop of time steps
    population <- logistic_growth(N0=100, K=6000, r=r_seq[i], time=time_vec[j])
    out_matrix[j,i] <- population
    }
  
}

out_df <- data.frame(out_matrix, time=time_vec)
colnames(out_df) <- c(paste0("growth_rate_", r_seq), "time")

out_df_long <- out_df %>%
  pivot_longer(cols=-time, names_to="growth_rate", values_to="population_size")

ggplot(data=out_df_long, aes(x=time, y=population_size, color=growth_rate)) + geom_line(show.legend=FALSE) + labs(title="Logistical Population Growth, r0.2 to 0.4", x="Time", y="Population Size")

# damn, that's a nice rainbow of logistic growth!









  
  
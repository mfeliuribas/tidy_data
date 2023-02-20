
library(tidyverse)
library(ds4ling)

test_scores_rm

#to select data/columns from our dataset
select(test_scores_rm, id, test2)

#to select consecutive columns
select(test_scores_rm, spec:test2)

#to rename columns directly
#select (dataset, new name = old name)
select(test_scores_rm, participant = id)



#exercise with mtcars dataset
glimpse(mtcars)

#select any 3 variables
select(mtcars, mpg, disp, wt)

#select the last 3 variables
select(mtcars, am:carb)

#rename mpg to hello_world
select(mtcars, hello_word = mpg)



#to filter rows of a dataframe
#exercise with mtcars dataset

#to filter rows in which mpg is less than 20 and greater than 14
filter(mtcars, mpg < 20, mpg > 14)
filter(mtcars, mpg < 20 & mpg > 14)

#to filter rows in which cyl is equal to 6
filter(mtcars, cyl == 6)

#to filter rows in which mpg is greater than 20 or disp is less than 200
filter(mtcars, mpg > 20 | disp < 200)



#preguntas de Laura
test_scores_rm
filter(test_scores_rm, spec == "g1_lo")

mtcars %>%
  select(mpg, disp) %>%
  filter(mpg > 20) %>%
  ggplot() + 
  aes(x = disp, y = mpg) + 
  geom_point()



#to arrange
#to arrange the mtcars dataset based ...
#another excersie



#to mutate
#create a new column called mpg_x2 that doubles every value in the dataframe
mtcars %>%
  select(mpg) %>%
  mutate(mpg_x2 = mpg * 2)

#create a new column called mpg_c that centers the mpg data by sustracting the 
#mean value of  mpg (...)
mtcars %>%
  select(mpg) %>%
  mutate(mpg_x2 = mpg * 2,
         mpg_c = mpg - mean(mpg)
         )

mean(mtcars$mpg)

#create a new column called value that applies the label "good" to cars that get 
#over 18 mpg and "bad" to cars that get 18 mpg or less
mtcars %>%
  select(mpg) %>%
  mutate(mpg_x2 = mpg * 2,
         mpg_c = mpg - mean(mpg),
         value = if_else(mpg <= 18, "bad", "good")
  )



#to summarize
#group + summarize
#to calculate the mean value of mpg in the dataset mtcars as a function of cyl
mtcars %>%
  summarize(avg = mean(mpg))

#to calculate the mean value of mpg as a function of cyl
mtcars%>% 
  group_by(cyl) %>% #for every level of cyl, calculate what we put in 103 line
  summarize(avg = mean(mpg))

#to calculate the mean, standard deviation, min, and max of mpg as a function 
#of cyl
mtcars%>%
  group_by(cyl) %>%
  summarize(
    avg = mean(mpg), 
    sd = sd(mpg), 
    min = min(mpg), 
    max = max(mpg)
  )



#MONDAY 02/20

library(tidyverse)
library(ds4ling)

# tidyr

test_scores_rm

#to separate
test_scores_rm %>%
  separate(col = id, into = c("lang", "trash"), sep = 4)
# I don't have a unique argument for each participant
test_scores_rm %>%
  separate(col = id, into = c("lang", "trash"), sep = 4, remove = FALSE)

test_scores_rm %>%
  separate(col = id, into = c("lang", "trash"), sep = 4, remove = FALSE) %>% 
  select(-trash)
#select(-trash) = I want everything except trash

test_scores_rm %>%
  separate(col = id, into = c("lang", "trash"), sep = 4, remove = FALSE) %>% 
  select(-trash) %>%
  separate(col=spec, into = c("group", "proficiency"), sep = "_")

#to unite

#to pivot variables
test_scores_rm %>%
  pivot_longer(
    cols = c("test1", "test2"),
    names_to = "test",
    values_to = "scores"
  )

#idk - pivot_wider?
scores_long <- test_scores_rm %>%
  pivot_longer(
    cols = c("test1", "test2"),
    names_to = "test",
    values_to = "scores"
  )

scores_long %>%
  ggplot() +
  aes(x = test, y = scores) +
  geom_boxplot()

test_scores_rm %>%
  ggplot() +
  aes(x = test1, y = test2) +
  geom_point() + 
  geom_smooth(method = "lm")


<!-- README.md is generated from README.Rmd. Please edit the README.Rmd file -->

# Lab report \#3 - instructions

Follow the instructions posted at
<https://ds202-at-isu.github.io/labs.html> for the lab assignment. The
work is meant to be finished during the lab time, but you have time
until Monday evening to polish things.

Include your answers in this document (Rmd file). Make sure that it
knits properly (into the md file). Upload both the Rmd and the md file
to your repository.

All submissions to the github repo will be automatically uploaded for
grading once the due date is passed. Submit a link to your repository on
Canvas (only one submission per team) to signal to the instructors that
you are done with your submission.

# Lab 3: Avenger’s Peril

## As a team

Extract from the data below two data sets in long form `deaths` and
`returns`

Get the data into a format where the five columns for Death\[1-5\] are
replaced by two columns: Time, and Death. Time should be a number
between 1 and 5 (look into the function `parse_number`); Death is a
categorical variables with values “yes”, “no” and ““. Call the resulting
data set `deaths`.

``` r
av <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/avengers/avengers.csv", stringsAsFactors = FALSE)
head(av)
```

    ##                                                       URL
    ## 1           http://marvel.wikia.com/Henry_Pym_(Earth-616)
    ## 2      http://marvel.wikia.com/Janet_van_Dyne_(Earth-616)
    ## 3       http://marvel.wikia.com/Anthony_Stark_(Earth-616)
    ## 4 http://marvel.wikia.com/Robert_Bruce_Banner_(Earth-616)
    ## 5        http://marvel.wikia.com/Thor_Odinson_(Earth-616)
    ## 6       http://marvel.wikia.com/Richard_Jones_(Earth-616)
    ##                    Name.Alias Appearances Current. Gender Probationary.Introl
    ## 1   Henry Jonathan "Hank" Pym        1269      YES   MALE                    
    ## 2              Janet van Dyne        1165      YES FEMALE                    
    ## 3 Anthony Edward "Tony" Stark        3068      YES   MALE                    
    ## 4         Robert Bruce Banner        2089      YES   MALE                    
    ## 5                Thor Odinson        2402      YES   MALE                    
    ## 6      Richard Milhouse Jones         612      YES   MALE                    
    ##   Full.Reserve.Avengers.Intro Year Years.since.joining Honorary Death1 Return1
    ## 1                      Sep-63 1963                  52     Full    YES      NO
    ## 2                      Sep-63 1963                  52     Full    YES     YES
    ## 3                      Sep-63 1963                  52     Full    YES     YES
    ## 4                      Sep-63 1963                  52     Full    YES     YES
    ## 5                      Sep-63 1963                  52     Full    YES     YES
    ## 6                      Sep-63 1963                  52 Honorary     NO        
    ##   Death2 Return2 Death3 Return3 Death4 Return4 Death5 Return5
    ## 1                                                            
    ## 2                                                            
    ## 3                                                            
    ## 4                                                            
    ## 5    YES      NO                                             
    ## 6                                                            
    ##                                                                                                                                                                              Notes
    ## 1                                                                                                                Merged with Ultron in Rage of Ultron Vol. 1. A funeral was held. 
    ## 2                                                                                                  Dies in Secret Invasion V1:I8. Actually was sent tto Microverse later recovered
    ## 3 Death: "Later while under the influence of Immortus Stark committed a number of horrible acts and was killed.'  This set up young Tony. Franklin Richards later brought him back
    ## 4                                                                               Dies in Ghosts of the Future arc. However "he had actually used a hidden Pantheon base to survive"
    ## 5                                                      Dies in Fear Itself brought back because that's kind of the whole point. Second death in Time Runs Out has not yet returned
    ## 6                                                                                                                                                                             <NA>

``` r
# View column names
colnames(av)
```

    ##  [1] "URL"                         "Name.Alias"                 
    ##  [3] "Appearances"                 "Current."                   
    ##  [5] "Gender"                      "Probationary.Introl"        
    ##  [7] "Full.Reserve.Avengers.Intro" "Year"                       
    ##  [9] "Years.since.joining"         "Honorary"                   
    ## [11] "Death1"                      "Return1"                    
    ## [13] "Death2"                      "Return2"                    
    ## [15] "Death3"                      "Return3"                    
    ## [17] "Death4"                      "Return4"                    
    ## [19] "Death5"                      "Return5"                    
    ## [21] "Notes"

``` r
# Load necessary libraries
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
```

    ## Warning: package 'tidyr' was built under R version 4.4.3

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 4.4.3

``` r
# Convert Death columns into long format and ensure correct categories
deaths <- av %>%
  select(Name.Alias, starts_with("Death")) %>%
  pivot_longer(cols = starts_with("Death"), 
               names_to = "Time", 
               values_to = "Death") %>%
  mutate(Time = readr::parse_number(Time),  # Convert Time to numeric
         Death = case_when(
           Death == "YES" ~ "yes",
           Death == "NO" ~ "no",
           TRUE ~ ""  # Keep empty values as they are
         )) %>%
  arrange(Name.Alias, Time)  # Sort the data for clarity

# View the first few rows to check the output
head(deaths)
```

    ## # A tibble: 6 × 3
    ##   Name.Alias  Time Death
    ##   <chr>      <dbl> <chr>
    ## 1 ""             1 yes  
    ## 2 ""             1 yes  
    ## 3 ""             1 yes  
    ## 4 ""             1 no   
    ## 5 ""             1 no   
    ## 6 ""             1 yes

``` r
# Check structure of deaths dataset
str(deaths)
```

    ## tibble [865 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ Name.Alias: chr [1:865] "" "" "" "" ...
    ##  $ Time      : num [1:865] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Death     : chr [1:865] "yes" "yes" "yes" "no" ...

``` r
# View unique values in Death column
unique(deaths$Death)
```

    ## [1] "yes" "no"  ""

Similarly, deal with the returns of characters.

``` r
# Convert Return columns into long format and ensure correct categories
returns <- av %>%
  select(Name.Alias, starts_with("Return")) %>%
  pivot_longer(cols = starts_with("Return"), 
               names_to = "Time", 
               values_to = "Return") %>%
  mutate(Time = readr::parse_number(Time),  # Convert Time to numeric
         Return = case_when(
           Return == "YES" ~ "yes",
           Return == "NO" ~ "no",
           TRUE ~ ""  # Keep empty values as they are
         )) %>%
  arrange(Name.Alias, Time)  # Sort for clarity

# View the first few rows to check the output
head(returns)
```

    ## # A tibble: 6 × 3
    ##   Name.Alias  Time Return
    ##   <chr>      <dbl> <chr> 
    ## 1 ""             1 "yes" 
    ## 2 ""             1 "no"  
    ## 3 ""             1 "no"  
    ## 4 ""             1 ""    
    ## 5 ""             1 ""    
    ## 6 ""             1 "yes"

Based on these datasets calculate the average number of deaths an
Avenger suffers.

``` r
# Calculate the average number of deaths per Avenger
avg_deaths <- deaths %>%
  filter(Death == "yes") %>%
  group_by(Name.Alias) %>%
  summarise(total_deaths = n()) %>%
  summarise(avg_deaths = mean(total_deaths))

avg_deaths
```

    ## # A tibble: 1 × 1
    ##   avg_deaths
    ##        <dbl>
    ## 1       1.39

## Individually

For each team member, copy this part of the report.

Each team member picks one of the statements in the FiveThirtyEight
[analysis](https://fivethirtyeight.com/features/avengers-death-comics-age-of-ultron/)
and fact checks it based on the data. Use dplyr functionality whenever
possible.

### FiveThirtyEight Statement (Selim’s Individual Work)

> “Out of 173 listed Avengers, my analysis found that 69 had died at
> least one time after they joined the team. That’s about 40 percent of
> all people who have ever signed on to the team.”

``` r
# How many Avengers have returned at least once?
avengers_returned <- returns %>%
  filter(Return == "yes") %>%
  summarise(unique_returns = n_distinct(Name.Alias))

# What percentage of Avengers have returned?
total_avengers <- n_distinct(deaths$Name.Alias)

return_percentage <- (avengers_returned$unique_returns / total_avengers) * 100

# Average number of returns per Avenger
avg_returns <- returns %>%
  filter(Return == "yes") %>%
  group_by(Name.Alias) %>%
  summarise(total_returns = n()) %>%
  summarise(avg_returns = mean(total_returns))

# Print results
avengers_returned
```

    ## # A tibble: 1 × 1
    ##   unique_returns
    ##            <int>
    ## 1             45

``` r
return_percentage
```

    ## [1] 27.60736

``` r
avg_returns
```

    ## # A tibble: 1 × 1
    ##   avg_returns
    ##         <dbl>
    ## 1        1.27

This means that on average, an Avenger comes back to life about 1.27
times in the comics.

``` r
avengers_died <- deaths %>%
  filter(Death == "yes") %>%
  summarise(unique_deaths = n_distinct(Name.Alias))

death_percentage <- (avengers_died$unique_deaths / total_avengers) * 100

death_percentage
```

    ## [1] 39.2638

Conclusion (FiveThirtyEight’s vs Selims Results) Based on the dataset,
the percentage of Avengers who have died at least once is 39.26%, which
is very close to FiveThirtyEight’s claim of 40%. Their claim appears to
be accurate.

### FiveThirtyEight Statement (Kush’s Individual Work)

> “The Avengers roster is overwhelmingly male — only 8 percent of all
> the people who have been Avengers are women.”

``` r
# Total number of unique Avengers

total_avengers <- av %>%
  filter(!is.na(Name.Alias)) %>%
  summarise(n = n_distinct(Name.Alias)) %>%
  pull(n)

# Total number of unique FEMALE Avengers
female_avengers <- av %>%
  filter(Gender == "FEMALE") %>%
  summarise(n = n_distinct(Name.Alias)) %>%
  pull(n)

# Calculate percentage
female_percent <- (female_avengers / total_avengers) * 100
female_percent
```

    ## [1] 33.12883

### FiveThirtyEight Statement (Ash’s Individual Work)

> “Many Avengers have died multiple times — some even five times!”

``` r
# Count how many times each Avenger has died
death_counts <- deaths %>%
  filter(Death == "yes") %>%
  group_by(Name.Alias) %>%
  summarise(num_deaths = n()) %>%
  arrange(desc(num_deaths))

# Count Avengers who died more than once
multi_deaths <- death_counts %>%
  filter(num_deaths > 1) %>%
  summarise(count = n())

# Count Avengers who died exactly five times
five_deaths <- death_counts %>%
  filter(num_deaths == 5) %>%
  summarise(count = n())

multi_deaths
```

    ## # A tibble: 1 × 1
    ##   count
    ##   <int>
    ## 1    16

``` r
five_deaths
```

    ## # A tibble: 1 × 1
    ##   count
    ##   <int>
    ## 1     1

### Conclusion:

According to my analysis (Kush), approximately 33.13% of Avengers are
female — significantly higher than the 8% reported by FiveThirtyEight.
This means the original statement underestimates the representation of
women among Avengers in this dataset. The claim appears inaccurate.

According to my analysis (Ash), several Avengers have died multiple
times — with 1 character(s) dying exactly five times and 16 dying more
than once. This confirms the FiveThirtyEight statement that “many
Avengers have died multiple times — some even five times.” The claim
appears accurate based on the dataset.

### FiveThirtyEight Statement

> Quote the statement you are planning to fact-check.

### Include the code

Make sure to include the code to derive the (numeric) fact for the
statement

### Include your answer

Include at least one sentence discussing the result of your
fact-checking endeavor.

Upload your changes to the repository. Discuss and refine answers as a
team.

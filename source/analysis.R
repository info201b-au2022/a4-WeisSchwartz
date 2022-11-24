library(tidyverse)
library(usmap)
library(ggmap)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}


# initialize dataframe
incarceration_df <- get_data()

# initialize display of scientific figures for ggplot2
options(scipen=100000)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# This function returns a dataframe for use by the plot_jail_pop_for_us()
# function. 
get_year_jail_pop <- function() {
  year_jail <- incarceration_df %>%
    group_by(year) %>%
    summarise(jail_pop = sum(total_jail_pop, na.rm=TRUE))
  return(year_jail)   
}

# This function returns a bar chart showing the trend of the total incarcerated
# population in the U.S. from 1970 to 2018.
plot_jail_pop_for_us <- function()  {
  year_jail_plt <- ggplot(get_year_jail_pop(), aes(x=year, y=jail_pop)) +
    geom_bar(stat='identity') +
    ggtitle('Increase of Jail Popualtion in U.S. (1970-2018)') +
    xlab('Year') + ylab('Total Jail Population')
  return(year_jail_plt)   
} 
#----------------------------------------------------------------------------#

## Section 4  ---- 
#----------------------------------------------------------------------------#
# This function returns a dataframe for use by the plot_jail_pop_by_states()
# function, given an input of state codes. 
get_jail_pop_by_states <- function(states) {
  states_pop <- incarceration_df %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarise(jail_pop = sum(total_jail_pop, na.rm=TRUE))
  return(states_pop)
}

# This function, given an input of U.S. state codes, returns a line chart
# showing the trend of the total incarcerated population in those U.S. states
# from 1970 to 2018.
plot_jail_pop_by_state <- function(states) {
  states_jail_pop_plt <- ggplot(get_jail_pop_by_states(states),
                                aes(x=year, y=jail_pop, color=state)) +
    geom_line() +
    ggtitle('Increase of Jail Popualtion in select U.S. states (1970-2018)') +
    xlab('Year') + ylab('Total Jail Population')
  return(states_jail_pop_plt)
}
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# This function returns a dataframe for use by the plot_black_pop()
# function. 
get_black_pop <- function() {
  black_pop <- incarceration_df %>%
    filter(year == 2018) %>%
    mutate(black_jail_pct = black_jail_pop / total_jail_pop * 100,
           black_pop_pct = black_pop_15to64 / total_pop_15to64 * 100) %>%
    filter(black_jail_pct <= 100, black_pop_pct <= 100)
  return(black_pop)
}

# This function returns a scatter plot comparing the percentage of the black
# population in counties and their respective jails in 2018.
plot_black_pop <- function() {
  black_pop_plt <- ggplot(get_black_pop(), aes(x=black_pop_pct,
                                               y=black_jail_pct,
                                               color=region)) +
    geom_point() +
    geom_segment(x=0, y=0, xend=100, yend=100, linetype='dashed') +
    ggtitle('Percentage of Black Population in Prison vs County (2018)') +
    xlab('Percent Black Population in County') +
    ylab('Percent Black Population in Jail')
  return(black_pop_plt)
}
#----------------------------------------------------------------------------#


## Section 6  ---- 
#----------------------------------------------------------------------------#
# This function returns a dataframe for use by the plot_most_pop()
# function. 
get_most_pop <- function() {
  most_pop <- incarceration_df %>%
    filter(year == 2018) %>%
    select(aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64,
           native_pop_15to64, white_pop_15to64) %>%
    rename(AAPI = aapi_pop_15to64, Black = black_pop_15to64,
           Latinx = latinx_pop_15to64, Native = native_pop_15to64,
           White = white_pop_15to64) %>%
    mutate(Majority_Demographic = names(.)[max.col(.)])
  
  shell <- incarceration_df %>%
    filter(year == 2018) %>%
    select(year, fips, state, county_name, total_pop_15to64)
  
  most_pop <- merge(shell, most_pop, by='row.names')
  return(most_pop)
}

# This function returns a dataframe for use by the plot_most_pop_jail()
# function. 
get_most_pop_jail <- function() {
  most_pop_jail <- incarceration_df %>%
    filter(year == 2018) %>%
    select(aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop,
           white_jail_pop) %>%
    rename(AAPI = aapi_jail_pop, Black = black_jail_pop,
           Latinx = latinx_jail_pop, Native = native_jail_pop,
           White = white_jail_pop) %>%
    mutate(Majority_Demographic = names(.)[max.col(.)])
    
  shell <- incarceration_df %>%
    filter(year == 2018) %>%
    select(year, fips, state, county_name, total_pop_15to64)
  
  most_pop_jail <- merge(shell, most_pop_jail, by='row.names')
  return(most_pop_jail)
}

# This function returns a map of the U.S. where each county is color-coded by
# the majority demographic it is represented by.
plot_most_pop <- function() {
  most_pop_plt <- plot_usmap(data=get_most_pop(), values='Majority_Demographic',
                             color='black') +
    ggtitle('Majority Demographic in Each U.S. County (2018)') +
    theme(legend.position='right')
  return(most_pop_plt)
}

# This function returns a map of the U.S. where each county is color-coded by
# the majority demographic its jails are represented by.
plot_most_pop_jail <- function() {
  most_pop_jail_plt <- plot_usmap(data=get_most_pop_jail(),
                                  values='Majority_Demographic',
                                  color='black') +
    ggtitle('Majority Demographic in Each U.S. County Jail (2018)') +
    theme(legend.position='right')
  return(most_pop_jail_plt)
}
#----------------------------------------------------------------------------#

## Section 2  ---- 
#----------------------------------------------------------------------------#
county_count_2018 <- nrow(incarceration_df %>% filter(year == 2018))
black_pop_county_count <- nrow(get_black_pop() %>%
                                 filter(black_jail_pct > black_pop_pct))
black_pop_diff_pct <- black_pop_county_count / county_count_2018 * 100

most_pop_combined_count <- nrow(merge(get_most_pop(), get_most_pop_jail(),
                                      by='row.names', all.x=TRUE) %>%
                                  filter(
                                    Majority_Demographic.x != Majority_Demographic.y)
)
most_pop_diff_pct <- most_pop_combined_count / county_count_2018 * 100
#----------------------------------------------------------------------------#

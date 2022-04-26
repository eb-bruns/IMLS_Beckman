

### Guided Project: Analyzing Forest Fire Data

## Emily Beckman Bruns | 14 March 2022

# load library
library("tidyverse")

# read in data (provided)
fire <- read.csv("/Users/emily/Desktop/work/forestfires.csv")
str(fire)

# description of variables
  #  X: X-axis spatial coordinate within the Montesinho park map: 1 to 9
  #  Y: Y-axis spatial coordinate within the Montesinho park map: 2 to 9
  #  month: Month of the year: 'jan' to 'dec'
  #  day: Day of the week: 'mon' to 'sun'
  #  FFMC: Fine Fuel Moisture Code index from the FWI system: 18.7 to 96.20
  #  DMC: Duff Moisture Code index from the FWI system: 1.1 to 291.3
  #  DC: Drought Code index from the FWI system: 7.9 to 860.6
  #  ISI: Initial Spread Index from the FWI system: 0.0 to 56.10
  #  temp: Temperature in Celsius degrees: 2.2 to 33.30
  #  RH: Relative humidity in percentage: 15.0 to 100
  #  wind: Wind speed in km/h: 0.40 to 9.40
  #  rain: Outside rain in mm/m2 : 0.0 to 6.4
  #  area: The burned area of the forest (in ha): 0.00 to 1090.84

# convert month and day to factors with the correct order
fire$month <- factor(fire$month,
  levels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))
fire$day <- factor(fire$day,
  levels = c("sun","mon","tue","wed","thu","fri","sat"))
str(fire)

# look at frequency of fires in each month
fire_mo <- fire %>%
  count(month)
  # bar chart
fire_mo %>%
  ggplot(aes(x = month, y = n)) +
  geom_col() +
  labs(
    title = "Predicting Forest Fires in Portugal",
    y = "Number of fires",
    x = "Month"
  )
  # OBSERVATION: Aug/Sep is fire season; some ramp up in Jul; Mar is a mini-season

# look at frequency of fires on each day of the week
fire_dy <- fire %>%
  count(day)
fire_dy %>%
  ggplot(aes(x = day, y = n)) +
  geom_col() +
  labs(
    title = "Predicting Forest Fires in Portugal",
    y = "Number of fires",
    x = "Day of the week"
  )
  # OBSERVATION: most fires occur on the weekend

# look at how each variable changes by month
  # first transform data into a 'long' format
fire_long <- fire %>%
  pivot_longer(
    cols = c(FFMC,DMC,DC,ISI,temp,RH,wind,rain,area),
    names_to = "column",
    values_to = "value"
    )
  # now visualize all variables vs. month
  #   scales="free_y" means each graph has its own scale
fire_long %>%
  ggplot(aes(x = month, y = value, color = column)) +
  geom_point() +
  facet_grid(vars(column), scales="free_y")
  # can also visualize all variables vs. day
fire_long %>%
  ggplot(aes(x = day, y = value, color = column)) +
  geom_col() +
  facet_wrap(vars(column), scales="free_y")

# look at how each variable changes by area
  # first transform data into a 'long' format
fire_long2 <- fire %>%
  pivot_longer(
    cols = c(FFMC,DMC,DC,ISI,temp,RH,wind,rain),
    names_to = "column",
    values_to = "value"
    )
  # now visualize all variables vs. area
    # point & grid
fire_long2 %>%
  ggplot(aes(x = area, y = value, color = column)) +
  geom_point() +
  facet_grid(vars(column), scales="free_x")
    # point & wrap
fire_long2 %>%
  ggplot(aes(x = value, y = area, color = column)) +
  geom_point() +
  facet_wrap(vars(column), scales="free_x")

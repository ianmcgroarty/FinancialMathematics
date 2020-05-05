#Demonstration
#https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

# Ian McGroarty - R Demonstration
################################ PACKAGES ###################################################
##### NOTE: YOU ONLY NEED TO INSTALL THE PACAKAGES ONCE. THEN RESTART R AND USE LIBRARY TO CALL THE PACKAGES. 
#install.packages('tidyverse') 
#install.packages('xlsx') 
#install.packages('usmap')
#install.packages('tmap')
#install.packages('tmaptools')
#install.packages('tigris')
#install.packages('cdlTools')
#install.packages('sf')

library(readxl)
library(tidyverse)
library(cdlTools)
library(tigris)
options(tigris_use_cache = TRUE)
library(sf)
library(usmap)
library(ggplot2)
options(tigris_class = "sf")
library(tmap)
library(tmaptools)

################################ IMPORT ###################################################
## Import the data using the file path.
energydata <- read_excel("//rb.win.frb.org/C1/Accounts/M-O/c1imm01/Redirected/Desktop/Financial Mathematics/Stats/table_07-01_1.xlsx", 
  sheet = "7-1", range = "A4:M56")

#Let's take a look at the data
energydata

summary(energydata)
# Take note that units are in Btu, "Brithish Thermal Units"


################################ CLEANING ###################################################
####Clean up the variable names
names(energydata) <- c("State", "Natural.Gas", "Distillate.Fuel", "Jet.Fuel", "Motor.Gas",
                       "Residential.Fuel", "Other", "Total.Petroleum", "Ethanol", "Electricity",
                       "Net.Energy", "Electrical.losses", "Total")

### I think that things might be getting skewed by the total in the last row
## 1. Create a usa flag

usvec <- c("United States, total")
energydata$usa <- ifelse(energydata$State %in% usvec, 0,1)


## 2. Do a summary table by the usa dummy
by(energydata, energydata$usa, summary)
  ## Looks like I was correct, check out the difference in means between just the states, and the original states table. 

## 3. For simplicity I will remove the total US from my data
energy_state <-    filter(energydata, grepl(0,usa))
              #### FORGET WHAT FILTER DOES?
                  ?Filter

test_filter2 <- filter(energy_state, energy_state$Natural.Gas > energy_state$Jet.Fuel)
drop <- c("Natural.Gas", "Distillate.Fuel")
test.drop <- energydata[,!(names(energydata) %in% drop )]

################################ OUTLIARS ###################################################
#### I suspect there are some states that produce a LOT of energy, lets check out to see if there are outliars!
#### Let's take a look at a variable. 
hist(energy_state$Natural.Gas)
  ## Looks like we have ourselves and outliar! Who could it be? 
      outliars <- energy_state %>% filter(Natural.Gas >=300) %>% select(State)  
        outliars
            #Don't mess with Texas!

### Let's try and look at all petrolium  energy
hist(energy_state$Total.Petroleum)
  ## 1.  I'll run another (more broad) outliars test
    outliars <- energy_state %>% filter(Total.Petroleum >=1000) %>% select(State)  
      outliars
      outliar_vector <- outliars$State
  ## 2. Get rid of these outliars (Cali, Florida, Texas)
    energy_noouts <- subset(energy_state, !State %in% outliar_vector) 

## Let's try one more time!
hist(energy_noouts$Total.Petroleum)
    # OKAY! Things look good, we can move on with our analysis. 

################################ CORRELATION ###################################################
#### I want to run a correlation matrix
## 1. Create a dataset with the columns you want to compare,
      # In our case, we don't care about State which is the first variable, 
      # we also don't care about the totals since it is comprised of other columns
numericcolumns <- energy_noouts[,c(2,3,4,5,6,7,9,10,12)]

## 2. Create the Correlation Matrix
res <- cor(numericcolumns)

## 3. Output
res
  # I was hoping to find something in Electrical Losses but it doesn't look like anything is too correlated
  # Distillate Fuel seems to be highly correlated with a number of other fuel types. This is probably because it is an input product.


################################ SCATTER PLOTS ###################################################
# Since we know our dataset looks good, lets work with just this energy_noouts
attach(energy_noouts)

# Quick Summary - notice how we don't need to specify dataset name now!
summary(Total.Petroleum)
summary(Natural.Gas)

## Let's do some scatter plots to get a sense of what the correlations look like
plot(Residential.Fuel,Distillate.Fuel)
plot(Ethanol,Motor.Gas) 
    ## You'll see in the notes for this dataset an explanation for this nearly perfect correlation. 
plot(Jet.Fuel,Residential.Fuel)
plot(Total.Petroleum, Natural.Gas)
detach()
################################ MAPS ###################################################
# It doesn't seem like there is much interesting to explore with just the energy outputs.
# Let's include states now!

## 1. Create a dataset with the state polygons. This is through the tigris package. 
us_states <- states(cb = TRUE, year = 2017, class = "sf")
  ### Make the variable names match
  colnames(us_states)[6] <-"State"

## 2. Merge the energy data with the polygon data.
energy_maps <- inner_join(us_states,energy_state, by = "State")

## 3. With the tmap packpage we can do a quick map
qtm(energy_maps, fill = "Total.Petroleum")
  ## This is kind of hard to see. So lets ignore Alaska, Hawaii, and Puerto Rico
    ## 3a. Create a new dataset that does not contain these threee geographies 
    energy_48states <- subset(energy_maps, !(State %in% c("Alaska", "Hawaii", "Puerto Rico")))
    qtm(energy_48states, fill = "Total.Petroleum")
      ## That looks better

## 4. Create a vector with all the energy types we are interested in
energytypes <- c("Natural.Gas", "Distillate.Fuel", "Jet.Fuel", "Motor.Gas",
                 "Residential.Fuel", "Other", "Total.Petroleum", "Ethanol", "Electricity",
                 "Net.Energy", "Electrical.losses", "Total")

## 5. Create an easy function to perform the mapping
quickmap <- function(varx) {
  qtm(energy_48states, fill = varx)
}

## 6. Use the map function to run our function 'quickmap' over all elements of 'energytypes'
map(energytypes,quickmap)
  ## Look how much brighter Texas and California are than every other state in nearly every category.
  ## However, New York is still king when it comes to electricty. There must be a waterfall in upstate NY or something. ;)


pa.counties <- c(1, 9, 11, 13, 15, 17, 21, 23, 25, 27, 29, 33, 35, 37, 41, 43, 45, 47, 55, 57, 61, 67, 69,
                 71, 75, 77, 79, 81, 83, 87, 89, 91, 93, 95, 97, 99, 101, 103, 105, 107, 109, 113, 115,
                 117, 119, 127, 131, 133)
nj.counties <- c(1, 5, 7, 9, 11, 15, 21, 29, 33)
t1$third <- ifelse(t1$state = 'DE',1,
            ifelse(t1$state = 'NJ', 
                ifelse(t1$county_code %in% nj.counties, 1 , 0),
            ifelse(t1$state = 'PA',
                ifelse(t1$county_code %in% pa.counties,1,0) )
            ))

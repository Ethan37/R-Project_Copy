---
title: "First Midterm"
author: "Ethan Brown"
date: "10/20/2019"
output: 
  html_document: 
    keep_md: yes
---


```r
library(tidyverse)
```

```
## ── Attaching packages ──────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 3.2.1     ✔ purrr   0.3.3
## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
## ✔ readr   1.3.1     ✔ forcats 0.4.0
```

```
## ── Conflicts ─────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
#nanair is for changing values to na.
library(naniar)
#maps and mapdata are for making map plots.
library(maps)
```

```
## 
## Attaching package: 'maps'
```

```
## The following object is masked from 'package:purrr':
## 
##     map
```

```r
library(mapdata)
```


```r
county_city <- read.csv("data/county_city.csv")
GTD <- read.csv("data/GTD.csv")
population <- read.csv("data/population.csv")
poverty <- read.csv("data/poverty.csv")
states <- read.csv("data/states.csv")
```

Throughout this midterm, I put links to websites I used for reference in the form of the name of what I used for things that we may not have learned in class.

First off, we will clean the poverty data set so that it merges cleanly into the final data set. This includes turning the MEDHHINC_2015 variable into a numeric variable, and changing some words into different words.


```r
poverty2 <- poverty %>%
  mutate(MEDHHINC_2015 = as.numeric(str_remove_all(MEDHHINC_2015, (","))
  )) %>%
  mutate(Area_Name = str_replace_all(Area_Name, c(" County" = "", 
                                             " Census Area" = "",
                                             " Parish" = "",
                                             " Borough" = "",
                                             "McLennan" = "Mclennan",
                                             "District of Columbia" = "District Of Columbia",
                                             "DuPage" = "Du Page",
                                             "city" = "City",
                                             "St. Louis" = "Saint Louis",
                                             "St. Lucie" = "Saint Lucie")))
```
([str_replace_all](https://www.rdocumentation.org/packages/stringr/versions/1.4.0/topics/str_replace)) 
Now we will clean the county_city data set. This includes joining the poverty data set to it, as they have similar variables that can join, joining a data set that contain the state abbreviations and state names to make for easier merging into the final data set, changing some words in the data set so that they merge correctly, taking out useless variables that clog the final data set, and changing the variable "name" to "state_name" for clarity.

```r
#I deleted row 11873 from county_city because that row had no county name in it, was not needed, and was messing my final data set up.
county_city2 <- county_city[-c(11873),] %>%
  left_join(states, by = c("state" = "abbreviation"))%>%
  left_join(poverty2, by = c("county" = "Area_Name", "state" = "State"))%>%
  select(-latitude, -longitude, -area)%>%
  distinct()%>%
  mutate(city = str_replace_all(city, c("Mc Cook" = "McCook",
                                        "New York" = "New York City",
                                        "Inglewood" = "Ingelwood",
                                        "Tyngsboro" = "Tyngsborough",
                                        "Saint Louis" = "St. Louis",
                                        "Saint Cloud" = "St. Cloud")))
```

```
## Warning: Column `state`/`abbreviation` joining factors with different
## levels, coercing to character vector
```

```
## Warning: Column `county`/`Area_Name` joining factor and character vector,
## coercing into character vector
```

```
## Warning: Column `state`/`State` joining character vector and factor,
## coercing into character vector
```

```r
colnames(county_city2)[colnames(county_city2)=="name"] <- "state_name"
```
([renaming a column](http://rprogramming.net/rename-columns-in-r/)) 
Now we will clean the GTD data set. All this entails is adding an observation column to keep track of our rows easier.

```r
GTD <- GTD %>%
  mutate(observation = row_number())
```

Now we will clean the population data set. To do this we change some words in the data set so that they merge properly, and we also take out unnecessary variables so that it merges properly.

```r
population2 <- population %>%
  select(-SUMLEV,-STATE,-COUNTY,-PLACE)%>%
  arrange(NAME)%>%
  distinct()%>%
  mutate(NAME = str_replace_all(NAME, c("New York city" = "New York City",
                                        "Macon-Bibb County" = "Macon",
                                        " city" = "", 
                                        " township" = "",
                                        " town" = "",
                                        " Town" = "",
                                        " village" = "",
                                        " borough" = "",
                                        "Inglewood" = "Ingelwood")))
```


Now we will merge our two data sets, population2 an county_city2, with GTD to make our final data set. We will also make a date variable that combines the three date variables into one.

```r
midterm_data_ethan37_1 <- GTD %>%
  mutate(date = make_date(iyear, imonth, iday))%>%
  left_join(population2, by = c("city" = "NAME", "provstate" = "STNAME")) %>%
  left_join(county_city2, by = c("city", "provstate" = "state_name" )) %>%
  distinct()%>%
  droplevels()
```

```
## Warning: Column `city`/`NAME` joining factor and character vector, coercing
## into character vector
```

```
## Warning: Column `provstate`/`STNAME` joining factors with different levels,
## coercing to character vector
```

```
## Warning: Column `provstate`/`state_name` joining character vector and
## factor, coercing into character vector
```

Now it is time to clean our final data set.

First, we will check for spelling differences and see if that fixes why we have NA for the population estimates for some rows.

```r
midterm_data_ethan37_1 %>%
  group_by(provstate, city, POPESTIMATE2013, POPESTIMATE2014, POPESTIMATE2015, POPESTIMATE2016) %>%
  summarise()%>%
  filter(is.na(POPESTIMATE2013) | is.na(POPESTIMATE2014) | is.na(POPESTIMATE2015) | is.na(POPESTIMATE2016))
```

```
## # A tibble: 4 x 6
## # Groups:   provstate, city, POPESTIMATE2013, POPESTIMATE2014,
## #   POPESTIMATE2015 [4]
##   provstate city  POPESTIMATE2013 POPESTIMATE2014 POPESTIMATE2015
##   <chr>     <chr>           <int>           <int>           <int>
## 1 Californ… Lake…              NA              NA              NA
## 2 New York  New …              NA              NA              NA
## 3 Oregon    Prin…              NA              NA              NA
## 4 South Ca… Warr…              NA              NA              NA
## # … with 1 more variable: POPESTIMATE2016 <int>
```
The cities we originally had NA for were New York City, Macon, Warrenville, Ingelwood, Princeton, Lake Los Angeles, and New City.

Now we look at the population data set where either the city name or the state name is in it to see if we can see why these didn't show up. Any cities I can not find I will find online and then I will add them to a new data set that I will then be able to bind into my final data set.


```r
# view(population %>%
#        filter(NAME == "New York City" | STNAME == "New York"))
# #I found that it is spelt "New York city" in the population data set. I will add to the str_replace_all function.
# 
# view(population %>%
#        filter(NAME == "Macon" | STNAME == "Georgia"))
# #I found that it is spelt "Macon-Bibb County" in the population data set. I will add to the str_replace_all function.
# 
# view(population %>%
#        filter(NAME == "Warrenville" | STNAME == "South Carolina"))
# #I could not find anything similar to Warrenville, South Carolina in the population data set. Insted, I found the populatin online for 2010 and 2017. I will use these to fill in the missing information.
# 
# view(population %>%
#        filter(NAME == "Ingelwood" | STNAME == "California"))
# #I found that it is spelt "Inglewood" in the population data set. I will add to the str_replace_all function.
# 
# view(population %>%
#        filter(NAME == "Princeton" | STNAME == "Oregon"))
# #I could not find anything similar to Princeton, Oregon in the population data set.
# 
# view(population %>%
#        filter(NAME == "Lake Los Angeles" | STNAME == "California"))
# #I could not find anything similar to Lake Los Angeles, California in the population data set.
# 
# view(population %>%
#        filter(NAME == "New City" | STNAME == "New York"))
# #I could not find anything similar to New City, New York in the population data set, but Clarkston, New York is and New City is in Clarkston.
```
I commented out all of this code because I do not want to accidentaly run it as it takes a long time to run.

Now I will make my new data set. Here is a list of websites I used to find this data. These are either averages or 2010 data, as I could not find anything else. 
[Warrenville, SC](http://www.towncharts.com/South-Carolina/Demographics/Warrenville-CDP-SC-Demographics-data.html.) 
[Princton, OR](https://www.bestplaces.net/people/zip-code/oregon/princeton/97721) 
[Lake Los Angeles, CA](https://www.census.gov/quickfacts/fact/table/lakelosangelescdpcalifornia/PST045218) 
[New City, NY](https://www.census.gov/quickfacts/fact/table/newcitycdpnewyork/PST045218) 

```r
STNAME <- c("South Carolina", "Oregon", "California", "New York")
NAME <- c("Warrenville", "Princeton", "Lake Los Angeles", "New City")
POPESTIMATE2013 <- c(1163, 299, 12328, 33559)
POPESTIMATE2014 <- c(1163, 299, 12328, 33559)
POPESTIMATE2015 <- c(1163, 299, 12328, 33559)
POPESTIMATE2016 <- c(1163, 299, 12328, 33559)
missing_pop <- data.frame(STNAME, NAME, POPESTIMATE2013, POPESTIMATE2014, POPESTIMATE2015, POPESTIMATE2016)

#Now I will bind this with the population2 data set.
population2 <-  population2 %>%
  rbind(missing_pop)

#Finally we will check for NA like we did before.
population2%>%
  filter(is.na(POPESTIMATE2013) | is.na(POPESTIMATE2014) | is.na(POPESTIMATE2015) | is.na(POPESTIMATE2016))
```

```
## [1] NAME            STNAME          POPESTIMATE2013 POPESTIMATE2014
## [5] POPESTIMATE2015 POPESTIMATE2016
## <0 rows> (or 0-length row.names)
```
([creating a data frame](https://www.dummies.com/programming/r/how-to-create-a-data-frame-from-scratch-in-r/)) ([binding rows](http://www.programmingr.com/examples/r-dataframe/rbind-in-r-row-bind/)) 
Every city in the GTD data set now has a population estimate for each year!


Now we will check for spelling differences in the poverty data set and see if that fixes why we have NA for the poverty estimates for some rows.


```r
midterm_data_ethan37_1 %>%
  group_by(provstate, city, PCTPOV017_2015, PCTPOVALL_2015, MEDHHINC_2015) %>%
  summarise()%>%
  filter(is.na(PCTPOV017_2015) | is.na(PCTPOVALL_2015) | is.na(MEDHHINC_2015))
```

```
## # A tibble: 5 x 5
## # Groups:   provstate, city, PCTPOV017_2015, PCTPOVALL_2015 [5]
##   provstate    city             PCTPOV017_2015 PCTPOVALL_2015 MEDHHINC_2015
##   <chr>        <chr>                     <dbl>          <dbl>         <dbl>
## 1 California   Lake Los Angeles             NA             NA            NA
## 2 Kansas       Overland Park                NA             NA            NA
## 3 Pennsylvania Blooming Grove               NA             NA            NA
## 4 Pennsylvania Colerain                     NA             NA            NA
## 5 Texas        Corinth                      NA             NA            NA
```
The cities we originally had NA for were West, TX; McCook, NE; Washington, DC; New York City, NY; Overland Park, KS; Corinth, TX; Blooming Grove, PA; Aurora, IL; Ingelwood, CA; Colerain, PA; Lake Los Angeles, CA; Tyngsborough, MA; St. Louis, MO; Fort Pierce, FL; St. Cloud, MN;

Now we will look at the county city data set that is joined with our poverty data set to see why we do not have these cities. Then go back to our original poverty data set if we have to. Any cities I can not find I will make a new data set for with information I find on google, and then bind it to the data set.


```r
# view(county_city2 %>%
#        filter(city == "West" | state == "TX"))
# #It is in county Mclennan.
# view(poverty %>%
#        filter(Area_Name == "Mclennan" | State == "TX"))
# #I found that it is spelt McLennan in the original data, so I will go back and add this change.
# 
# view(county_city2 %>%
#        filter(city == "McCook" | state == "NE"))
# #I found that the city is spelt Mc Cook in the county_city2 data set, so I will go back and add this change.
# 
# view(county_city2 %>%
#        filter(city == "Washington" | state == "DC"))
# #It is in county District Of Columbia.
# view(poverty %>%
#        filter(Area_Name == "District Of Columbia" | State == "DC"))
# #I found that it is spelt District of Columbia in the poverty data set, so I will go back and add this change.
# 
# view(county_city2 %>%
#        filter(city == "New York City" | state == "NY"))
# #I found that it is just spelt New York in this data set, so I will go back and add this change.
# 
# view(county_city2 %>%
#        filter(city == "Overland Park" | state == "KS"))
# #I could not find any cities similar to Overland Park, KS in this data set.
# 
# view(county_city %>%
#        filter(city == "Corinth" | state == "TX"))
# #It could not find any cities similar to Corinth, TX in this data set.
# 
# view(county_city2 %>%
#        filter(city == "Blooming Grove" | state == "PA"))
# #I could not find any cities similar to Blooming Grove, PA.
# 
# view(county_city2 %>%
#        filter(city == "Aurora" | state == "IL"))
# #It is in county Du Page.
# view(poverty %>%
#        filter(Area_Name == "Du Page" | State == "IL"))
# #I found that it is spelt DuPage in this data set, so I will go back and add this change.
# 
# view(county_city2 %>%
#        filter(city == "Ingelwood" | state == "CA"))
# #I found that it is spelt Inglewood in this data set, so I will go back and add this change.
# 
# view(county_city2 %>%
#        filter(city == "Colerain" | state == "PA"))
# #I could not find any cities similar to Colerain.
# 
# view(county_city2 %>%
#        filter(city == "Lake Los Angeles" | state == "CA"))
# #I could not find any cities similar to Lake Los Angeles.
# 
# view(county_city2 %>%
#        filter(city == "Tyngsborough" | state == "MA"))
# #I found that it is spelt Tyngsboro in this data set, so I will go back and add this change.
# 
# view(county_city2 %>%
#        filter(city == "St. Louis" | state == "MO"))
# #I found that it is spelt Saint Louis in this data set, so I will go back and add this change. The County is also spelt Saint Louis in this data set, while it is spelt St. Louis in the poverty data set, so I will add this change also.
# 
# view(county_city2 %>%
#        filter(city == "Fort Pierce" | state == "FL"))
# #It is in county Saint Lucie.
# view(poverty %>%
#        filter(Area_Name == "Saint Lucie" | State == "FL"))
# #I found that it is spelt St. Lucie in this data set, so I will go back and add this change.
# 
# view(county_city2 %>%
#        filter(city == "St. Cloud" | state == "MN"))
# #I found that it is spelt Saint Cloud in this data set, so I will go back and add this change.
```
I commented out all of this code because I do not want to accidentaly run it as it takes a long time to run.

Now we will find data online for the cities I could not find, and make a new data set with what I find. These cities and the websites I found them on are: 
[Overland Park, KS](https://datausa.io/profile/geo/overland-park-ks/);
[Corinth, TX](https://datausa.io/profile/geo/corinth-tx) 
[Blooming Grove, PA](https://www.point2homes.com/US/Neighborhood/PA/Springfield-Paradise/Blooming-Grove-Demographics.html) 
[Colerain, PA](https://www.point2homes.com/US/Neighborhood/PA/Bedford-County/Colerain-Demographics.html) 
[Lake Los Angeles, CA](https://datausa.io/profile/geo/lake-los-angeles-ca) 

```r
#The numbers for Blooming Grove, PA and Colerain, PA are estimates and may not be accurate as I could not find much information about these cities poverty level and median income level.
city <- c("Overland Park", "Corinth", "Blooming Grove", "Colerain", "Lake Los Angeles")
state <- c("KS", "TX", "PA", "PA", "CA")
county <- c("Johnson", "Denton", "Pike", "Lancaster", "Los Angeles")
PCTPOVALL_2015 <- c(5.79, 5.04, 8.35, 14.31, 30.1)
PCTPOV017_2015 <- c(1.55, 2.23, 3.00, 5.00, 12.73)
MEDHHINC_2015 <- c(72463, 90505, 61194, 50417, 40240)
state_name <- c("Kansas", "Texas", "Pennsylvania", "Pennsylvania", "California")
missing_pov <- data.frame(city, state, county, PCTPOVALL_2015, PCTPOV017_2015, MEDHHINC_2015, state_name)

#Finally I will bind this with the county_city data set.
county_city2 <- county_city2 %>%
  rbind(missing_pov)
```

All of our cities now have poverty data!

Now we have to join our two data sets together, county_city2 and population2.

```r
county_pop <- county_city2 %>%
  left_join(population2, by = c("city" = "NAME", "state_name" = "STNAME"))
```

Finally, we join this data set with the GTD data set for our new final data set.


```r
midterm_data_ethan37 <- GTD %>%
  mutate(date = make_date(iyear, imonth, iday))%>%
  left_join(county_pop, by = c("city", "provstate" = "state_name" )) %>%
  distinct()%>%
  droplevels()%>%
  ungroup()
```

```
## Warning: Column `city` joining factor and character vector, coercing into
## character vector
```

```
## Warning: Column `provstate`/`state_name` joining factors with different
## levels, coercing to character vector
```

Now, we will try to figure out why there are more rows in our new dataset than in GTD.

```r
table(midterm_data_ethan37$observation)
```

```
## 
##   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18 
##   1   1   1   2   2   1   1   1   1   1   1   1   2   1   3   1   1   1 
##  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   3   1   2   2 
##  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54 
##   1   1   1   1   2   2   2   1   1   2   1   1   1   1   1   1   2   1 
##  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72 
##   2   1   1   1   1   1   1   1   4   1   2   1   1   1   1   1   1   1 
##  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90 
##   1   1   1   2   3   1   1   2   2   1   1   2   2   1   2   2   3   1 
##  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 
##   1   1   1   1   1   1   1   2   2   2   1   2   2   1   1   1   2   1 
## 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   2   4   1   1   1 
## 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 
##   2   1   2   2   4   1   3   7   7   2   1   2   1   1   1   1   3   1 
## 145 146 
##   1   1
```

Making tables of how many times each observation in GTD happens in our new dataset, we find that a good amount happen more than once. This is because some cities are in two different counties, and each county has its own poverty levels, while we want each city to have only one poverty level.

To fix this, we will group by observation number in our data set, and then take the mean of the values for the population and poverty numbers.


```r
average_data <- midterm_data_ethan37 %>%
  group_by(observation)%>%
  summarise(POPESTIMATE2013 = mean(POPESTIMATE2013),
            POPESTIMATE2014 = mean(POPESTIMATE2014),
            POPESTIMATE2015 = mean(POPESTIMATE2015),
            POPESTIMATE2016 = mean(POPESTIMATE2016),
            PCTPOVALL_2015 = mean(PCTPOVALL_2015),
            PCTPOV017_2015 = mean(PCTPOV017_2015),
            MEDHHINC_2015 = mean(MEDHHINC_2015))
```

Now we will join this with our final data set so that we only have one row for each observation.


```r
#we unselect all of those variables in order to make it so that they are not included in the data set along with our new averages.
midterm_data_ethan37 <-  midterm_data_ethan37 %>%
       select(-county,
              -POPESTIMATE2013, 
              -POPESTIMATE2014, 
              -POPESTIMATE2015,
              -POPESTIMATE2016,
              -PCTPOVALL_2015,
              -PCTPOV017_2015,
              -MEDHHINC_2015)%>%
  right_join(average_data, by = "observation")%>%
  distinct()
```

Now that we have the correct amount of rows, we will check all of the variables that I have not mentioned yet to see if we need to change anything there. I will keep any tables I make that say I need to fix the data.


```r
table(midterm_data_ethan37$nperps)
```

```
## 
## -99   1   2   3   4   7  12 
##  50  78  10   4   2   1   1
```

```r
#Here we find -99 is used for unknown. We will change this to NA.

midterm_data_ethan37 <- midterm_data_ethan37 %>%
    replace_with_na_at(.vars = "nperps", 
                       condition = ~.x == -99)

table(midterm_data_ethan37$weapsubtype1_txt)
```

```
## 
##                                                  Arson/Fire 
##                             5                            43 
##              Automatic Weapon                     Explosive 
##                            18                             1 
##           Gasoline or Alcohol                       Handgun 
##                             5                             9 
##            Hands, Feet, Fists   Knife or Other Sharp Object 
##                             3                             8 
##  Molotov Cocktail/Petrol Bomb          Other Explosive Type 
##                            12                            13 
##                Remote Trigger Rifle/Shotgun (non-automatic) 
##                             7                             4 
##        Unknown Explosive Type              Unknown Gun Type 
##                             6                            12
```

```r
#Here we find that missing values are left blank. We will change this to NA.

midterm_data_ethan37$weapsubtype1_txt[midterm_data_ethan37$weapsubtype1_txt %in% ""] <- NA 
#The other way to make values NA was not working for character values, so I found this way instead.

table(midterm_data_ethan37$property)
```

```
## 
## -9  0  1 
##  8 46 92
```

```r
#Here we find -9 is used for unknown. We will change this to NA.

midterm_data_ethan37 <- midterm_data_ethan37 %>%
    replace_with_na_at(.vars = "property", 
                       condition = ~.x == -9)

table(midterm_data_ethan37$propextent_txt)
```

```
## 
##                                              
##                                           54 
## Major (likely > $1 million but < $1 billion) 
##                                            4 
##                  Minor (likely < $1 million) 
##                                           81 
##                                      Unknown 
##                                            7
```

```r
#Here we find some values are left blank, we will change these to NA.

midterm_data_ethan37$propextent_txt[midterm_data_ethan37$propextent_txt %in% ""] <- NA 
         
table(midterm_data_ethan37$propvalue)
```

```
## 
##       -99       200      1000      2000      3000      5000      5744 
##        72         1         2         1         1         1         1 
##     25000     30000     60000     70000     90000    250000    333333 
##         1         1         1         1         1         1         3 
##   1000000   1830000   2000000 100000000 
##         1         1         1         1
```

```r
#Here we find -99 is used for unknown. We will change this to NA.

midterm_data_ethan37 <- midterm_data_ethan37 %>%
    replace_with_na_at(.vars = "propvalue", 
                       condition = ~.x == -99)
```
([replace_with_na_at](http://naniar.njtierney.com/articles/replace-with-na.html)) 
All other variables have either already been dealt with, or already have all valid values. 

After starting analysis, I realized that it would be useful to make a "population" variable that tells us the population of the city the year the incident happens based on our estimates.

```r
midterm_data_ethan37 <- midterm_data_ethan37 %>%
  mutate(population_estimate = case_when(
    iyear == 2013 ~ POPESTIMATE2013,
    iyear == 2014 ~ POPESTIMATE2014,
    iyear == 2015 ~ POPESTIMATE2015,
    iyear == 2016 ~ POPESTIMATE2016))
```
Our data set is now clean! Time to do analysis.

First we will look at the locations of each incident and how the population size and poverty levels might effect the occurance of these incidents.

```r
#This creates the data for the usa map and outlines for states.
states <- map_data("state")

#This creates the frame of the map.
states_blank <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), 
               fill = "gray", color = "darkgray") + 
  coord_fixed(1.3) +
  guides(color=FALSE)

#This adds in all of the points, where the size is the population, and the darker the point is, the more incedents that happened.
states_population <- states_blank +
  geom_point(data = midterm_data_ethan37, 
             aes(x = longitude, y = latitude, size = population_estimate),
             alpha = .3,
             color = "red") +
  guides(size=FALSE) +
  labs(title = "Terrorist attacks from 2013-2016",
       subtitle = "by location and population") +
  theme_void()

states_population
```

![](first-midterm_files/figure-html/plot-of-US-population-1.png)<!-- -->

([making maps](https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html)) 
From this map, it does not seem like population has a relationship with the occurance of terrorists attacks, with the exception of New York City. If population did have a relationship with the occurance of terrorist attacks, every large point would be very dark, like in New York, but this is not the case.


```r
states_PCTPOVALL <- states_blank +
  geom_point(data = midterm_data_ethan37, 
             aes(x = longitude, y = latitude, size = PCTPOVALL_2015),
             alpha = .3,
             color = "red") +
  labs(title = "Terrorist attacks from 2013-2016",
       subtitle = "by location and percent of all residents in poverty ") +
  theme_void()

states_PCTPOVALL
```

![](first-midterm_files/figure-html/plot-of-us-PCTPOVALL-1.png)<!-- -->

From this map, we can see that the poverty levels for all residents in the city may have a relationship with the occurance of terrorists attacks in that city. This is because it looks like whenever the circles are larger, there are more circles than in other places.


```r
states_PCTPOV017 <- states_blank +
  geom_point(data = midterm_data_ethan37, 
             aes(x = longitude, y = latitude, size = PCTPOV017_2015),
             alpha = .3,
             color = "red") +
  labs(title = "Terrorist attacks from 2013-2016",
       subtitle = "by location and percent of residents 17 years old and younger in poverty ") +
  theme_void()

states_PCTPOV017
```

![](first-midterm_files/figure-html/plot-of-us-PCTPOV017-1.png)<!-- -->

This map looks like it shows a very similar relationship as to the map for the percent of all residents.


```r
states_MEDHHINC <- states_blank +
  geom_point(data = midterm_data_ethan37, 
             aes(x = longitude, y = latitude, size = MEDHHINC_2015),
             alpha = .3,
             color = "red") +
  labs(title = "Terrorist attacks from 2013-2016",
       subtitle = "by location and median household income ") +
  theme_void()

states_MEDHHINC
```

![](first-midterm_files/figure-html/plot-of-us-MEDHHINC-1.png)<!-- -->

This map shows that the median household income of a city may also have a relationship with the amount of terroist attacks. Whenever the circles are larger on the map, meaning higher median household income, it looks like there are more circles in that area.

Now we will look at how many times of each type of incident happen per month by the type of attack, and whether or not it was commited by an individual or by a group.

```r
incidents_by_type_indivual <- midterm_data_ethan37 %>%
  group_by(month = month(imonth, label = TRUE), attacktype1_txt, individual) %>%
  summarise(count = n()) %>%
  mutate(individual = case_when( individual == 1 ~ "Yes",
                               individual == 0 ~ "No"))%>%
  ggplot() +
  geom_bar(mapping = aes(x = month, 
                         y = count, 
                         fill = individual), 
           stat = "identity",
           position = position_dodge2()) +
  facet_wrap(~ attacktype1_txt, 4,2) +
  labs(title = "Amount of terrorist attacks by month",
       x = "Month",
       y = "Amount of terrorist attacks")+
  guides(fill = guide_legend())

incidents_by_type_indivual
```

![](first-midterm_files/figure-html/incidents-by-type-and-individual-1.png)<!-- -->

Here we can see that attacks happen all throughout the year with no significant spikes in a certain month, except for attacks on Facility/Infrastructure where most attacks happen between April and October. We can also see that for attacks on Facility/Infrastructure most of them are by groups of people, while Armed Assault attacks are usually by individuals.

We will again look at the amount of attack per month by the type of the attack, but this time it will also be by if someone claimed responsibility for the attack.

```r
incidents_by_type_claimed <- midterm_data_ethan37 %>%
  group_by(month = month(imonth, label = TRUE), attacktype1_txt, claimed) %>%
  summarise(count = n()) %>%
  mutate(claimed = case_when( claimed == 1 ~ "Yes",
                               claimed == 0 ~ "No"))%>%
  ggplot() +
  geom_bar(mapping = aes(x = month, 
                         y = count, 
                         fill = claimed), 
           stat = "identity",
           position = position_dodge2()) +
  facet_wrap(~ attacktype1_txt, 4,2) +
  labs(title = "Amount of terrorist attacks by month",
       x = "Month",
       y = "Amount of terrorist attacks")+
  guides(fill = guide_legend())

incidents_by_type_claimed
```

![](first-midterm_files/figure-html/incidents-by-type-and-claimed-1.png)<!-- -->

This time, we see that most of the time no one claims responsibility for the attacks on Facility/Infrastructure.

Now we will look at the relationship between the amount of wounded and killed combined and the population of the city the attack occured in.

```r
wounded_killed_population <- midterm_data_ethan37 %>%
  group_by(nwound, nkill, population_estimate) %>%
  summarise(wounded_killed = sum(nwound, nkill)) %>%
  ggplot() +
  geom_point(mapping = aes(x = population_estimate, y = wounded_killed), alpha = .4) +
  scale_x_log10() +
  labs(title = "Population and amount of people wounded or killed",
       x = "log10 of population",
       y = "Amount of people wounded or killed")

wounded_killed_population
```

![](first-midterm_files/figure-html/wounded-killed-population-1.png)<!-- -->

We can see a slight increase of amount of people wounded or killed as population increases

Now we will look at the relationship between the amount of wounded and killed combined and the percent of all residents living in poverty in the city the attack occured.

```r
wounded_killed_PCTPOVALL <- midterm_data_ethan37 %>%
  group_by(nwound, nkill, PCTPOVALL_2015) %>%
  summarise(wounded_killed = sum(nwound, nkill)) %>%
  ggplot() +
  geom_point(mapping = aes(x = PCTPOVALL_2015, y = wounded_killed), alpha = .4) +
  labs(title = "Percent of all residents in poverty and amount of people wounded or killed",
       x = "Percent of all residents living in poverty",
       y = "Amount of people wounded or killed")

wounded_killed_PCTPOVALL
```

![](first-midterm_files/figure-html/wounded-killed-PCTPOVALL-1.png)<!-- -->

Here we can see that the amount of people wounded or killed does not have a relationship with the percent of all residents living in that city.

Now we will look at the relationship between the amount of wounded and killed combined and the percent of residents 17 and under living in poverty in the city the attack occured.

```r
wounded_killed_PCTPOV017 <- midterm_data_ethan37 %>%
  group_by(nwound, nkill, PCTPOV017_2015) %>%
  summarise(wounded_killed = sum(nwound, nkill)) %>%
  ggplot() +
  geom_point(mapping = aes(x = PCTPOV017_2015, y = wounded_killed), alpha = .3) +
  labs(title = "Percent of residents 17 and under in poverty and amount of people wounded or
       killed",
       x = "Percent of residents 17 and under living in poverty",
       y = "Amount of people wounded or killed")

wounded_killed_PCTPOV017
```

![](first-midterm_files/figure-html/wounded-killed-PCTPOV017-1.png)<!-- -->

This graph is similar to the previous one and does not show a relationship between the amount of people wounded or killed and the percent of resident 17 and under living in poverty.

Now we will look at the relationship between the amount of wounded and killed combined and the median household income in the city the attack occured.

```r
wounded_killed_MEDHHINC <- midterm_data_ethan37 %>%
  group_by(nwound, nkill, MEDHHINC_2015, gname) %>%
  summarise(wounded_killed = sum(nwound, nkill)) %>%
  ggplot() +
  geom_point(mapping = aes(x = MEDHHINC_2015, y = wounded_killed, label = gname), alpha = .3) +
  labs(title = "Percent of residents 17 and under in poverty and amount of people wounded or
       killed",
       x = "Median household income",
       y = "Amount of people wounded or killed") +
  geom_text(aes(x = MEDHHINC_2015, 
                 y = wounded_killed,
                 label=ifelse(wounded_killed>150,as.character(gname),'')),
             nudge_x=6000, nudge_y=0)
```

```
## Warning: Ignoring unknown aesthetics: label
```

```r
wounded_killed_MEDHHINC
```

![](first-midterm_files/figure-html/wounded-killed-MEDHHINC-1.png)<!-- -->

([labeling a point](https://stackoverflow.com/questions/15624656/label-points-in-geom-point)) 
From this graph, it does not look like there is a relationship between median household income and the amount of people wounded or killed in the attack. We do, although, have a point of interest that is in every graph. This point would be the one that is above 150 wounded or killed. The group that commited this attack is unknown.

Now we will look at the relationship between the attack type and the population of the city the attack occured in.

```r
attack_type_population <- midterm_data_ethan37 %>%
  mutate(attacktype1_txt = case_when( attacktype1_txt %in% "Hijacking" ~ "other", 
                                      attacktype1_txt %in% "Hostage Taking (Barricade Incident)" ~ "other",
                                      attacktype1_txt %in% "Assassination" ~ "other",
                                      attacktype1_txt %in% "Armed Assault" ~ "Armed Assault", 
                                      attacktype1_txt %in% "Bombing/Explosion" ~ "Bombing/Explosion", 
                                      attacktype1_txt %in% "Facility/Infrastructure Attack" ~ "Facility/Infrastructure Attack", 
                                      attacktype1_txt %in% "Unarmed Assault" ~ "Unarmed Assault")) %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = attacktype1_txt, y = population_estimate)) +
  scale_y_log10() +
  labs(title = "Type of attack and population",
       x = "Type of attack",
       y = "log10 of population")

attack_type_population
```

![](first-midterm_files/figure-html/attack_type-population-1.png)<!-- -->

Here we can see that in an area with high population, the attack that happens most often is an unarmed assault. While other types of attacks do occur in high populations, they mostly occur in lower populations, with bombing and explosion attacks happening at the lowest populations.

Now we will look at the relationship between the type of attack and the percent of all residents living in poverty in the city the attack occured.

```r
attack_type_PCTPOVALL <- midterm_data_ethan37 %>%
mutate(attacktype1_txt = case_when( attacktype1_txt %in% "Hijacking" ~ "other", 
                                      attacktype1_txt %in% "Hostage Taking (Barricade Incident)" ~ "other",
                                      attacktype1_txt %in% "Assassination" ~ "other",
                                      attacktype1_txt %in% "Armed Assault" ~ "Armed Assault", 
                                      attacktype1_txt %in% "Bombing/Explosion" ~ "Bombing/Explosion", 
                                      attacktype1_txt %in% "Facility/Infrastructure Attack" ~ "Facility/Infrastructure Attack", 
                                      attacktype1_txt %in% "Unarmed Assault" ~ "Unarmed Assault")) %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = attacktype1_txt, y = PCTPOVALL_2015)) +
  labs(title = "Type of attack and percent of all residents living in poverty",
       x = "Type of attack",
       y = "Percent of all residents in poverty")

attack_type_PCTPOVALL
```

![](first-midterm_files/figure-html/attack_type-PCTPOVALL-1.png)<!-- -->

Here we can see that there is no relationship between the type of attack and the percent of all residents living in poverty in the city the attack took place.

Now we will look at the relationship between the type of the attack and the percent of residents 17 and under living in poverty in the city the attack occured.

```r
attack_type_PCTPOV017 <- midterm_data_ethan37 %>%
  mutate(attacktype1_txt = case_when( attacktype1_txt %in% "Hijacking" ~ "other", 
                                      attacktype1_txt %in% "Hostage Taking (Barricade Incident)" ~ "other",
                                      attacktype1_txt %in% "Assassination" ~ "other",
                                      attacktype1_txt %in% "Armed Assault" ~ "Armed Assault", 
                                      attacktype1_txt %in% "Bombing/Explosion" ~ "Bombing/Explosion", 
                                      attacktype1_txt %in% "Facility/Infrastructure Attack" ~ "Facility/Infrastructure Attack", 
                                      attacktype1_txt %in% "Unarmed Assault" ~ "Unarmed Assault")) %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = attacktype1_txt, y = PCTPOV017_2015)) +
  labs(title = "Type of attack and percent of residents 17 and under living in poverty",
       x = "Type of attack",
       y = "Percent of residents 17 and under in poverty")

attack_type_PCTPOV017
```

![](first-midterm_files/figure-html/attack_type-PCTPOV017-1.png)<!-- -->

Here we can see that there is no relationship between the type of attack and the percent of residents that are the age 17 and under that are living in poverty in the city that the attack took place.

Now we will look at the relationship between the type of attack and the median household income in the city the attack occured.

```r
attack_type_MEDHHINC <- midterm_data_ethan37 %>%
  mutate(attacktype1_txt = case_when( attacktype1_txt %in% "Hijacking" ~ "other", 
                                      attacktype1_txt %in% "Hostage Taking (Barricade Incident)" ~ "other",
                                      attacktype1_txt %in% "Assassination" ~ "other",
                                      attacktype1_txt %in% "Armed Assault" ~ "Armed Assault", 
                                      attacktype1_txt %in% "Bombing/Explosion" ~ "Bombing/Explosion", 
                                      attacktype1_txt %in% "Facility/Infrastructure Attack" ~ "Facility/Infrastructure Attack", 
                                      attacktype1_txt %in% "Unarmed Assault" ~ "Unarmed Assault")) %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = attacktype1_txt, y = MEDHHINC_2015)) +
  labs(title = "Type of attack and median household income",
       x = "Type of attack",
       y = "Median household income")

attack_type_MEDHHINC
```

![](first-midterm_files/figure-html/attack_type-MEDHHINC-1.png)<!-- -->

Here we can see again that there is no relationship between the type of attack and the median household income in the city the attack took place.

Now we will explore the relationship between the type of attack and if property was damaged in the attack.

```r
attack_type_property <- midterm_data_ethan37 %>%
  mutate(attacktype1_txt = case_when( attacktype1_txt %in% "Hijacking" ~ "other", 
                                      attacktype1_txt %in% "Hostage Taking (Barricade Incident)" ~ "other",
                                      attacktype1_txt %in% "Assassination" ~ "other",
                                      attacktype1_txt %in% "Armed Assault" ~ "Armed Assault", 
                                      attacktype1_txt %in% "Bombing/Explosion" ~ "Bombing/Explosion", 
                                      attacktype1_txt %in% "Facility/Infrastructure Attack" ~ "Facility/Infrastructure Attack", 
                                      attacktype1_txt %in% "Unarmed Assault" ~ "Unarmed Assault")) %>%
  mutate(property = case_when( property == 1 ~ "Yes",
                               property == 0 ~ "No")) %>%
  ggplot() +
  geom_count(mapping = aes(x = property, y = attacktype1_txt, size = stat(prop), group = attacktype1_txt, color = stat(prop)), na.rm = TRUE)+
  labs(title = "Type of attack and whether or not property damage was done",
       x = "Property damage",
       y = "Type of attack")+
  guides(size = FALSE)

attack_type_property
```

![](first-midterm_files/figure-html/attack_type-property-1.png)<!-- -->

([geom_count](https://www.rdocumentation.org/packages/ggplot2/versions/3.2.1/topics/geom_count)) 
From this chart, we can see that facility/infrastructure attacks most commonly cause property damage, and unarmed assaults most commonly do not cause property damage. The other categories do not have a as much of a relationship with whether or not property damage was done.

Last but not least, we will explore the relationship between the type of attack and if the attack was committed by only one person.

```r
attack_type_individual <- midterm_data_ethan37 %>%
  mutate(attacktype1_txt = case_when( attacktype1_txt %in% "Hijacking" ~ "other", 
                                      attacktype1_txt %in% "Hostage Taking (Barricade Incident)" ~ "other",
                                      attacktype1_txt %in% "Assassination" ~ "other",
                                      attacktype1_txt %in% "Armed Assault" ~ "Armed Assault", 
                                      attacktype1_txt %in% "Bombing/Explosion" ~ "Bombing/Explosion", 
                                      attacktype1_txt %in% "Facility/Infrastructure Attack" ~ "Facility/Infrastructure Attack", 
                                      attacktype1_txt %in% "Unarmed Assault" ~ "Unarmed Assault")) %>%
  mutate(individual = case_when( individual == 1 ~ "Yes",
                                 individual == 0 ~ "No")) %>%
  ggplot() +
  geom_count(mapping = aes(x = individual, y = attacktype1_txt, size = stat(prop), group = attacktype1_txt, color = stat(prop)))+
  labs(title = "Type of attack and if the attack was committed by an individual",
       x = "Committed by individual",
       y = "Type of attack")+
  guides(size = FALSE)

attack_type_individual
```

![](first-midterm_files/figure-html/attack_type-individual-1.png)<!-- -->

Here we can see that facility/infastructure attacks are usually committed by a group, and unarmed assaults, armed assaults, and other attacks not listed are usually committed by individuals. Bombing/explosion attacks are committed by individuals and groups almost equally.

All in all, from these charts, the variables that have relationships with each other are location and percent of all residents in poverty, percent of residents 17 and under in poverty, and median household income; amount of attacks per month for facility/infrastructure attacks; if the attack was committed by an individual and if it was an armed assault attack, or a facility/infrastructure attack; if someone claimed responsibility for an attack and if it was a facility/infrastructure attack; the amount of people wounded or killed in the attack and the population of the city the attack was in; if the attack was an unarmed assualt and the population of the city the attack was in; whether or not property damage was done and if was an unarmed assualt or a facility/infrastructure attack; and finally, if it was committed by an individual and it was an unarmed assualt, other attack not listed, facility/infrastructure attack, or an armed assualt. 






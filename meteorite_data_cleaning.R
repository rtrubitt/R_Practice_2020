#Data upload and cleaning with the Meteorite Landing Dataset
##https://catalog.data.gov/dataset/meteorite-landings
##R version 4.0.2

#Install necessary packages
install.packages("tidyverse")
library("tidyverse")
install.packages("pivottabler")
library("pivottabler")

#Upload dataset
meteorites <- read.csv("C:/Users/Rebecca Trubitt/Downloads/Meteorite_Landings.csv")

#Step 1: Familiarization with the dataset
head(meteorites)
str(meteorites1)
summary(meteorites)

#Step 2: Edit columns for clarity
##Column 1: "name"- This is the name of the meteorite. No changes needed.

##Column 2: "id"- This is simply an ID number. No changes needed.

##Column 3: "nametype" - This column tells whether a meteorite was found directly after falling (valid) or had been discovered some time after the fact (relic).
  ##It is currently stored as a string, but we will change it to a factor to save processing space.We'll also change the name.
meteorites <- meteorites %>% rename(valid_mass = nametype)
meteorites$valid_mass <- as.factor(meteorites$valid_mass)

##Column 4: "recclass"- This column describes the classification of the found meteorite. For now, we'll rename this and change it to a factor as with the previous column.
meteorites <- meteorites %>% rename(class = recclass)
meteorites$class <- as.factor(meteorites$class)

##Column 5: "mass..g."- Mass in grams. We won't make any changes to this column for now.

##Column 6: "fall"- Again, this is a character column that we will change to a factor.
meteorites$fall <- as.factor(meteorites$fall)

##Column 7: "year"- This is the year of the meteorite fall/find. It is currently in a string with a full date and time. We can separate this out for more effective analysis.
meteorites <- meteorites %>% separate(year, into=c("month","day","year","time","am"), sep="[ /]")
##Although it appears that the year is the only changing variable, I'm choosing to separate rather than extract so that I can check that the other variables are actually empty before deleting them.
##To take a quick look at the variables I think are uninformative, I'll switch them to factors and then use summary() to investigate variability.
meteorites$month <- as.factor(meteorites$month)
meteorites$day <- as.factor(meteorites$day)
meteorites$time <- as.factor(meteorites$time)
meteorites$am <- as.factor(meteorites$am)

meteorites$year <- as.numeric(meteorites$year) #We'll change the year to a numeric variable for later analysis.

##There are a few observations that show variation in the non-year fields, but at a very minimal level (maximum of eight non-standard observations). I'll remove these as they are relatively uninformative.
meteorites<- select(meteorites, c(-month, -day, -time, -am))

##Columns 8 and 9: "reclat" and "reclong"- Change the names to lat and long.
meteorites <- meteorites %>% rename(lat = reclat)
meteorites <- meteorites %>% rename(long = reclong)

##Column 10: "GeoLocation" is a repeat of the latitude and longitude information, so I'll remove it.
meteorites <-select(meteorites, -GeoLocation)

#Saving our progress for later:
write.csv(meteorites, "C:/Users/Rebecca Trubitt/Downloads/Meteorite_Landings_Edited.csv")
#code to start from this point:
#meteorites <- read.csv("C:/Users/Rebecca Trubitt/Downloads/Meteorite_Landings_Edited.csv")

#Step 3: Look for impossible data.
##Numeric columns- each of these has logical cutoffs.
##Mass column- mass cannot be negative or zero. We can tell from summary() that there are no negative values, but we do have at least one zero.
zeromass <- filter(meteorites, mass..g.== 0) #Take a look at our missing mass records.
meteorites$mass..g.[meteorites$mass..g. == 0] <- NA #Switch zeros to NAs.

##Year column - No future dates allowed.
futureyears <- filter(meteorites, year>2020) #There is only one future year. We could replace this with NA, or review the original data to fix the typo. I looked this one up online, and it was found in 2010.
meteorites$year[30684]=2010 #Replacing value

##Latitude and longitude - Latitude must be between -90 and 90 degrees, and longitude must be between -180 and 180.
##We can see from summary() that all of the latitude observations are within range, but there is at least one impossibly high longitude.
highlong<-filter(meteorites, long > 180)
## There is only one impossible data point. As with the impossible year record, I looked up this data point to see if I could identify and fix a typo. However, it turns out that this is not a typo.
## This meteorite was found on Mars, and the recorded position is Martian latitude and longitude. This is not noted specifically in our dataset, and that raises the additional question of how many of the other
## observations are Martian? I compared a list of official Martian meteorites to our database (by hand using the name field, as there were only 15 records in this list), and it appears that
## this is the only Martian find on the list. The best way forward is dictated by the question we're actually trying to answer. Since this is just a data cleaning exercise, I will just leave this observation
## for now, as it is technically accurate.

##There are are also many zeros in the lat/long data- we can investigate whether these are true zeros or missing data.
zeroloc<- meteorites %>% filter(lat==0)
zerolong<-meteorites %>% filter(long==0)
##All of the records with 0 longitude also have 0 latitude. I checked a few of these, and all were listed as location unknown in the Meteorological Society meteorite database.As there is no land at (0,0), it isn't an unreasonable assumption that these are all NAs.
##On the other hand, there are about 200 records that have a 0 latitude and a value in longitude. All but one are from the same set, which were collected in Antarctica. These are listed
##in the TMS database with two sets of Lat/Long data, one of which has 0 latitude. These could be re-pulled and edited now that we've found the issue. The last one appears to have actually been collected at a 0 latitude location in Kenya, so it should be kept as is.
##Based on this, I need to change the lat and longs to NAs for observations with a longitude of 0.
meteorites$long[meteorites$long == 0] <- NA
meteorites$lat[is.na(meteorites$long)] <- NA

#Step 4: Evaluate missing or duplicate data
meteorites <- meteorites %>% distinct() #This removes one duplicated record, which I planted in for illustrative purposes.
  
##For missing data, there are a few different approaches, including removal of the rows with missing data, removal of variables with excessive missing data,
## or inputting the average, or inputting data from the existing data's distribution. We can take a look at the summary to see how many records are NAs from each column.
summary(meteorites)
##I'll remove the rows that are missing values for all but location data, where I'd like to take a closer look.
meteorites_cut <- meteorites %>% filter(!is.na(year), !is.na(mass..g.))

##Lat and Long stand out here, missing appx. 30% of their records. I want to see if these missing records are associated with any other variables, particularly year- perhaps there were particular years with poor location based record keeping.
##To simplify this process, we'll make an new column that indicates whether the location is missing or not.
meteorites_cut <- mutate(meteorites_cut, 
       missingloc = if_else(is.na(meteorites_cut$lat), "Missing", "Present"))

##Using this new column, we can make quick pivot tables to see if particular years/categories are missing more data.
qhpvt(meteorites_cut, "fall", "missingloc", "n()") #A higher proportion of the found meteorites do not have location data, but the proportion of found to fell meteorites is so high that it throws things off a bit.
qhpvt(meteorites_cut, "valid_mass", "missingloc", "n()") #Similar issue as above; there are so few relic meteorites that the distribution of missing locations in these categories is a bit meaningless.
qhpvt(meteorites_cut, "year", "missingloc", "n()") #This makes a pretty big pivot table, but from it we can see that we do have particular years with high proportions of missing location data, but no years that are entirely missing their locations.
##Looking manually at the years with lots of missing data, it appears that there are specific expeditions (e.g. the Yamato meteorites in 1998) that don't have location data, and these account for most (but not all) of the missing locations.
##Since there are so many observations with no location data, I will not remove all of them unless I wanted to ask a specifically location-based question. In that case, I could cut the dataset using my missingloc column.

#Step 5: Check for spelling errors
  
## For categories like "class", misspelled or shortened entries could read as an additional factor. We want to check for that and standardize spellings as needed.
levels(factor(meteorites_cut$class))
## There are 457 (!) different factors listed here. Right off the bat we can clean it up by standardizing case and removing trailing spaces.
meteorites_cut$class<-str_to_upper(meteorites_cut$class) #Down to 456 factors by standardizing case.
meteorites_cut$class<-str_trim(meteorites_cut$class) #Down to 445 factors by removing hanging white spaces.

## We've cut out some extraneous categories, but still have some issues. For example, we have listings for both "L5-6" and "L5/6", which appear to be synonymous.
## I also think that combining some of these smaller categories into larger ones, we can have a more analyzable dataset. I don't really know anything about meteorite classes, so this is really more for illustrative purposes.
## For illustrative purposes, I'll just do this with the H type meteorites. 
htype<-meteorites_cut %>% filter(str_detect(class, "^H"))

htype$class2 <- fct_collapse(htype$class,
          "H"= c("H", "H-AN", "H?"),
          "H-MINERAL"= c("H-IMP MELT", 	"H-MELT BRECCIA", "H-MELT ROCK", "H-METAL", "H4-MELT BRECCIA", "H5-MELT BRECCIA", "H6-MELT BRECCIA"),
          "HL"= c("H(L)3", "H(L)3-AN", "H/L~4", "H/L3", "H/L3-4", "H/L3.5","H/L3.6", "H/L3.7", "H/L3.9","H/L4", "H/L4-5","H/L5","H/L6"),
          "H3"= c("H3", "H3-AN"),
          "H3-4"= c("H3-4", "H3/4"),
          "H3-5"= "H3-5",
          "H3-6"= "H3-6",
          "H3.X"=c("H3.0", "H3.0-3.4", "H3.05", "H3.1", "H3.10", "H3.2", "H3.2-3.7", "H3.2-6", "H3.2-AN", "H3.3", "H3.4", "H3.4-5", "H3.4/3.5", "H3.5", 	
                  "H3.5-4","H3.6", "H3.6-6","H3.7","H3.7-5", "H3.7-6","H3.7/3.8","H3.8","H3.8-4", "H3.8-5", "H3.8-6", "H3.8-AN", "H3.8/3.9", "H3.8/4", 	
                  "H3.9", "H3.9-5", "H3.9-6", "H3.9/4"),
          "H4"=c("H(?)4" , "H~4", "H4", "H4-AN", "H4(?)"),
          "H4-5"= c("H~4/5", "H4-5", "H4/5"),
          "H4-6"=c("H4-6","H4/6"),
          "H5"= c("H(5?)" ,"H5", "H5-AN","H~5"),
          "H5-6"= c("H5-6", "H5/6"),
          "H6"= c("H~6", "H6"),
          "H6-7"= "H6/7",
          "H7"= "H7",
          "HOWARDITE"= c("HOWARDITE", "HOWARDITE-AN"))
levels(factor(htype$class2))
##This consolidates the H type meteorites into 20 classes, and could be extended to the full dataset.

#These data cleaning steps have helped us 

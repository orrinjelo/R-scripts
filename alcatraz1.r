# Loading data

train.fresh <- read_csv('~/Data/Criminals/train.csv', 
                        col_types = list(Dates = col_datetime("%Y-%m-%d %H:%M:%S")))
test.fresh <- read.csv('~/Data/Criminals/test.csv')

# Load me up some libraries -- make sure these are installed by 'install.packages(...)'
# Ha ha, I updated R to 3.1.2, and I had to reinstall these.  T_T  Took forever.

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(randomForest)

# Play with the data 

train.fresh$when <- as.POSIXct(round(train.fresh$Dates, units = "days"))
train.fresh$friendly.when <- as.POSIXlt(round(train.fresh$Dates, units = "days"))
        # Puts it in an easier format than ct
train.fresh$friendly.when <- NULL # ggplot hates it. T_T
train.fresh$DayOfWeek <- factor(train.fresh$DayOfWeek, levels= c("Sunday", "Monday", 
                            "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

gta <- train.fresh[train.fresh$Category == "VEHICLE THEFT",]


# Looking at the data for GTA, avoiding all Jerry Rice references
ggplot(count(gta, Category, when), aes(x = when, y = n)) +
    geom_point() +
    ylab('"Vehicle thefts" per day') +
    xlab('Day of "vehicle theft"') +
    ggtitle("'Misleading' Plot of Thefts per Day")

# Filtering out cars not stolen
gta.stolen <- gta[!grepl("RECOVERED",gta$Descript),]

# Did we do it?
ggplot(count(gta.stolen, Category, when), aes(x = when, y = n)) +
    geom_point() +
    ylab('"Vehicle thefts" per day') +
    xlab('Day of "vehicle theft"') +
    ggtitle("Plot of Thefts per Day")

# Let's look at other crimes
levels(factor(train.fresh$Category))

arson <- train.fresh[train.fresh$Category == "ARSON",] # Plot looks decent, but few
assault <- train.fresh[train.fresh$Category == "ASSAULT",] # Looks nice
bad.checks <- train.fresh[train.fresh$Category == "BAD CHECKS",] # Rarely more than 1-2
bribery <- train.fresh[train.fresh$Category == "BRIBERY",] # Same as bad.checks
burglary <- train.fresh[train.fresh$Category == "BURGLARY",] # Has a 'wave-like' pattern like gta
disorderly <- train.fresh[train.fresh$Category == "DISORDERLY CONDUCT",] # 1-12 per day
dui <- train.fresh[train.fresh$Category == "DRIVING UNDER THE INFLUENCE",] # Few
drug <- train.fresh[train.fresh$Category == "DRUG/NARCOTIC",] # Holy cow, lots
drunk <- train.fresh[train.fresh$Category == "DRUNKENNESS",] # Not as common as you think
embezzlement <- train.fresh[train.fresh$Category == "EMBEZZLEMENT",] # Few
extortion <- train.fresh[train.fresh$Category == "EXTORTION",] # Rare
family <- train.fresh[train.fresh$Category == "FAMILY OFFENSES",] # Very few
# Other stuff ...
gambling <- train.fresh[train.fresh$Category == "GAMBLING",] # Rare
kidnapping <- train.fresh[train.fresh$Category == "KIDNAPPING",] # Few, but surprising
theft <- train.fresh[train.fresh$Category == "LARCENY/THEFT",] # Holy crap, this is something to look at
loitering <- train.fresh[train.fresh$Category == "LOITERING",] # Interesting...
missing.person <- train.fresh[train.fresh$Category == "MISSING PERSON",] # Surprisingly intersting
non.criminal <- train.fresh[train.fresh$Category == "NON-CRIMINAL",] # Also worth looking at
other <- train.fresh[train.fresh$Category == "OTHER OFFENSES",] # Look again...why the gap?
porn <- train.fresh[train.fresh$Category == "PORNOGRAPHY/OBSCENE MAT",] # One or none.
prostitution <- train.fresh[train.fresh$Category == "PROSTITUTION",] # 2014-11-26 UTC: day of the prostitutes
robbery <- train.fresh[train.fresh$Category == "ROBBERY",] # Thick and chunky
runaway <- train.fresh[train.fresh$Category == "RUNAWAY",] # 0-9
suicide <- train.fresh[train.fresh$Category == "SUICIDE",] # 1-2 a day, no more than 4
vandalism <- train.fresh[train.fresh$Category == "VANDALISM",] # Beautiful

# Customize plot to your preference.
# ggplot(count(vandalism, Category, when), aes(x=when, y=n)) + geom_point() 

# Looking for a correlation between the two...
gta.counts <- count(gta.stolen, Category, when)
burglary.counts <- count(burglary, Category, when)

plot(gta.counts$n, burglary.counts$n[-length(burglary.counts)])
# Okay, there's not a strong correlation...

# Let's look at days of the week
ggplot(data=count(gta.stolen, DayOfWeek), mapping=aes(DayOfWeek,n)) + geom_bar(stat="identity")

# What I learned from this:  Friday is the most common day, and weekends are slightly higher.
# Tuesday is the lowest, and people aren't particularly caring about a Sabbath day.  Not a strong
# source of crime.  

# Is prostitution more common on the weekends?
ggplot(data=count(prostitution, DayOfWeek), mapping=aes(DayOfWeek,n)) + geom_bar(stat="identity")

# Wow.  It's more common during the middle of the week!  Mondays is a low.\
# How about suicide?

ggplot(data=count(suicide, DayOfWeek), mapping=aes(DayOfWeek,n)) + geom_bar(stat="identity")

# Thursday is a good day to die. :(
# Runaways?

ggplot(data=count(runaway, DayOfWeek), mapping=aes(DayOfWeek,n)) + geom_bar(stat="identity")

# Fridays are most common...
# Missing persons?  Kidnappings?

ggplot(data=count(missing.person, DayOfWeek), mapping=aes(DayOfWeek,n)) + geom_bar(stat="identity")
ggplot(data=count(kidnapping, DayOfWeek), mapping=aes(DayOfWeek,n)) + geom_bar(stat="identity")

# Fridays are a common thing...
# Drunk?

ggplot(data=count(drunk, DayOfWeek), mapping=aes(DayOfWeek,n)) + geom_bar(stat="identity")

# Hey!  People drink during the weekends!  Go figure!
# How about arson?

ggplot(data=count(arson, DayOfWeek), mapping=aes(DayOfWeek,n)) + geom_bar(stat="identity")

# Not a big difference.
# Bad checks?

ggplot(data=count(bad.checks, DayOfWeek), mapping=aes(DayOfWeek,n)) + geom_bar(stat="identity")

# Banks are closed on Sunday, I guess.
# DUI?

ggplot(data=count(dui, DayOfWeek), mapping=aes(DayOfWeek,n)) + geom_bar(stat="identity")

# Weekends.  Go figure.
# Family-related stuff?

ggplot(data=count(family, DayOfWeek), mapping=aes(DayOfWeek,n)) + geom_bar(stat="identity")

# During the week?  Friday?
# Gambling?

ggplot(data=count(gambling, DayOfWeek), mapping=aes(DayOfWeek,n)) + geom_bar(stat="identity")

# Wednesday and Friday, I suppose, are good days to roll the dice.

# Now let's see about whether one thing is practiced in one area or not.
# I think hookers hang out in the same part of town.  Prove me wrong?

ggplot(data=count(prostitution, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

# They like hanging around Mission District, looks like...
# Arson?

ggplot(data=count(arson, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

# Burninate the Bayview-side!
# Burglary?  Theft, Robbery?

ggplot(data=count(burglary, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

ggplot(data=count(robbery, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

ggplot(data=count(theft, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

ggplot(data=count(merge(burglary, theft, robbery, 
                        by=c("Category", "PdDistrict")), PdDistrict), 
       mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")


# Meh, not too impressive, except Tenderloin has it pretty low.
# Like prostitution, I think drugs might be regional.

ggplot(data=count(drug, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

# Well, looky there!  Tenderloin has a druggy problem.  In fact....let's see what kind of drug?

Tenderloin.drug <- drug[drug$PdDistrict == "TENDERLOIN",]
table(Tenderloin.drug$Descript)

# or

ggplot(data=count(Tenderloin.drug, Descript), mapping=aes(Descript, n)) + 
    geom_bar(stat="identity")

# Rock cocaine and narcotics.  Now you know where to buy.
# Drunk...

ggplot(data=count(drunk, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

# Dui...

ggplot(data=count(dui, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

# Family

ggplot(data=count(family, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

# Funny, it's the same district with the prostitution...
# Gambling...

ggplot(data=count(gambling, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

# GTA

ggplot(data=count(gta.stolen, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

# People in Tenderloin are too doped up to do much bad.
# Loitering

ggplot(data=count(loitering, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

# People in Southern love to loiter.
# Missing persons and kidnappings

ggplot(data=count(missing.person, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

ggplot(data=count(kidnapping, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

ggplot(data=count(runaway, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

# Why would anyone want to runaway from drugtown?  Park, on the other hand...
# Porn?

ggplot(data=count(porn, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

# Non-criminal?

ggplot(data=count(non.criminal, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

# Same place as loitering.  I wonder what non-criminal means....

Southern.noncrimi <- non.criminal[non.criminal$PdDistrict == "SOUTHERN",]
ggplot(data=count(Southern.noncrimi, Descript), mapping=aes(Descript, n)) + 
    geom_bar(stat="identity")
table(Southern.noncrimi$Descript)

# Lost property, mentally disturbed/aided case, and found property.
# Suicides?

ggplot(data=count(suicide, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

# Park has lowest suicide rate, along with drugtown Tenderloin.
# Vandalism?

ggplot(data=count(vandalism, PdDistrict), mapping=aes(PdDistrict, n)) + 
    geom_bar(stat="identity")

# Most common in Southern.  

# Let's try planting some trees.

crime.forest <- randomForest(as.factor(Category) ~ DayOfWeek + PdDistrict + PdDistrict +  X + Y,
                             data=train.fresh, importance=TRUE, ntree=1000)

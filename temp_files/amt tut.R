

#https://cran.r-project.org/web/packages/amt/vignettes/p1_getting_started.html

# amt 
#install.packages("amt")

library(dplyr)
library(ggplot2)
library(amt)
df1 <- tibble(x = 1:3, y = 1:3)
is.data.frame(df1)


# Now we can create a track
tr1 <- make_track(df1, x, y)
is.data.frame(tr1)

class(tr1)


#To create a track_xyt we could do the following

df1 <- tibble(x = 1:3, y = 1:3, t = lubridate::ymd("2017-01-01") + lubridate::days(0:2))
tr2 <- make_track(df1, x, y, t)
class(tr2)



df1 <- tibble(x = 1:3, y = 1:3, t = lubridate::ymd("2017-01-01") + lubridate::days(0:2), 
              id = 1, age = 4)
df1


# first we only create a track_xy
tr3 <- make_track(df1, x, y, id = id, age = age)
tr3


# now lets create a track_xyt
tr4 <- make_track(df1, x, y, t, id = id, age = age)
tr4
data(sh)
head(sh)


# Before creating a track, we have to do some data cleaning:
#   
#   check if any coordinates are missing (and if so, remove the relocation),
# parse the date and time,
# create a time stamp,
# check for duplicated time stamps, and
# create two new columns for the id and month of the year.

# check if all observations are complete
all(complete.cases(sh)) # no action required

# parse date and time and create time stamps
sh$ts <- as.POSIXct(lubridate::ymd(sh$day) +
                      lubridate::hms(sh$time))


# check for duplicated time stamps
any(duplicated(sh$ts))

# We have some duplicated time stamps, these need to be removed prior to
# creating a track.
sh <- sh[!duplicated(sh$ts), ]


# create new columns
sh$id <- "Animal 1"
sh$month <- lubridate::month(sh$ts)


#Now we can create a track.

tr1 <- make_track(sh, x_epsg31467, y_epsg31467, ts, id = id, month = month)

tr1 <- make_track(sh, x_epsg31467, y_epsg31467, ts, id = id, month = month, 
                  crs = sp::CRS("+init=epsg:31467"))


### ok now I have turned my GPS data points into a track. - now what??


data(sh)
tr2 <- sh %>% filter(complete.cases(.)) %>% 
  mutate(
    ts = as.POSIXct(lubridate::ymd(day) + lubridate::hms(time)), 
    id = "Animal 1", 
    month = lubridate::month(ts)
  ) %>% 
  filter(!duplicated(ts)) %>% 
  make_track(x_epsg31467, y_epsg31467, ts, id = id, month = month, 
             crs = sp::CRS("+init=epsg:31467"))

tr3 <- tr2 %>% filter(month == 5)

# we are left with a track
class(tr3)


#Several functions for calculating derived quantities are available. 
#We will start with looking at step length. The function step_lengths can be used for this.

transform_coords(tr2, sp::CRS("+init=epsg:4326"))

tr2 <- tr2 %>% mutate(sl_ = step_lengths(.))
summary(tr2$sl_)

# Note, 
# 1) there is a NA for the last step length, 
# this is expected because we are still in a point representation (i.e., there is no step length for the last relocation). 
# 2) the range is fairly large ranging from 0 to almost 5 km. 
# Before looking at step lengths in any further detail, 
# we will have to make sure the sampling rate is more or less regular (i.e., the same time step between any two points).

summarize_sampling_rate(tr2)


# This suggests that a sampling rate for 6 hours might be adequate. 
# We can then use the function track_resample to resample the track and only keep relocations that are approximately 6 hours apart (within some tolerance, that can be specified). 
# We will use the function lubridate::hours to specify the sampling rate and lubridate::minutes to specify the tolerance. 
# Both arguments rate and tolerance are expected to be a Period.

str(tr2)
tr3 <- tr2 %>% track_resample(rate = hours(6), tolerance = minutes(20))
tr3
summarize_sampling_rate(tr3)
tr3 <- tr3 %>% mutate(sl_ = step_lengths(.))
summary(tr3$sl_)


### nested tracks on animal class


data("amt_fisher")
trk <- amt_fisher %>% make_track(x_, y_, t_, id = id)


# get the data for the first animal
x <- trk1$data[[1]]
x
# apply the data analysis
x %>% track_resample(rate = minutes(30), tolerance = minutes(5)) %>%
  steps_by_burst()
trk1 <- trk %>% nest(data = -"id")
trk1



trk2 <- trk1 %>% 
  mutate(steps = map(data, function(x) 
    x %>% track_resample(rate = minutes(30), tolerance = minutes(5)) %>% steps_by_burst()))

trk2
trk2a <- trk2 %>% select(id, steps) %>% unnest(cols = steps)

trk2 %>% select(id, steps) %>% unnest(cols = steps) %>% 
  ggplot(aes(sl_, fill = factor(id))) + geom_density(alpha = 0.4)

test <- trk2 %>% select(id, steps) %>% unnest(cols = steps)

print(test)
getwd()
write.csv(test,"C:/Users/ouz001/working_from_home_post_Sep2022/Waikerie_VF/amt_tut_step_lenghth.csv" )

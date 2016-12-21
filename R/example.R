load(file = 'data/trips.20160613_19.all.RData')

# Check the structure
str(univWeek)  # ~ 5 million rows


# Just subset to 10000 rows data
sampled <- univWeek[sample(1:nrow(univWeek), size = 10000, replace = FALSE) , ]

# About each column, see: http://www.nyc.gov/html/tlc/downloads/pdf/data_dictionary_trip_records_yellow.pdf
# inMt: in Manhattan

plot(sampled$pickup_longitude, sampled$pickup_latitude, pch=18)
plot(sampled$tip_amount, sampled$total_amount)

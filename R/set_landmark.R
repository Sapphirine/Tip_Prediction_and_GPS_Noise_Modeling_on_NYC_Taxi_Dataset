# STSt.: Street station
# HC: hunter college
# CPCond: Carnegie Park Condominium at 3rd Avenue
# 59ST-LASt.: 59 Street - Lexington Avenue Station
# Beacon: Beacon Theatre / Hotel Beacon
landmark <- data_frame( name = c('TSQ',   '51ST',
                                 'CU', 'Julliard', 'CCircle', '125ST', 'Beacon',
                                 '96STSt.', '68ST-HC', '3rd-CPC', '59ST-LASt.'),
                        area = c('MID', 'MID',
                                 'UES', 'UES', 'UES', 'UES', 'UES',
                                 'UWS', 'UWS', 'UWS', 'UWS'), 
                        type = c('Hub', 'Hub',
                                 'School', 'School', 'Hub', 'Edge', 'Luxury',
                                 'Edge', 'School', 'Cond', 'Edge'),
                        lat = c( 40.758855,  40.757002,
                                 40.807933,  40.773928,  40.768077, 40.815633, 40.780623,
                                 40.785807,  40.767900,  40.783647, 40.762846),
                        lon = c(-73.985134, -73.972018,
                                -73.963741, -73.982824, -73.981886, -73.958242, -73.981325,
                                -73.950722, -73.964059, -73.949683, -73.967722) ) 
metered <- latlon2meter(landmark$lon, landmark$lat)
landmark <- landmark %>% mutate(
    y = rotateManhattanY(metered@coords[,'lat'],metered@coords[,'lon'],gcl$EPSG2908$theta),
    x = rotateManhattanX(metered@coords[,'lat'],metered@coords[,'lon'],gcl$EPSG2908$theta)
)
rm(metered)

# Neighborhood boundaries in New York City are not officially set, but according to the Encyclopedia of New York City, the Upper East Side is bounded by 59th Street in the south, 96th Street on the north, Fifth Avenue to the west and the East River to the east.[26] The AIA Guide to New York City extends the northern boundary to 106th Street near Fifth Avenue.[27] (Wikipedia)
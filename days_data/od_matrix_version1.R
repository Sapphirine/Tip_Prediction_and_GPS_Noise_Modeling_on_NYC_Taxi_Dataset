load("/Users/qiucongying/Desktop/6893/trips.20160501_0531.preped.RData")
dim(univ1month)

options(max.print = 200)

rm(df)

## Since the data is large which contains 14376990 dimensions, I'm going to convert them to the tbl_df for a faster manipulation.
library(dplyr)
library(lubridate)
library(ggplot2)

univ1month <- tbl_df(univ1month)
class(univ1month)
str(univ1month)

## My goal is to analyze the weekend data for one month. Before doing this, I need to tidy it.

weekend_sample <- univ1month %>% 
                  sample_frac(0.25) %>%
                  filter(isWeekday == 0) %>%
                  mutate(day = wday(pickup_datetime, label = TRUE)) %>%
                  arrange(pickup_datetime)

test <- sample_frac(weekend_sample, 0.01)

## I'd like to get an inside look to the traffic volume distribution of NYC and create a OD matrix. One important work is to 
## determine how many sectioons/regions. Because I don't know anything about the data, visualizing "px" & "py" will be helpful 
## know trip location distribution. Then according to the preliminary results to intuitively decide what I should do next.

ggplot(weekend_sample, aes(px, py)) + geom_point(size = 0.001) + labs(x = "x", y = "y")

## Divide the areas into 12 components.

## First, define them. Create a logical function to classify each point. 

region1 <- function(px, py) {
  return(
    ifelse( px < -4000 & 45000 < py & py < 60000, 1, 0)
  )
}

region2 <- function(px, py) {
  return(
    ifelse( -4000 < px & px < -1300 & 45000 < py & py < 60000, 1, 0)
  )
}

region3 <- function(px, py) {
  return(
    ifelse( -1300 < px & px <  1300  & 45000 < py & py < 60000, 1, 0)
  )
}

region4 <- function(px, py) {
  return(
    ifelse(  1300 < px & 45000 < py & py < 60000, 1, 0)
  )
}

region5 <- function(px, py) {
  return(
    ifelse( px < -4000 & 31000 < py & py < 40000, 1, 0)
  )
}

region6 <- function(px, py) {
  return(
    ifelse( -4000 < px & px < -1300 & 31000 < py & py < 40000, 1, 0)
  )
}

region7 <- function(px, py) {
  return(
    ifelse( -1300 < px & px <  1300 & 31000 < py & py < 40000, 1, 0)
  )
}

region8 <- function(px, py) {
  return(
    ifelse( 1300 < px & 31000 < py & py < 40000, 1, 0)
  )
}

region9 <- function(px, py) {
  return(
    ifelse( px < -4000 & 21000 < py & py < 31000, 1, 0)
  )
}

region10 <- function(px, py) {
  return(
    ifelse( -4000 < px & px < -1300 & 21000 < py & py < 31000, 1, 0)
  )
}

region11 <- function(px, py) {
  return(
    ifelse( -1300 < px & px <  1300 & 21000 < py & py < 31000, 1, 0)
  )
}

region12 <- function(px, py) {
  return(
    ifelse(  1300 < px & 21000 < py & py < 31000, 1, 0)
  )
}


tiny <- weekend_sample[1:10000, ]


tiny %>% filter( py > 21000 ) %>%  ##analyze manhattan area
  mutate( pxcat = cut(px, breaks=4),
          pycat = cut(py, breaks=3),
          pcat = as.factor(paste0(pxcat,'and',pycat)) %>% as.numeric,
          pstr = paste0('Region_', pcat) ) %>% # dplyr::select(pxcat, pycat) %>%
  group_by(pxcat, pycat) %>% mutate( n = n() ) %>%
  ggplot() + geom_point(aes(px, py, label=LETTERS[ pcat ], color=pstr))
  #ggplot() + geom_text(aes(px, py, label=LETTERS[ pcat ], color=pstr))

tiny %>% filter( py > 21000 ) %>%
  mutate( pxcat = cut(px, breaks=4),
          pycat = cut(py, breaks=3),
          pcat = as.factor(paste0(pxcat,'and',pycat)) %>% as.numeric,
          pstr = paste0('Region_', pcat) ) %>% # dplyr::select(pxcat, pycat) %>%
  mutate( dxcat = cut(dx, breaks=4),
          dycat = cut(dy, breaks=3),
          dcat = as.factor(paste0(dxcat,'and',dycat)) %>% as.numeric,
          dstr = paste0('Region_', dcat) ) %>% # dplyr::select(pxcat, pycat) %>%
  group_by(pstr, pycat) %>% mutate( pn = n() ) %>%
  group_by(dxcat, dycat) %>% mutate( dn = n() ) 


xg = 400
yg = 4000

tmp <-
weekend_sample %>% filter( py > 21000 ) %>%
  mutate( npx = round(px,0) + (xg - round(px,0) %% xg),
          ndx = round(dx,0) + (xg - round(dx,0) %% xg),
          npy = round(py,0) + (yg - round(py,0) %% yg),
          ndy = round(dy,0) + (yg - round(dy,0) %% yg)
          )  %>%
  group_by(npx, npy) %>% mutate( pn = n() ) %>%
  group_by(ndx, ndy) %>% mutate( dn = n() ) %>%
  mutate( pxy = paste0(npx,',',npy),
          dxy = paste0(ndx,',',ndy)) %>%
  group_by(pxy,dxy) %>% mutate( ntrip = n() ) %>% dplyr::slice(1) # %>% dplyr::select(pxy,dxy,ntrip) %>% View
  
ggplot(tmp) + geom_rect(aes(xmin = npx, xmax = npx + xg,
                            ymin = npy, ymax = npy + yg, fill=ntrip ))





library(tidyverse)

##merging in R

mi <- tibble(
  id = c(1L, 1L, 1L, 2L, 2L),
  mi_date = as.Date(c("2018-12-31", "2019-01-02", "2019-01-05", "2019-01-04", "2019-01-01"))
) %>%
  mutate(lower = mi_date-1,
         upper= mi_date+1)

qol <- tibble(
  pid = c(1L, 1L, 2L),
  qol_date = as.Date(c("2019-01-01", "2019-01-05", "2019-01-02"))
) %>%
  mutate(lower = qol_date-1,
         upper= qol_date+1)

mi %>%
  left_join(qol, by = "id") %>%
  mutate(diff = mi_date - qol_date)

mi %>%
  full_join(qol, join_by(id==pid, mi_date==qol_date)) ->joinby

## different types of joins
?join_by

closest<-left_join(mi, qol, join_by(id==pid, closest(mi_date>=qol_date)))

bet<-left_join(mi, qol, join_by(id==pid, between(x$lower, y$lower, y$upper)))

within<-left_join(mi, qol, join_by(id==pid, within(x$lower,x$upper, y$lower, y$upper)))

bet<-left_join(mi, qol, join_by(id==pid, between(x$lower,x$upper,  y$lower, y$upper)))

##the one that was most useful for me was within!

write_record <- function(planning_log) {
  planning_log$geometry <- st_as_text(planning_log$geometry)

  data.table::fwrite(
    data.frame(planning_log),
    'henrico_planning_log.csv',
    append = TRUE)

  'henrico_planning_log.csv'
}

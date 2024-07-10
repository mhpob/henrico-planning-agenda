#######
# Henrico

henrico_gis_base <- 'https://portal.henrico.us/mapping/rest/services/Layers/'

#####
# Parcels
####

grab_parcels <- function(agenda){
  paste0(
    henrico_gis_base,
    "Tax_Parcels_and_CAMA_Data_External/MapServer/0/query?where=GPIN IN ('",
    paste(agenda$parcel, collapse = "','"),
    "')",
    "&outFields=GPIN,FULL_ADDRESS,ZIP_CODE,USE_DESCRIPTION,ACREAGE,YEAR_BUILT",
    "&outSR=4326&f=json"
  ) |>
    URLencode() |>
    st_read(quiet = TRUE)
}

join_parcels <- function(agenda, parcels){
  agenda <- agenda |>
    left_join(parcels, by = join_by(parcel == GPIN)) |>
    st_as_sf(sf_column_name = 'geometry')

  agenda[, c('case', 'hearing_date', 'parcel', 'district', 'FULL_ADDRESS',
             'ZIP_CODE', 'USE_DESCRIPTION', 'ACREAGE', 'YEAR_BUILT',
             'applicant', 'representative', 'staff', 'description', 'geometry')]
}


#####
# Existing land use
####

grab_existing_use <- function(agenda){
  paste0(
    henrico_gis_base,
    "Existing_Land_Use/MapServer/0/query?where=GPIN IN ('",
    paste(agenda$parcel, collapse = "','"),
    "outFields=GPIN,LANDUSE_DESCR",
    "&outSR=4326&f=json"
  ) |>
    URLencode() |>
    st_read(quiet = TRUE)
}

#future land use; no gpin
# https://portal.henrico.us/mapping/rest/services/Layers/Future_Land_Use_2026/MapServer/0/query?outFields=*&where=1%3D1



#####
# Leaflet
####
build_leaflet <- function(parcels) {
  casePal <- leaflet::colorFactor(
    "viridis",
    parcels$case
  )
  labels <- paste(
    "Case: ", parcels$case, "<br>",
    "Applicant: ", parcels$applicant, "<br>",
    "Representative: ", parcels$representative, "<br>",
    "Staff: ", parcels$staff, "<br>",
    "Address: ", paste(parcels$FULL_ADDRESS, parcels$ZIP_CODE), "<br>",
    "Parcel: ", parcels$parcel, "<br>",
    "Use: ", parcels$USE_DESCRIPTION, "<br>",
    "Acreage: ", parcels$ACREAGE, "<br>",
    "Year built: ", parcels$YEAR_BUILT
  ) |>
    lapply(htmltools::HTML)


  leaflet(parcels) |>
    addProviderTiles(providers$CartoDB.Positron) |>
    addPolygons(
      color = ~ casePal(case),
      fillColor = ~ casePal(case),
      fillOpacity = 0.5,
      label = labels,
      highlightOptions = highlightOptions(
        color = "white", weight = 2,
        bringToFront = TRUE)
    )
}

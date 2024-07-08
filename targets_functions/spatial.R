grab_parcels <- function(agenda){
  paste0(
    'https://portal.henrico.us/mapping/rest/services/Layers/Tax_Parcels_and_CAMA_Data_External/MapServer/0/query?where=GPIN IN ',
    "('",
    paste(agenda$parcel, collapse = "','"),
    "')",
    "&outFields=GPIN,FULL_ADDRESS,ZIP_CODE,USE_DESCRIPTION,ACREAGE,YEAR_BUILT",
    "&outSR=4326&f=json"
  ) |>
    URLencode() |>
    st_read(quiet = TRUE)
}

join_parcels <- function(agenda, parcels){
  agenda |>
    left_join(parcels, by = join_by(parcel == GPIN)) |>
    st_as_sf(sf_column_name = 'geometry')
}


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

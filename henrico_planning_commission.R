library(pdftools)

pdf_info('https://henrico.gov/pdfs/planning/meetnext.pdf')
# Created and modified




#### Actual meat
agenda <-
  pdf_text('https://henrico.gov/pdfs/planning/meetnext.pdf') |>
  unlist() |>
  # convert repeated line breaks andpage numbers to carriage returns
  gsub('\\n{2,}|\\s{2,}\\d', '\r', x = _) |>
  # Fix parcels split across lines
  gsub('(?<=\\d-)\\n', '', x = _, perl = TRUE) |>
  # convert remaining new lines to spaces
  gsub('\\n', ' ', x = _, perl = TRUE) |>

  # split by magisterial district and misc. items
  strsplit('\\r+(?=(BROOKLAND|FAIRFIELD|THREE CHOPT|TUCKAHOE|VARINA|DISCUSSION|APPROVAL))',
           perl = TRUE) |>
  unlist()

agenda_date <-
  agenda[1] |>
  strsplit('\r') |>
  unlist() |>
  _[1] |>
  gsub(".*\\s{2,}", "", x = _) |>
  as.Date(format = "%B %d, %Y")



agenda <- agenda |>
  grep("^(BROOKLAND|FAIRFIELD|THREE CHOPT|TUCKAHOE|VARINA)", x=_, value = TRUE) |>
  lapply(
    function(.) {
      cases <- case_splitter(.)
      df <- data.frame(
        case = case_grabber(cases),

        district = district_grabber(.),

        staff = staff_grabber(cases)
      )

      ## Parcels
      parcels <- list()
      for(i in seq_along(cases)){
        parcels[[i]] <- data.frame(
          case = cases[[i]] |>
            case_grabber() |> unlist(),
          parcel = cases[[i]] |>
            parcel_grabber() |> unlist()
        )
      }

      parcels <- do.call(rbind, parcels)


      ## Applicants
      applicants <- list()
      for(i in seq_along(cases)){
        applicants[[i]] <- data.frame(
          case = cases[[i]] |>
            case_grabber() |> unlist(),
          applicant = cases[[i]] |>
            applicant_grabber() |> unlist(),
          representative = cases[[i]] |>
            applicant_rep_grabber() |> unlist()
        )
      }

      applicants <- do.call(rbind, applicants)


      ## Merge
      df <- merge(df, parcels)
      df <- merge(df, applicants)
    }
  ) |>
  do.call(rbind, args = _)

library(sf)

parcels <- paste0('https://portal.henrico.us/mapping/rest/services/Layers/Tax_Parcels_and_CAMA_Data_External/MapServer/0/query?where=GPIN IN ',
                  "('",
                  paste(agenda$parcel, collapse = "','"),
                  "')",
                  "&outFields=GPIN,FULL_ADDRESS,ZIP_CODE,USE_DESCRIPTION,ACREAGE,YEAR_BUILT",
                  "&outSR=4326&f=json"
) |>
  URLencode() |>
  st_read()

library(dplyr)

parcels <- agenda |>
  left_join(parcels, by = join_by(parcel == GPIN)) |>
  st_as_sf(sf_column_name = 'geometry')

library(leaflet)

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
    highlightOptions = highlightOptions(color = "white", weight = 2,
                                        bringToFront = TRUE)
  )


scrape_agenda <- function(pdf_url){
  pdf_text(pdf_url) |>
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
}

build_agenda_df <- function(agenda){
  the_date <- grab_agenda_date(agenda)

  agenda |>
    grep("^(BROOKLAND|FAIRFIELD|THREE CHOPT|TUCKAHOE|VARINA)", x=_, value = TRUE) |>
    lapply(
      function(.) {
        cases <- split_cases(.)
        df <- data.frame(
          hearing_date = the_date,

          case = grab_case(cases),

          district = grab_district(.),

          staff = grab_staff(cases),

          description = cases
        )

        ## Parcels
        parcels <- list()
        for(i in seq_along(cases)){
          parcels[[i]] <- data.frame(
            case = cases[[i]] |>
              grab_case() |> unlist(),
            parcel = cases[[i]] |>
              grab_parcel() |> unlist()
          )
        }

        parcels <- do.call(rbind, parcels)


        ## Applicants
        applicants <- list()
        for(i in seq_along(cases)){
          applicants[[i]] <- data.frame(
            case = cases[[i]] |>
              grab_case() |> unlist(),
            applicant = cases[[i]] |>
              grab_applicant() |> unlist(),
            representative = cases[[i]] |>
              grab_applicant_rep() |> unlist()
          )
        }

        applicants <- do.call(rbind, applicants)


        ## Merge
        df <- merge(df, parcels)
        df <- merge(df, applicants)
      }
    ) |>
    do.call(rbind, args = _)
}

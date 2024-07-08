split_cases <- function(x){
  x |>
    strsplit('\r') |>
    unlist() |>
    grep('(REZ|PUP)', x= _, value = TRUE)
}

grab_agenda_date <- function(agenda) {
  agenda[1] |>
    strsplit('\r') |>
    unlist() |>
    _[1] |>
    gsub(".*\\s{2,}", "", x = _) |>
    as.Date(format = "%B %d, %Y")
}


grab_district <- function(x) {
  gsub(
    '(?:(?<=BROOKLAND)|(?<=FAIRFIELD)|(?<=THREE CHOPT)|(?<=TUCKAHOE)|(?<=VARINA)): .*',
    '',
    x = x,
    perl = TRUE
  )
}


grab_case <- function(x) {
  sapply(
    x,
    function(.){
      regmatches(., regexpr('(REZ|PUP)[-0123456789]*(?= )', text = ., perl = T))
    },
    USE.NAMES = FALSE
  )
}


grab_staff <- function(x){
  sapply(
    x,
    function(.){
      # Be aware of the dash!! it's a different kind of dash!
      match <- regmatches(., regexpr('(?<=(Staff – )).*(?=\r)', text = ., perl = T))

      if (length(match) == 0){
        match <- regmatches(., regexpr('(?<=(Staff – )).*(?=$)', text = ., perl = T))
      }

      match
    },
    USE.NAMES = FALSE
  )
}


grab_parcel <- function(x){
  sapply(
    x,
    function(.){
      regmatches(., gregexpr('\\d{3}-\\d{3}-\\d{4}',
                             text = ., perl = T))
    },
    USE.NAMES = FALSE
  )
}


grab_applicant <- function(x){
  sapply(
    x,
    function(.){
      regmatches(
        .,
        regexpr('(?<= for ).*(?=: Request)',
                text = ., perl = T)
      )
    },
    USE.NAMES = FALSE
  )
}

grab_applicant_rep <- function(x){
  sapply(
    x,
    function(.){
      regmatches(
        .,
        regexpr('(?<=\\d{6} ).*(?= for .*: Request)',
                text = ., perl = T)
      )
    },
    USE.NAMES = FALSE
  )
}

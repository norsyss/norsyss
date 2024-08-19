get_providers_n_nhn <- function(){
  db <- DBI::dbConnect(
    odbc::odbc(),
    driver="ODBC Driver 17 for SQL Server",
    server="sql-sykdomspulsen",
    database="SykdomspulsenAnalyse",
    trusted_connection="yes"
  )

  command <- glue::glue(
    '
    select distinct(Behandler_Id) as providers_id, Konsultasjonsdato as date
    FROM Konsultasjon
    JOIN KonsultasjonTakst ON Id=KonsultasjonTakst.KonsultasjonId
    WHERE Takst IN (\'11ad\', \'11ak\', \'2ad\', \'2ak\', \'2fk\', \'2ae\', \'2aek\')
    '
  )
  d <- DBI::dbGetQuery(db, command)
  setDT(d)

  d[, isoyear := cstime::date_to_isoyear_n(date)]
  d[, isoyearweek := cstime::date_to_isoyearweek_c(date)]
  d[, isoweek := cstime::date_to_isoweek_n(date)]

  d <- d[,.(providers_id, isoyear, isoweek, isoyearweek)] |>
    unique()

  setorder(d, isoyear)

  collapsed <- d[isoyearweek >= "2006-01",.(
    providers_registered_n = length(unique(providers_id))
  ),keyby=.(isoyear, isoweek, isoyearweek)]

  collapsed[
    ,
    providers_expected_n :=
      pmax(
        providers_registered_n,
        shift(providers_registered_n, n=1L),
        shift(providers_registered_n, n=2L),
        shift(providers_registered_n, n=3L),
        shift(providers_registered_n, n=4L)
      ,
      #probs = c(0.75),
      na.rm = T
      ),
    by =.(
      isoweek
    )
  ]

  isoyearweeks <- sort(collapsed$isoyearweek, decreasing = TRUE)
  collapsed[!isoyearweek %in% isoyearweeks[1:26], providers_expected_n := providers_registered_n]
  collapsed[, completeness_pr1 := round(providers_registered_n / providers_expected_n, 4)]

  return(collapsed)
}

#' @export
get_providers_n <- function(location="NHN"){
  get_providers_n_nhn()
}

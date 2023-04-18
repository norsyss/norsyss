get_raw_data <- function(date_from, date_to){
  db <- DBI::dbConnect(
    odbc::odbc(),
    driver="ODBC Driver 17 for SQL Server",
    server="dm-prod",
    database="SykdomspulsenAnalyse",
    trusted_connection="yes"
  )

  command <- glue::glue(
    "select Id,Diagnose,PasientAlder,PasientKj\u00F8nn as PasientKjonn,BehandlerKommune,Konsultasjonsdato as date,Takst,Praksis from Konsultasjon join KonsultasjonDiagnose on Id=KonsultasjonId join KonsultasjonTakst on Id=KonsultasjonTakst.KonsultasjonId where Konsultasjonsdato >='{date_from}' AND Konsultasjonsdato<='{date_to}'"
  )
  d <- DBI::dbGetQuery(db, command)

  attr(d, "date_from") <- date_from
  attr(d, "date_to") <- date_to

  return(d)
}

#' @export
get_and_process_raw_data <- function(isoyearweek_from = "2021-02", isoyearweek_to = isoyearweek_from){
  # isoyearweek_from <- "2022-52"
  # isoyearweek_to <- "2022-52"
  date_from <- cstime::dates_by_isoyearweek[isoyearweek>=isoyearweek_from & isoyearweek<=isoyearweek_to]$mon %>% min()
  date_to <- cstime::dates_by_isoyearweek[isoyearweek>=isoyearweek_from & isoyearweek<=isoyearweek_to]$sun %>% max()
  #date_to <- date_from
  d <- get_raw_data(date_from, date_to)
  setDT(d)

  time_from_date <- data.table(date = unique(d$date))
  time_from_date[, calyear := cstime::date_to_calyear_n(date)]
  time_from_date[, isoyearweek := cstime::date_to_isoyearweek_c(date)]

  time_from_isoyearweek <- unique(time_from_date[,.(isoyearweek)])
  time_from_isoyearweek[, isoyear := cstime::isoyearweek_to_isoyear_n(isoyearweek)]
  time_from_isoyearweek[, isoweek := cstime::isoyearweek_to_isoweek_n(isoyearweek)]
  time_from_isoyearweek[, season := cstime::isoyearweek_to_season_c(isoyearweek)]
  time_from_isoyearweek[, seasonweek := cstime::isoyearweek_to_seasonweek_n(isoyearweek)]
  time_from_isoyearweek[, calyear := NA_integer_]
  time_from_isoyearweek[, calmonth := NA_integer_]
  time_from_isoyearweek[, calyearmonth := NA_character_]
  time_from_isoyearweek[, date := cstime::isoyearweek_to_last_date(isoyearweek)]

  # dont need practice_tag
  # d[, practice_tag := fcase(
  #   Praksis %in% c(
  #     "Fastl\u00F8nnet",
  #     "Fastlege"
  #   ), "v",
  #   Praksis %in% c(
  #     "kommunal legevakt",
  #     "Legevakt"
  #   ), "k",
  #   default = "other"
  # )]
  # d[, Praksis := NULL]
  # d_v <- d[practice_tag=="v"]
  # d_k <- d[practice_tag=="k"]
  # d[, practice_tag := "kv"]
  # d <- rbind(d, d_v, d_k)
  # rm("d_v", "d_k")
  # gc()

  d[, age := fcase(
    PasientAlder == "0-4",   "000-004",

    PasientAlder == "5-9",   "005-014",
    PasientAlder == "0-9",   "005-014", # for kommune with <500 people
    PasientAlder == "10-14", "005-014",

    PasientAlder == "10-19", "015-019", # for kommune with <500 people
    PasientAlder == "15-19", "015-019",

    PasientAlder == "20-29", "020-029",
    PasientAlder == "30-39", "030-039",
    PasientAlder == "40-49", "040-049",
    PasientAlder == "50-59", "050-059",
    PasientAlder == "60-64", "060-064",
    PasientAlder == "65-69", "065-069",
    PasientAlder == "70-79", "070-079",
    PasientAlder == "80+", "080p",
    default = "missing"
  )]
  d[, PasientAlder := NULL]

  # recode into the ages that we will use
  d[, age := fcase(
    age == "000-004",   "000-004",
    age == "005-014",   "005-014",
    age == "015-019",   "015-019",
    age == "020-029",   "020-029",
    age == "030-039",   "030-064",
    age == "040-049",   "030-064",
    age == "050-059",   "030-064",
    age == "060-064",   "030-064",
    age == "065-069",   "065-069",
    age == "070-079",   "070-079",
    age == "080p",   "080p",
    default = "missing"
  )]

  d_age <- copy(d)
  d_age[, age := "total"]
  d <- rbind(d, d_age)
  rm("d_age")
  gc()

  d[, tariffgroup_tag := fcase(
    Takst %in% tariff$raw[tariffgroup_tag=="f"]$tariffraw_tag, "f",
    Takst %in% tariff$raw[tariffgroup_tag=="e"]$tariffraw_tag, "e",
    Takst %in% tariff$raw[tariffgroup_tag=="s"]$tariffraw_tag, "s"
  )]
  d[, Takst := NULL]
  d_f <- d[tariffgroup_tag=="f"]
  d_e <- d[tariffgroup_tag=="e"]
  d_s <- d[tariffgroup_tag=="s"]

  d_fe <- d[tariffgroup_tag %in% c("f", "e")]
  d_fe[, tariffgroup_tag := "fe"]
  # d_fs <- d[tariffgroup_tag %in% c("f", "s")]
  # d_fs[, tariffgroup_tag := "fs"]
  # d_es <- d[tariffgroup_tag %in% c("e", "s")]
  # d_es[, tariffgroup_tag := "es"]

  d[, tariffgroup_tag := "fes"]

  d <- rbind(d, d_f, d_e, d_s, d_fe) #, d_fs, d_es)
  rm("d_f", "d_e", "d_s", "d_fe")
  gc()

  d[, sex := "total"]
  gc()

  for (i in seq_along(icpc2$icpc2raw_tag)) {
    d[, (icpc2$icpc2group_tag[i]) := 0]
    d[Diagnose %in% icpc2$icpc2raw_tag[[i]], (icpc2$icpc2group_tag[i]) := 1]
  }
  gc()

  # Collapsing it down to 1 row per consultation
  d <- d[,
         lapply(.SD, sum),
         by = .(
           Id,
           BehandlerKommune,
           age,
           sex,
           date,
           # practice_tag,
           tariffgroup_tag
         ),
         .SDcols = icpc2$icpc2group_tag
  ]
  d[, consultations_all_n := 1]
  gc()

  # Collapsing it down to 1 row per kommune/age/sex/day/tariff <- BEA
  d <- d[, lapply(.SD, sum), ,
         by = .(
           BehandlerKommune,
           age,
           sex,
           date,
           # practice_tag,
           tariffgroup_tag
         ),
         .SDcols = c(icpc2$icpc2group_tag, "consultations_all_n")
  ]
  d[time_from_date, on = "date", isoyearweek := isoyearweek]
  gc()

  d_nation <- d[, lapply(.SD, sum), ,
         by = .(
           age,
           sex,
           isoyearweek,
           # practice_tag,
           tariffgroup_tag
         ),
         .SDcols = c(icpc2$icpc2group_tag, "consultations_all_n")
  ]

  d[, isoyearweek := NULL]
  d[, location_code_original := paste0("municip_nor",BehandlerKommune)]

  # skeletons ----
  s_nation <- expand.grid(
    age = c("total", "000-004", "005-014", "015-019", "020-029", "030-064", "065-069", "070-079", "080p"),
    sex = c("total"),
    # practice_tag = c("k", "v", "kv"),
    tariffgroup_tag = c("f", "e", "s", "fe", "fes"),
    date = seq.Date(as.Date(date_from), as.Date(date_to), 1)
  ) %>%
    setDT()

  s_nation[time_from_date, on = "date", c("calyear", "isoyearweek") := .(calyear, isoyearweek)]

  s_municip <- merge(
    s_nation,
    csdata::nor_locations_redistricting()[granularity_geo == "municip"],
    by = c("calyear"),
    allow.cartesian = TRUE
  )

  # remove the dates from s_nation ()
  s_nation[, date := NULL]
  s_nation[, calyear := NULL]
  s_nation <- unique(s_nation)

  # introducing data to skeletons ----
  ## municip ----
  s_municip <- merge(
    s_municip,
    d,
    by = c(
      "age",
      "sex",
      # "practice_tag",
      "tariffgroup_tag",
      "date",
      "location_code_original"
    ),
    all.x = T
  )
  rm("d")
  gc()
  for (i in c(icpc2$icpc2group_tag, "consultations_all_n")) s_municip[, (i) := fcase(
    !is.na(get(i)), get(i) * weighting,
    default = 0
    )]
  s_municip <- s_municip[
    ,
    lapply(.SD, function(x) round(sum(x))),
    by = .(
      sex,
      age,
      isoyearweek,
      # practice_tag,
      tariffgroup_tag,
      location_code_current
    ),
    .SDcols = c(icpc2$icpc2group_tag, "consultations_all_n")
  ]
  gc()
  setnames(s_municip, "location_code_current", "location_code")

  # fylke ----
  s_county <- merge(
    s_municip,
    csdata::nor_locations_hierarchy_from_to("municip", "county"),
    by.x = "location_code",
    by.y = "from_code"
  )

  s_county <- s_county[
    ,
    lapply(.SD, sum),
    by = .(
      sex,
      age,
      isoyearweek,
      # practice_tag,
      tariffgroup_tag,
      to_code
    ),
    .SDcols = c(icpc2$icpc2group_tag, "consultations_all_n")
  ]
  setnames(s_county, "to_code", "location_code")

  # nation ----
  s_nation <- merge(
    s_nation,
    d_nation,
    by = c(
      "age",
      "sex",
      # "practice_tag",
      "tariffgroup_tag",
      "isoyearweek"
    ),
    all.x = T
  )
  rm("d_nation")
  gc()
  for (i in c(icpc2$icpc2group_tag, "consultations_all_n")) s_nation[is.na(get(i)), (i) := 0]
  s_nation[, location_code := "nation_nor"]

  # together
  d_agg <- rbindlist(list(s_nation,s_county, s_municip), use.names = TRUE)
  rm("s_nation", "s_county", "s_municip")
  gc()

  d_agg[, consultations_without_influenza_covid19_n := consultations_all_n - r80 - covid19]

  d_agg <- melt.data.table(
    d_agg,
    id.vars = c(
      "location_code",
      "age",
      "sex",
      # "practice_tag",
      "tariffgroup_tag",
      "isoyearweek",
      "consultations_all_n",
      "consultations_without_influenza_covid19_n"
    ),
    variable.name = "icpc2group_tag",
    value.name = "consultations_icpc2group_n",
    variable.factor = FALSE
  )
  gc()

  d_agg[, granularity_time := "isoweek"]
  d_agg[, border := 2020]


  s_locations <- data.table(location_code = unique(d_agg$location_code), country_iso3 = "nor")
  csdata::add_granularity_geo_to_data_set(s_locations)

  d_agg[
    s_locations,
    on="location_code",
    c("granularity_geo", "country_iso3") := .(granularity_geo, country_iso3)
  ]

  d_agg[
    time_from_isoyearweek,
    on="isoyearweek",
    c(
      "isoyear",
      "isoweek",
      "season",
      "seasonweek",
      "calyear",
      "calmonth",
      "calyearmonth",
      "date"
    ) := .(
      isoyear,
      isoweek,
      season,
      seasonweek,
      calyear,
      calmonth,
      calyearmonth,
      date
    )
  ]

  gc()

  # pr100
  d_agg[, consultations_icpc2group_vs_all_pr100 := round(100*consultations_icpc2group_n/consultations_all_n, 3)]
  d_agg[is.nan(consultations_icpc2group_vs_all_pr100), consultations_icpc2group_vs_all_pr100 := 0]
  d_agg[, consultations_icpc2group_vs_without_influenza_covid19_pr100 := round(100*consultations_icpc2group_n/consultations_without_influenza_covid19_n, 3)]
  d_agg[is.nan(consultations_icpc2group_vs_without_influenza_covid19_pr100), consultations_icpc2group_vs_without_influenza_covid19_pr100 := 0]

  setcolorder(
    d_agg,
    c(
      "granularity_time",
      "granularity_geo",
      "country_iso3",
      "location_code",
      "border",
      "age",
      "sex",
      "isoyear",
      "isoweek",
      "isoyearweek",
      "season",
      "seasonweek",
      "calyear",
      "calmonth",
      "calyearmonth",
      "date",
      # "practice_tag",
      "tariffgroup_tag",
      "consultations_icpc2group_n",
      "consultations_icpc2group_vs_all_pr100",
      "consultations_icpc2group_vs_without_influenza_covid19_pr100",
      "consultations_all_n",
      "consultations_without_influenza_covid19_n",
      "icpc2group_tag"
    )
  )
  # cstidy::set_csfmt_rts_data_v1(d_agg)

  data.table::shouldPrint(d_agg)

  return(d_agg)
}

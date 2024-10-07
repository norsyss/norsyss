get_raw_data_nhn <- function(date_from, date_to){
  db <- DBI::dbConnect(
    odbc::odbc(),
    driver="ODBC Driver 17 for SQL Server",
    server="sql-sykdomspulsen",
    database="SykdomspulsenAnalyse",
    trusted_connection="yes"
  )

  # command <- glue::glue(
  #   "select Id,Diagnose,PasientAlder,PasientKjønn as sex,BehandlerKommune,Konsultasjonsdato as date,Takst from Konsultasjon join KonsultasjonDiagnose on Id=KonsultasjonId join KonsultasjonTakst on Id=KonsultasjonTakst.KonsultasjonId where Konsultasjonsdato >='{date_from}' AND Konsultasjonsdato<='{date_to}'"
  # )
  command <- glue::glue(
    "select Id,Diagnose,PasientAlder,PasientKjønn as sex,PasientKommune,PasientBydel,Konsultasjonsdato as date,Takst from Konsultasjon join KonsultasjonDiagnose on Id=KonsultasjonId join KonsultasjonTakst on Id=KonsultasjonTakst.KonsultasjonId where Konsultasjonsdato >='{date_from}' AND Konsultasjonsdato<='{date_to}'"
  )
  d <- DBI::dbGetQuery(db, command)

  attr(d, "date_from") <- date_from
  attr(d, "date_to") <- date_to

  return(d)
}

get_raw_data_fida_pilot <- function(date_from, date_to){
  db <- DBI::dbConnect(
    odbc::odbc(),
    driver=Sys.getenv("CS9_DBCONFIG_DRIVER"),
    server=Sys.getenv("CS9_DBCONFIG_SERVER"),
    port=Sys.getenv("CS9_DBCONFIG_PORT"),
    uid=Sys.getenv("CS9_DBCONFIG_USER"),
    password=Sys.getenv("CS9_DBCONFIG_PASSWORD"),
    database="p2932_db01",
    sslmode="require"
  )

  # command <- glue::glue(
  #   'SELECT "Id", "Diagnose", "PasientAlder", "PasientKjønn" AS sex, "BehandlerKommune", "Konsultasjonsdato" AS date, "Takst"
  #   FROM raw_norsyss."Konsultasjon"
  #   JOIN raw_norsyss."KonsultasjonDiagnose"
  #   ON "Id"="KonsultasjonId"
  #   JOIN raw_norsyss."KonsultasjonTakst"
  #   ON "Id"="KonsultasjonTakst"."KonsultasjonId"
  #   WHERE "Konsultasjonsdato" >=\'{date_from}\' AND "Konsultasjonsdato"<=\'{date_to}\''
  # )
  command <- glue::glue(
    'SELECT "Id", "Diagnose", "PasientAlder", "PasientKjønn" AS sex, "PasientKommune", "Konsultasjonsdato" AS date, "Takst"
    FROM raw_norsyss."Konsultasjon"
    JOIN raw_norsyss."KonsultasjonDiagnose"
    ON "Id"="KonsultasjonId"
    JOIN raw_norsyss."KonsultasjonTakst"
    ON "Id"="KonsultasjonTakst"."KonsultasjonId"
    WHERE "Konsultasjonsdato" >=\'{date_from}\' AND "Konsultasjonsdato"<=\'{date_to}\''
  )
  d <- DBI::dbGetQuery(db, command)

  attr(d, "date_from") <- date_from
  attr(d, "date_to") <- date_to

  return(d)
}

#' @export
get_and_process_raw_data <- function(x_isoyearweek = "2021-02", border = 2024, location="NHN"){
  # x_isoyearweek <- "2024-39"
  date_from <- cstime::dates_by_isoyearweek[isoyearweek==x_isoyearweek]$mon %>% min()
  date_to <- cstime::dates_by_isoyearweek[isoyearweek==x_isoyearweek]$sun %>% max()
  #date_to <- date_from

  if(location=="NHN"){
    d <- get_raw_data_nhn(date_from, date_to)
    use_icpc2 <- icpc2
  } else if(location=="FIDA_PILOT"){
    d <- get_raw_data_fida_pilot(date_from, date_to)
    use_icpc2 <- fida_pilot$icpc2
  } else {
    stop("location must be either NHN or FIDA_PILOT")
  }
  if(nrow(d)==0) return(NULL)
  setDT(d)

  # Fixing municipalities
  d[, PasientKommune := formatC(as.numeric(PasientKommune), width = 4, flag = "0")]
  d[, PasientBydel := formatC(as.numeric(PasientBydel), width = 6, flag = "0")]

  # Oslo
  d[
    PasientKommune %in% c(
      "0301",
      "0312",
      "0313",
      "0314",
      "0315",
      "0316",
      "0318",
      "0319",
      "0321",
      "0325",
      "0326",
      "0327",
      "0328",
      "0330",
      "0331",
      "0333",
      "0334",
      "0335"
    ),
    PasientKommune := "0301"
  ]
  d[
    PasientBydel %in% c(
      "030101",
      "030102",
      "030103",
      "030104",
      "030105",
      "030106",
      "030107",
      "030108",
      "030109",
      "030110",
      "030111",
      "030112",
      "030113",
      "030114",
      "030115",
      "030116",
      "030117"
    ),
    PasientKommune := "0301"
  ]

  # Stavanger
  d[
    PasientKommune %in% c(
      "1103",
      "1161",
      "1162",
      "1163",
      "1164",
      "1165",
      "1166",
      "1167"
    ),
    PasientKommune := "1103"
  ]
  d[
    PasientBydel %in% c(
      "110301",
      "110302",
      "110303",
      "110304",
      "110305",
      "110306",
      "110307",
      "110308",
      "110309"
    ),
    PasientKommune := "1103"
  ]

  # Bergen
  d[
    date < "2020-01-01" &
      PasientKommune %in% c(
        "1201",
        "1202",
        "1203",
        "1204",
        "1205",
        "1206",
        "1207",
        "1208",
        "1209",
        "1210"
      ),
    PasientKommune := "1201"
  ]
  d[
    date >= "2020-01-01" &
      PasientKommune %in% c(
        "1201",
        "1202",
        "1203",
        "1204",
        "1205",
        "1206",
        "1207",
        "1208",
        "1209",
        "1210"
      ),
    PasientKommune := "4601"
  ]
  d[
    date >= "2020-01-01" &
      PasientBydel %in% c(
        "460101",
        "460102",
        "460103",
        "460104",
        "460105",
        "460106",
        "460107"
      ),
    PasientKommune := "4601"
  ]

  # Tromso

  # Fixing covid
  d[Diagnose=="R270000", Diagnose := "R27"]
  d[Diagnose=="R9910000", Diagnose := "R991"]
  d[Diagnose=="R9920000", Diagnose := "R992"]
  d[date < "2020-03-05" & Diagnose == "R991", Diagnose := "XXX"]
  d[date < "2020-04-30" & Diagnose == "R992", Diagnose := "XXX"]

  time_from_date <- data.table(date = unique(d$date))
  time_from_date[, calyear := cstime::date_to_calyear_n(date)]
  time_from_date[, isoyearweek := cstime::date_to_isoyearweek_c(date)]

  # time_from_isoyearweek <- unique(time_from_date[,.(isoyearweek)])
  # time_from_isoyearweek[, isoyear := cstime::isoyearweek_to_isoyear_n(isoyearweek)]
  # time_from_isoyearweek[, isoweek := cstime::isoyearweek_to_isoweek_n(isoyearweek)]
  # time_from_isoyearweek[, season := cstime::isoyearweek_to_season_c(isoyearweek)]
  # time_from_isoyearweek[, seasonweek := cstime::isoyearweek_to_seasonweek_n(isoyearweek)]
  # time_from_isoyearweek[, calyear := NA_integer_]
  # time_from_isoyearweek[, calmonth := NA_integer_]
  # time_from_isoyearweek[, calyearmonth := NA_character_]
  # time_from_isoyearweek[, date := cstime::isoyearweek_to_last_date(isoyearweek)]

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
  # #gc()

  d[, age := fcase(
    PasientAlder == "0-4",   "000_004",

    PasientAlder == "5-9",   "005_014",
    PasientAlder == "0-9",   "005_014", # for kommune with <500 people
    PasientAlder == "10-14", "005_014",

    PasientAlder == "10-19", "015_019", # for kommune with <500 people
    PasientAlder == "15-19", "015_019",

    PasientAlder == "20-29", "020_029",
    PasientAlder == "30-39", "030_039",
    PasientAlder == "40-49", "040_049",
    PasientAlder == "50-59", "050_059",
    PasientAlder == "60-64", "060_064",
    PasientAlder == "65-69", "065_069",
    PasientAlder == "70-79", "070_079",
    PasientAlder == "80+", "080p",
    default = "missing"
  )]
  d[, PasientAlder := NULL]

  # recode into the ages that we will use
  d[, age := fcase(
    age == "000_004",   "000_004",
    age == "005_014",   "005_014",
    age == "015_019",   "015_019",
    age == "020_029",   "020_029",
    age == "030_039",   "030_064",
    age == "040_049",   "030_064",
    age == "050_059",   "030_064",
    age == "060_064",   "030_064",
    age == "065_069",   "065_069",
    age == "070_079",   "070_079",
    age == "080p",   "080p",
    default = "missing"
  )]

  d_age <- copy(d)
  d_age[, age := "total"]
  d <- rbindlist(list(d, d_age))
  rm("d_age")

  d[, sex := fcase(
    sex == "Mann", "male",
    sex == "Kvinne", "female",
    default = "missing"
  )]
  d_sex <- copy(d)
  d_sex[, sex := "total"]
  d <- rbindlist(list(d[sex %in% c("male", "female")], d_sex))
  rm("d_sex")
  #gc()

  d[, tariffgroup_tag := fcase(
    Takst %in% tariff$raw[tariffgroup_tag=="f"]$tariffraw_tag, "f",
    Takst %in% tariff$raw[tariffgroup_tag=="e"]$tariffraw_tag, "e",
    Takst %in% tariff$raw[tariffgroup_tag=="s"]$tariffraw_tag, "s",
    default = "XXX"
  )]
  d[, Takst := NULL]
  d <- d[tariffgroup_tag!="XXX"]

  # d_fes <- copy(d)
  # d_fes[, tariffgroup_tag := "fes"]

  d <- d[tariffgroup_tag %in% c("f", "e")]
  d[, tariffgroup_tag := "fe"]

  #gc()

  setkey(d, Diagnose)
  for (i in seq_along(use_icpc2$icpc2raw_tag)) {
    d[, (use_icpc2$icpc2group_tag[i]) := as.integer(Diagnose %in% use_icpc2$icpc2raw_tag[[i]])]
    # d[, (icpc2$icpc2group_tag[i]) := 0]
    # d[Diagnose %in% icpc2$icpc2raw_tag[[i]], (icpc2$icpc2group_tag[i]) := 1]
  }
  #gc()

  # consultations_all_n will be = 1 for the first observation per Id
  d[, consultations_all_n := as.integer(1:.N==1), by=.(
    Id,
    PasientKommune,
    age,
    sex,
    date,
    tariffgroup_tag
  )]
  # # Collapsing it down to 1 row per consultation
  # d <- d[,
  #        lapply(.SD, sum),
  #        keyby = .(
  #          Id,
  #          PasientKommune,
  #          age,
  #          date,
  #          # practice_tag,
  #          tariffgroup_tag
  #        ),
  #        .SDcols = icpc2$icpc2group_tag
  # ]
  # d[, consultations_all_n := 1]
  # #gc()

  # Collapsing it down to 1 row per kommune/age/sex/day/tariff <- BEA
  d <- d[, lapply(.SD, sum), ,
         by = .(
           PasientKommune,
           age,
           sex,
           date,
           tariffgroup_tag
         ),
         .SDcols = c(use_icpc2$icpc2group_tag, "consultations_all_n")
  ]
  # d[time_from_date, on = "date", isoyearweek := isoyearweek]
  #gc()

  d_nation <- d[, lapply(.SD, sum), ,
         by = .(
           age,
           sex,
           tariffgroup_tag
         ),
         .SDcols = c(use_icpc2$icpc2group_tag, "consultations_all_n")
  ]

  d[, location_code_original := paste0("municip_nor",PasientKommune)]

  # skeletons ----
  s_nation <- expand.grid(
    age = c("total", "000_004", "005_014", "015_019", "020_029", "030_064", "065_069", "070_079", "080p"),
    sex = c("total", "male", "female"),
    tariffgroup_tag = c("fe"),
    date = seq.Date(as.Date(date_from), as.Date(date_to), 1)
  ) %>%
    setDT()

  s_nation[time_from_date, on = "date", c("calyear") := .(calyear)]

  s_municip <- merge(
    s_nation,
    csdata::nor_locations_redistricting(border = border)[granularity_geo == "municip"],
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
      "tariffgroup_tag",
      "date",
      "location_code_original"
    ),
    all.x = T
  )
  rm("d")
  #gc()

  for (i in c(use_icpc2$icpc2group_tag, "consultations_all_n")) s_municip[, (i) := fcase(
    !is.na(get(i)), get(i) * weighting,
    default = 0
  )]
  s_municip <- s_municip[
    ,
    lapply(.SD, function(x) round(sum(x))),
    keyby = .(
      age,
      sex,
      tariffgroup_tag,
      location_code_current
    ),
    .SDcols = c(use_icpc2$icpc2group_tag, "consultations_all_n")
  ]
  #gc()
  setnames(s_municip, "location_code_current", "location_code")
  s_municip[, granularity_geo := "municip"]

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
      age,
      sex,
      tariffgroup_tag,
      to_code
    ),
    .SDcols = c(use_icpc2$icpc2group_tag, "consultations_all_n")
  ]
  setnames(s_county, "to_code", "location_code")
  s_county[, granularity_geo := "county"]

  # georegion ----
  s_georegion <- merge(
    s_county,
    csdata::nor_locations_hierarchy_from_to("county", "georegion"),
    by.x = "location_code",
    by.y = "from_code"
  )

  s_georegion <- s_georegion[
    ,
    lapply(.SD, sum),
    by = .(
      age,
      sex,
      tariffgroup_tag,
      to_code
    ),
    .SDcols = c(use_icpc2$icpc2group_tag, "consultations_all_n")
  ]
  setnames(s_georegion, "to_code", "location_code")
  s_georegion[, granularity_geo := "georegion"]

  # nation ----
  s_nation <- merge(
    s_nation,
    d_nation,
    by = c(
      "age",
      "sex",
      "tariffgroup_tag"
    ),
    all.x = T
  )
  rm("d_nation")
  #gc()
  for (i in c(use_icpc2$icpc2group_tag, "consultations_all_n")) s_nation[is.na(get(i)), (i) := 0]
  s_nation[, location_code := "nation_nor"]
  s_nation[, granularity_geo := "nation"]

  # together
  d_agg <- rbindlist(list(s_nation, s_georegion, s_county, s_municip), use.names = TRUE)
  rm("s_nation", "s_georegion", "s_county", "s_municip")
  #gc()

  d_agg[, consultations_without_influenza_covid19_n := consultations_all_n - r80 - covid19]

  d_agg <- melt.data.table(
    d_agg,
    id.vars = c(
      "granularity_geo",
      "location_code",
      "age",
      "sex",
      "tariffgroup_tag",
      "consultations_all_n",
      "consultations_without_influenza_covid19_n"
    ),
    variable.name = "icpc2group_tag",
    value.name = "consultations_icpc2group_n",
    variable.factor = FALSE
  )
  #gc()

  d_agg[, isoyearweek := x_isoyearweek]
  d_agg[, granularity_time := "isoyearweek"]
  d_agg[, border := border]
  d_agg[, country_iso3 := "nor"]

  # s_locations <- data.table(location_code = unique(d_agg$location_code), country_iso3 = "nor")
  # csdata::add_granularity_geo_to_data_set(s_locations)
  #
  # d_agg[
  #   s_locations,
  #   on="location_code",
  #   c("granularity_geo", "country_iso3") := .(granularity_geo, country_iso3)
  # ]

  # d_agg[
  #   time_from_isoyearweek,
  #   on="isoyearweek",
  #   c(
  #     "isoyear",
  #     "isoweek",
  #     "season",
  #     "seasonweek",
  #     "calyear",
  #     "calmonth",
  #     "calyearmonth",
  #     "date"
  #   ) := .(
  #     isoyear,
  #     isoweek,
  #     season,
  #     seasonweek,
  #     calyear,
  #     calmonth,
  #     calyearmonth,
  #     date
  #   )
  # ]
  cstidy::set_csfmt_rts_data_v2(d_agg)

  #gc()

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
      "isoquarter",
      "isoyearquarter",
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

  if(location=="FIDA_PILOT"){
    setnames(
      d_agg,
      c(
        "consultations_icpc2group_n",
        "consultations_icpc2group_vs_all_pr100",
        "consultations_icpc2group_vs_without_influenza_covid19_pr100",
        "consultations_all_n",
        "consultations_without_influenza_covid19_n"
      ),
      c(
        "consults_icpc2group_n",
        "consults_icpc2group_vs_all_pr100",
        "consults_icpc2group_vs_without_influenza_covid19_pr100",
        "consults_all_n",
        "consults_without_influenza_covid19_n"
      )
    )
  }

  data.table::shouldPrint(d_agg)

  return(d_agg)
}

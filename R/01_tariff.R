# https://lovdata.no/dokument/SF/forskrift/2022-06-29-1269
# https://www.helsedirektoratet.no/statistikk/om-data-statistikk-om-fastlegetjenesten
#' @export
tariff <- list()
tariff$raw <- rbindlist(list(
  data.table("11ad", "f", "Home visit by a general practitioner (day).", "Sykebesøk ved allmennpraktiserende lege (dag)."),
  data.table("11ak", "f", "Home visit by a general practitioner (night).", "Sykebesøk ved allmennpraktiserende lege (kveld)."),

  data.table("2ad", "f", "Consultation with a general practitioner (day).", "Konsultasjon hos allmennpraktiserende lege (dag)."),
  data.table("2ak", "f", "Consultation with a general practitioner (night).", "Konsultasjon hos allmennpraktiserende lege (kveld)."),

  data.table("2fk", "f", "Consultation and supplement for being called to the office for immediate help during the emergency room when an ambulance has been used, for the first patient. Travel allowances and transport allowances can also be calculated. The tariff cannot be used for on-call duty.", "Konsultasjon og tillegg for utrykning til kontor ved øyeblikkelig hjelp under legevakt når skyssmiddel er benyttet, for første pasient. Det kan i tillegg beregnes reisetillegg og skyssgodtgjørelse. Taksten kan ikke benyttes ved tilstedevakt."),

  data.table("2ae", "e", "E-consultation with a GP and at the emergency room (day).", "E-konsultasjon hos fastlege og på legevakt (dag)."),
  data.table("2aek", "e", "E-consultation with a GP and at the emergency room (night).", "E-konsultasjon hos fastlege og på legevakt (kveld)."),

  data.table("1ad", "s", "Simple patient contact by personal attendance or by delivery. The rate assumes that advice/guidance is given. Also applies when the contact/request results in the writing of a prescription, sick leave, requisition or referral (day).", "Enkel pasientkontakt ved personlig frammøte eller ved bud. Taksten forutsetter at det gis råd/veiledning. Gjelder også når kontakten/forespørselen resulterer i skriving av resept, sykmelding, rekvisisjon eller henvisning (dag)."),
  data.table("1ak", "s", "Simple patient contact by personal attendance or by delivery. The rate assumes that advice/guidance is given. Also applies when the contact/request results in the writing of a prescription, sick leave, requisition or referral (night).", "Enkel pasientkontakt ved personlig frammøte eller ved bud. Taksten forutsetter at det gis råd/veiledning. Gjelder også når kontakten/forespørselen resulterer i skriving av resept, sykmelding, rekvisisjon eller henvisning (kveld)."),

  data.table("1bd", "s", "Simple patient contact in writing, by telephone or by electronic communication. The rate assumes that advice/guidance is given. Also applies when the contact/request results in the writing of a prescription sick note, requisition or referral (day).", "Enkel pasientkontakt skriftlig, per telefon eller ved elektronisk kommunikasjon. Taksten forutsetter at det gis råd/veiledning. Gjelder også når kontakten/forespørselen resulterer i skriving av resept sykmelding, rekvisisjon eller henvisning (dag)."),
  data.table("1bk", "s", "Simple patient contact in writing, by telephone or by electronic communication. The rate assumes that advice/guidance is given. Also applies when the contact/request results in the writing of a prescription sick note, requisition or referral (night).", "Enkel pasientkontakt skriftlig, per telefon eller ved elektronisk kommunikasjon. Taksten forutsetter at det gis råd/veiledning. Gjelder også når kontakten/forespørselen resulterer i skriving av resept sykmelding, rekvisisjon eller henvisning (kveld)."),

  data.table("1be", "s", "Simple patient contact through electronic communication. The rate has expired on 1 July 2022, and included in rate 1bd.", "Enkel pasientkontakt ved elektronisk kommunikasjon. Taksten er utgått 1. juli 2022, og inkludert i takst 1bd."),

  data.table("1g", "s", "Easy patient contact, enquiries, counseling by telephone between 23 and 08. The rate can only be used during emergency care in an on-call district without an on-call presence.", "Enkel pasientkontakt, forespørsel, rådgivning per telefon mellom kl. 23 og 08. Taksten kan bare benyttes under legevakt i vaktdistrikt uten tilstedevakt."),
  data.table("1h", "s", "Sick leave and referral without patient present.", "Sykemelding og henvisning uten pasient tilstede."),
  data.table("1i", "s", "??????", "?????")
))
setnames(tariff$raw, c("tariffraw_tag", "tariffgroup_tag", "tariffraw_description_en", "tariffraw_description_nb"))
tariff$group <- rbindlist(list(
  data.table("f", "Consultation/visit (face-to-face)", "Konsultasjon/sykebesøk (tilstede)"),
  data.table("e", "E-Consultation", "E-konsultasjon"),
  data.table("s", "Simple contact", "Enkelt kontakt")
))
setnames(tariff$group, c("tariffgroup_tag", "tariffgroup_description_en", "tariffgroup_description_nb"))

# NAV Kommune nummer til FREG
# Some municip numbers received by KUHR do not match the expected
# numbers from folkeregistret. This table translates between them

# Any other municip numbers not in config for sykdomspulsen will be set to 9999
# Bydels number also exist for these codes (see docoumentation)
nav_to_freg <- list(
  "312" = 301,
  "313" = 301,
  "314" = 301,
  "315" = 301,
  "316" = 301,
  "318" = 301,
  "319" = 301,
  "321" = 301,
  "326" = 301,
  "327" = 301,
  "328" = 301,
  "330" = 301,
  "331" = 301,
  "334" = 301,
  "335" = 301,
  "1161" = 1103,
  "1162" = 1103,
  "1164" = 1103,
  "1165" = 1103,
  "1202" = 1201,
  "1203" = 1201,
  "1204" = 1201,
  "1205" = 1201,
  "1206" = 1201,
  "1208" = 1201,
  "1209" = 1201,
  "1210" = 1201,
  "1603" = 301,
  "1604" = 1601,
  "1605" = 1601,
  "1607" = 1601
)

nav_to_freg_bydel <- c(
  "312" = 30105,
  "313" = 30104,
  "314" = 30103,
  "315" = 30102,
  "316" = 30101,
  "318" = 30114,
  "319" = 30115,
  "321" = 30113,
  "326" = 30112,
  "327" = 30111,
  "328" = 30110,
  "330" = 30109,
  "331" = 30108,
  "334" = 30107,
  "335" = 30106,
  "1161" = 110303,
  "1162" = 110301,
  "1164" = 110306,
  "1165" = 110304,
  "1202" = 120103,
  "1203" = 120108,
  "1204" = 120101,
  "1205" = 120104,
  "1206" = 120105,
  "1208" = 120107,
  "1209" = 120102,
  "1210" = 120106
)
nav_to_freg_bydel <- data.table(
  nav = as.numeric(names(nav_to_freg_bydel)),
  freg = nav_to_freg_bydel
)

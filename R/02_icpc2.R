#' @export
icpc2 <- rbindlist(list(
  # Single
  data.table("A02", "a02", "Chills", "Symptoms/Complaints", "General and unspecified", "Frysninger", "Symptomer", "Allment og uspesifisert", "Frysningar", "Symptom", "Allment og uspesifisert"),
  data.table("A03", "a03", "Fever", "Symptoms/Complaints", "General and unspecified", "Feber", "Symptomer", "Allment og uspesifisert", "Feber", "Symptom", "Allment og uspesifisert"),
  data.table("A04", "a04", "Weakness/tiredness general", "Symptoms/Complaints", "General and unspecified", "Slapphet/tretthet", "Symptomer", "Allment og uspesifisert", "Slappheit/trøyttleik", "Symptom", "Allment og uspesifisert"),
  data.table("A05", "a05", "Feeling ill", "Symptoms/Complaints", "General and unspecified", "Sykdomsfølelse", "Symptomer", "Allment og uspesifisert", "Sykdomsfølelse", "Symptom", "Allment og uspesifisert"),
  data.table("A71", "a71", "Measles", "Infections", "General and unspecified", "Meslinger", "Infeksjoner", "Allment og uspesifisert", "Meslingar", "Infeksjonar", "Allment og uspesifisert"),
  data.table("A72", "a72", "Chickenpox", "Infections", "General and unspecified", "Vannkopper", "Infeksjoner", "Allment og uspesifisert", "Vannkoppar", "Infeksjonar", "Allment og uspesifisert"),
  data.table("A73", "a73", "Malaria", "Infections", "General and unspecified", "Malaria", "Infeksjoner", "Allment og uspesifisert", "Malaria", "Infeksjonar", "Allment og uspesifisert"),
  data.table("A74", "a74", "Rubella", "Infections", "General and unspecified", "Røde hunder", "Infeksjoner", "Allment og uspesifisert", "Røde hundar", "Infeksjonar", "Allment og uspesifisert"),
  data.table("A75", "a75", "Infectious mononucleosis", "Infections", "General and unspecified", "Mononukleose", "Infeksjoner", "Allment og uspesifisert", "Mononukleose", "Infeksjonar", "Allment og uspesifisert"),
  data.table("A76", "a76", "Viral exanthem other", "Infections", "General and unspecified", "Virussykdom med utslett IKA", "Infeksjoner", "Allment og uspesifisert", "Virussjukdom med utslett IKA", "Infeksjonar", "Allment og uspesifisert"),
  data.table("A77", "a77", "Viral disease other/NOS", "Infections", "General and unspecified", "Virussykdom IKA", "Infeksjoner", "Allment og uspesifisert", "Virussjukdom IKA", "Infeksjonar", "Allment og uspesifisert"),
  data.table("A78", "a78", "Infectious disease other/NOS", "Infections", "General and unspecified", "Infeksjonssykdom IKA", "Infeksjoner", "Allment og uspesifisert", "Infeksjonssjukdom IKA", "Infeksjonar", "Allment og uspesifisert"),
  data.table("B02", "b02", "Lymph gland(s) enlarged/painful", "Symptoms/Complaints", "Blood, blood forming organs, and immune mechanism", "Lymfeknuter forstørrede/smertefulle", "Symptomer", "Blod, bloddannende organer og immunsystemet", "Lymfeknuter forstørra/smertefulle", "Symptom", "Blod, bloddannande organ og immunsystemet"),
  data.table("B70", "b70", "Lymphadenitis acute", "Infections", "Blood, blood forming organs, and immune mechanism", "Lymfadenitt akutt", "Infeksjoner", "Blod, bloddannende organer og immunsystemet", "Lymfadenitt akutt", "Infeksjonar", "Blod, bloddannande organ og immunsystemet"),
  data.table("B71", "b71", "lymphadenitis non-specific", "Infections", "Blood, blood forming organs, and immune mechanism", "Lymfadenitt kronisk/uspesifikk", "Infeksjoner", "Blod, bloddannende organer og immunsystemet", "Lymfadenitt kronisk/uspesifikk", "Infeksjonar", "Blod, bloddannande organ og immunsystemet"),
  data.table(list(c("D11", "D70", "D73")), "gastroenteritis", "Gastroenteritis", "Combination", "Digestive", "Mage-tarminfeksjoner", "Kombinasjon", "Fordøyelsessystemet", "Mage-tarminfeksjonar", "Kombinasjon", "Fordøyingssystemet"),
  data.table("D01", "d01", "Abdominal pain/cramps general", "Symptoms/Complaints", "Digestive", "Abdominalsmerte/krampe generell", "Symptomer", "Fordøyelsessystemet", "Abdominalsmerte/krampe generell", "Symptom", "Fordøyingssystemet"),
  data.table("D02", "d02", "Abdominal pain epigastric", "Symptoms/Complaints", "Digestive", "Abdominalsmerte epigastriet", "Symptomer", "Fordøyelsessystemet", "Abdominalsmerte epigastriet", "Symptom", "Fordøyingssystemet"),
  data.table("D06", "d06", "Abdominal pain localized other", "Symptoms/Complaints", "Digestive", "Abdonimalsmerte lokalisert IKA", "Symptomer", "Fordøyelsessystemet", "Abdonimalsmerte lokalisert IKA", "Symptom", "Fordøyingssystemet"),
  data.table("D08", "d08", "Flatulence/gas/belching", "Symptoms/Complaints", "Digestive", "Flatulens/oppblåsthet/raping", "Symptomer", "Fordøyelsessystemet", "Flatulens/oppblåstheit/raping", "Symptom", "Fordøyingssystemet"),
  data.table("D09", "d09", "Nausea", "Symptoms/Complaints", "Digestive", "Kvalme", "Symptomer", "Fordøyelsessystemet", "Kvalme", "Symptom", "Fordøyingssystemet"),
  data.table("D10", "d10", "Vomiting", "Symptoms/Complaints", "Digestive", "Oppkast/brekninger", "Symptomer", "Fordøyelsessystemet", "Oppkast/brekkingar", "Symptom", "Fordøyingssystemet"),
  data.table("D11", "d11", "Diarrhea", "Symptoms/Complaints", "Digestive", "Diaré", "Symptomer", "Fordøyelsessystemet", "Diaré", "Symptom", "Fordøyingssystemet"),
  data.table("D14", "d14", "Hematemesis/vomiting blood", "Symptoms/Complaints", "Digestive", "Oppkast med blod/hematemese", "Symptomer", "Fordøyelsessystemet", "Oppkast med blod/hematemese", "Symptom", "Fordøyingssystemet"),
  data.table("D18", "d18", "Change feces/bowel movements", "Symptoms/Complaints", "Digestive", "Endring i avføring/avføringsvane", "Symptomer", "Fordøyelsessystemet", "Endring i avføring/avføringsvane", "Symptom", "Fordøyingssystemet"),
  data.table("D25", "d25", "Abdominal distension", "Symptoms/Complaints", "Digestive", "Utspilt abdomen", "Symptomer", "Fordøyelsessystemet", "Utspilt abdomen", "Symptom", "Fordøyingssystemet"),
  data.table("D29", "d29", "Digestive symptom/complaint other", "Symptoms/Complaints", "Digestive", "Fordøyelsesyst symptomer/plager IKA", "Symptomer", "Fordøyelsessystemet", "*Fordøyelsesyst symptom/plagar IKA", "Symptom", "Fordøyingssystemet"),
  data.table("D70", "d70", "Gastrointestinal infection", "Infections", "Digestive", "Tarminfeksjon", "Infeksjoner", "Fordøyelsessystemet", "Tarminfeksjon", "Infeksjonar", "Fordøyingssystemet"),
  data.table("D73", "d73", "Gastroenteritis presumed infection", "Infections", "Digestive", "Gastroenteritt antatt infeksiøs", "Infeksjoner", "Fordøyelsessystemet", "Gastroenteritt anteke infeksiøs", "Infeksjonar", "Fordøyingssystemet"),
  data.table("D87", "d87", "Stomach function disorder", "Other diagnoses", "Digestive", "Funksjonelle plager magesekk", "Annet", "Fordøyelsessystemet", "Funksjonelle plagar magesekk", "Anna", "Fordøyingssystemet"),
  data.table("D99", "d99", "Disease digestive system, other", "Other diagnoses", "Digestive", "Sykdom i fordøyelsessystemet IKA", "Annet", "Fordøyelsessystemet", "Sjukdom i fordøyingssystemet IKA", "Anna", "Fordøyingssystemet"),
  data.table("F70", "f70", "Conjunctivitis infectious", "Infections", "Eye", "Infeksiøs konjunktivitt", "Infeksjoner", "Øye", "Infeksiøs konjunktivitt", "Infeksjonar", "Auge"),
  data.table("F73", "f73", "Eye infection/inflammation other", "Infections", "Eye", "Øye infeksjon/betennelse IKA", "Infeksjoner", "Øye", "Auge infeksjon/betennelse IKA", "Infeksjonar", "Auge"),
  data.table("H01", "h01", "Ear pain/earache", "Symptoms/Complaints", "Ear", "Øresmerte/øreverk", "Symptomer", "Øre", "Øresmerte/øreverk", "Symptom", "Øre"),
  data.table("H04", "h04", "Ear discharge", "Symptoms/Complaints", "Ear", "Øreflod/sekresjon", "Symptomer", "Øre", "Øreflod/sekresjon", "Symptom", "Øre"),
  data.table("H13", "h13", "Plugged feeling ear", "Symptoms/Complaints", "Ear", "Øreplugg/øretetthet", "Symptomer", "Øre", "Øreplugg/øretettleik", "Symptom", "Øre"),
  data.table("H29", "h29", "Ear symptom/complaint other", "Symptoms/Complaints", "Ear", "Øre symptomer/plager IKA", "Symptomer", "Øre", "Øre symptom/plagar IKA", "Symptom", "Øre"),
  data.table("H71", "h71", "Acute otitis media/myringitis", "Infections", "Ear", "Mellomørebetennelse akutt", "Symptomer", "Øre", "Mellomørebetennelse akutt", "Symptom", "Øre"),
  data.table("H77", "h77", "Perforation ear drum", "Injuries", "Ear", "Perforert trommehinne IKA", "Skader", "Øre", "Perforert trommehinne IKA", "Skadar", "Øre"),
  data.table(list(c(
    "R01",
    "R02",
    "R03",
    "R04",
    "R05",
    # "R06",
    "R07",
    "R08",
    "R09",
    "R21",
    "R24",
    "R25",
    "R27",
    "R29",
    "R33",
    "R71",
    "R72",
    "R74",
    "R75",
    "R76",
    "R77",
    "R78",
    "R79",
    "R80",
    "R81",
    "R82",
    "R83",
    # "R95",
    # "R96",
    "R99",
    "R991",
    "R992"
  )), "respiratory_infections", "Respiratory infections", "Combination", "Respiratory", "Luftveisinfeksjoner", "Kombinasjon", "Luftveier", "Luftvegsinfeksjonar", "Kombinasjon", "Luftvegar"),
  data.table(list(c("R991", "R992")), "covid19", "COVID-19", "Combination", "Respiratory", "Covid-19", "Kombinasjon", "Luftveier", "Covid-19", "Kombinasjon", "Luftvegar"),
  data.table("R01", "r01", "Pain respiratory system", "Symptoms/Complaints", "Respiratory", "Smerte luftveier", "Symptomer", "Luftveier", "Smerte luftvegar", "Symptom", "Luftvegar"),
  data.table("R02", "r02", "Shortness of breath/dyspnea", "Symptoms/Complaints", "Respiratory", "Kortpustethet/dyspné", "Symptomer", "Luftveier", "Kortpustheit/dyspné", "Symptom", "Luftvegar"),
  data.table("R03", "r03", "Wheezing", "Symptoms/Complaints", "Respiratory", "Piping i brystet", "Symptomer", "Luftveier", "Piping i brystet", "Symptom", "Luftvegar"),
  data.table("R04", "r04", "Breathing problem, other", "Symptoms/Complaints", "Respiratory", "Pusteproblem IKA", "Symptomer", "Luftveier", "Pusteproblem IKA", "Symptom", "Luftvegar"),
  data.table("R05", "r05", "Cough", "Symptoms/Complaints", "Respiratory", "Hoste", "Symptomer", "Luftveier", "Hoste", "Symptom", "Luftvegar"),
  data.table("R06", "r06", "Nose bleed/epistaxis", "Symptoms/Complaints", "Respiratory", "Neseblødning", "Symptomer", "Luftveier", "Nasebløding", "Symptom", "Luftvegar"),
  data.table("R07", "r07", "Sneezing/nasal congestion", "Symptoms/Complaints", "Respiratory", "Nysing/tetthet i nese", "Symptomer", "Luftveier", "Nysing/tettleik i nase", "Symptom", "Luftvegar"),
  data.table("R08", "r08", "Nose symptom/complaint other", "Symptoms/Complaints", "Respiratory", "Nese symptomer/plager IKA", "Symptomer", "Luftveier", "Nese symptom/plagar IKA", "Symptom", "Luftvegar"),
  data.table("R09", "r09", "Sinus symptom/complaint", "Symptoms/Complaints", "Respiratory", "Bihule symptomer/plager IKA", "Symptomer", "Luftveier", "Bihule symptom/plagar IKA", "Symptom", "Luftvegar"),
  data.table("R21", "r21", "Throat symptom/complaint", "Symptoms/Complaints", "Respiratory", "Hals symptomer/plager", "Symptomer", "Luftveier", "Hals symptom/plagar", "Symptom", "Luftvegar"),
  data.table("R24", "r24", "Hemoptysis", "Symptoms/Complaints", "Respiratory", "Blodig oppspytt/hemoptyse", "Symptomer", "Luftveier", "Blodig oppspytt/hemoptyse", "Symptom", "Luftvegar"),
  data.table("R25", "r25", "Sputum/phlegm abnormal", "Symptoms/Complaints", "Respiratory", "Unormalt oppspytt/ekspektorat", "Symptomer", "Luftveier", "Unormalt oppspytt/ekspektorat", "Symptom", "Luftvegar"),
  data.table("R27", "r27", "Fear of respiratory disease, other", "Symptoms/Complaints", "Respiratory", "Engstelig for sykdom luftveier IKA", "Symptomer", "Luftveier", "Engsteleg for sjukdom luftvegar IKA", "Symptom", "Luftvegar"),
  data.table("R29", "r29", "Respiratory symptom/complaint other", "Symptoms/Complaints", "Respiratory", "Luftveier symptomer/plager IKA", "Symptomer", "Luftveier", "Luftvegar symptom/plagar IKA", "Symptom", "Luftvegar"),
  data.table("R33", "r33", "Respiratory microbiological/immunological test", "Process codes", "Respiratory", "Luftveier mikrobiologisk/immunologisk prøve", "Prosesskoder", "Luftveier", "Luftvegar mikrobiologisk/immunologisk prøve", "Prosesskodar", "Luftvegar"),
  data.table("R71", "r71", "Whooping cough", "Infections", "Respiratory", "Kikhoste", "Infeksjoner", "Luftveier", "Kikhoste", "Infeksjonar", "Luftvegar"),
  data.table("R72", "r72", "Strep throat", "Infections", "Respiratory", "Streptokokkhals", "Infeksjoner", "Luftveier", "Streptokokkhals", "Infeksjonar", "Luftvegar"),
  data.table("R74", "r74", "Upper respiratory infection acute", "Infections", "Respiratory", "Akutt øvre luftveisinfeksjon", "Infeksjoner", "Luftveier", "Akutt øvre luftvegsinfeksjon", "Infeksjonar", "Luftvegar"),
  data.table("R75", "r75", "Sinusitis acute/chronic", "Infections", "Respiratory", "Bihulebetennelse", "Infeksjoner", "Luftveier", "Bihulebetennelse", "Infeksjonar", "Luftvegar"),
  data.table("R76", "r76", "Tonsillitis acute", "Infections", "Respiratory", "Akutt tonsillitt", "Infeksjoner", "Luftveier", "Akutt tonsillitt", "Infeksjonar", "Luftvegar"),
  data.table("R77", "r77", "Laryngitis/tracheitis acute", "Infections", "Respiratory", "Akutt laryngitt/trakeitt", "Infeksjoner", "Luftveier", "Akutt laryngitt/trakeitt", "Infeksjonar", "Luftvegar"),
  data.table("R78", "r78", "Acute bronchitis/bronchiolitis", "Infections", "Respiratory", "Akutt bronkitt/bronkiolitt", "Infeksjoner", "Luftveier", "Akutt bronkitt/bronkiolitt", "Infeksjonar", "Luftvegar"),
  data.table("R79", "r79", "Chronic bronchitis", "Infections", "Respiratory", "Kronisk bronkitt", "Infeksjoner", "Luftveier", "Kronisk bronkitt", "Infeksjonar", "Luftvegar"),
  data.table("R80", "r80", "Influenza", "Infections", "Respiratory", "Influensa", "Infeksjoner", "Luftveier", "Influensa", "Infeksjonar", "Luftvegar"),
  data.table("R81", "r81", "Pneumonia", "Infections", "Respiratory", "Lungebetennelse", "Infeksjoner", "Luftveier", "Lungebetennelse", "Infeksjonar", "Luftvegar"),
  data.table("R82", "r82", "Pleurisy/pleural effusion", "Infections", "Respiratory", "Pleuritt IKA", "Infeksjoner", "Luftveier", "Pleuritt IKA", "Infeksjonar", "Luftvegar"),
  data.table("R83", "r83", "Respiratory infection other", "Infections", "Respiratory", "Luftveisinfeksjon IKA", "Infeksjoner", "Luftveier", "Luftvegsinfeksjon IKA", "Infeksjonar", "Luftvegar"),
  data.table("R95", "r95", "Chronic obstructive pulmonary disorder", "Other diagnoses", "Respiratory", "Kronisk obstruktiv lungesykdom", "Annet", "Luftveier", "Kronisk obstruktiv lungesjukdom", "Anna", "Luftvegar"),
  data.table("R96", "r96", "Asthma", "Other diagnoses", "Respiratory", "Astma", "Annet", "Luftveier", "Astma", "Anna", "Luftvegar"),
  data.table("R99", "r99", "Respiratory disease other", "Other diagnoses", "Respiratory", "Luftveissykdom IKA", "Annet", "Luftveier", "Luftvegssjukdom IKA", "Anna", "Luftvegar"),
  data.table("R991", "r991", "COVID-19 (Probable/Suspected)", "Infections", "Respiratory", "Covid-19 (sannsynlig/mistenkt)", "Infeksjoner", "Luftveier", "Covid-19 (sannsynleg/mistenkt)", "Infeksjonar", "Luftvegar"),
  data.table("R992", "r992", "COVID-19 (Confirmed)", "Infections", "Respiratory", "Covid-19 (bekreftet)", "Infeksjoner", "Luftveier", "Covid-19 (stadfesta)", "Infeksjonar", "Luftvegar"),
  data.table("S06", "s06", "Rash localized", "Symptoms/Complaints", "Skin", "Lokalisert utslett hud", "Symptomer", "Hud", "Lokalisert utslett hud", "Symptom", "Hud"),
  data.table("S07", "s07", "Rash generalized", "Symptoms/Complaints", "Skin", "Utbredte utslett hud", "Symptomer", "Hud", "Utbreidde utslett hud", "Symptom", "Hud"),
  data.table("S10", "s10", "Boil/carbuncle", "Infections", "Skin", "Abscess/furunkel", "Infeksjoner", "Hud", "Abscess/furunkel", "Infeksjonar", "Hud"),
  data.table("S12", "s12", "Insect bite/sting", "Injuries", "Skin", "Insektstikk/bitt", "Skader", "Hud", "Insektstikk/bitt", "Skadar", "Hud"),
  data.table("S17", "s17", "Abrasion/scratch/blister", "Injuries", "Skin", "Skrubbsår/blemmer", "Skader", "Hud", "Skrubbsår/blemmer", "Skadar", "Hud"),
  data.table("S29", "s29", "Skin symptom/complaint other", "Symptoms/Complaints", "Skin", "Hud symptomer/plager IKA", "Symptomer", "Hud", "Hud symptom/plagar IKA", "Symptom", "Hud"),
  data.table("S70", "s70", "Herpes zoster", "Infections", "Skin", "Herpes zoster", "Infeksjoner", "Hud", "Herpes zoster", "Infeksjonar", "Hud"),
  data.table("S71", "s71", "Herpes simplex", "Infections", "Skin", "Herpes simplex IKA", "Infeksjoner", "Hud", "Herpes simplex IKA", "Infeksjonar", "Hud"),
  data.table("S72", "s72", "Scabies/other acariasis", "Infections", "Skin", "Skabb/infestasjon midd", "Infeksjoner", "Hud", "Skabb/infestasjon midd", "Infeksjonar", "Hud"),
  data.table("S73", "s73", "Pediculosis/skin infestation other", "Infections", "Skin", "Infestasjon parasitt IKA", "Infeksjoner", "Hud", "Infestasjon parasitt IKA", "Infeksjonar", "Hud"),
  data.table("S74", "s74", "Dermatophytosis", "Infections", "Skin", "Ringorm/hudsopp IKA", "Infeksjoner", "Hud", "Ringorm/hudsopp IKA", "Infeksjonar", "Hud"),
  data.table("S75", "s75", "Moniliasis/candidiasis skin", "Infections", "Skin", "Candidainfeksjon hud", "Infeksjoner", "Hud", "Candidainfeksjon hud", "Infeksjonar", "Hud"),
  data.table("S76", "s76", "Skin infection other", "Infections", "Skin", "Hudinfeksjon IKA", "Infeksjoner", "Hud", "Hudinfeksjon IKA", "Infeksjonar", "Hud"),
  data.table("S84", "s84", "Impetigo", "Infections", "Skin", "Brennkopper", "Infeksjoner", "Hud", "Brennkoppar", "Infeksjonar", "Hud"),
  data.table("S95", "s95", "Molluscum contagiosum", "Infections", "Skin", "Molluscum contagiosum", "Infeksjoner", "Hud", "Molluscum contagiosum", "Infeksjonar", "Hud"),
  data.table("S99", "s99", "Skin disease, other", "Other diagnoses", "Skin", "Hud/underhud sykdom IKA", "Annet", "Hud", "Hud/underhud sjukdom IKA", "Anna", "Hud")
))
setnames(
  icpc2,
  c(
    "icpc2raw_tag",
    "icpc2group_tag",
    "icpc2group_description_en",
    "component_en",
    "bodysystem_en",
    "icpc2group_description_nb",
    "component_nb",
    "bodysystem_nb",
    "icpc2group_description_nn",
    "component_nn",
    "bodysystem_nn"
  )
)
icpc2[, includes_influenza_covid19 := icpc2group_tag %in% c("r80", "r991", "r992", "covid19", "respiratory_infections")]
icpc2[, has_historical_data := !icpc2group_tag %in% c("r991", "r992", "covid19")]
icpc2[, outbreak_pattern := "unknown"]
icpc2[icpc2group_tag %in% c("r991", "r992", "covid19"), outbreak_pattern := "valley_increase_decrease"]
icpc2[, short_term_trend_include_decreasing := outbreak_pattern %in% c("unknown")]

#' @export
reports <- list()
reports$descriptions <- rbindlist(list(
  data.table(
    report_tag = "selected",
    report_name_en = "Selected diseases",
    report_name_nb = "Utvalgte sykdommer",
    report_name_file_nb_utf = "utvalgte_sykdommer",
    report_name_file_nb_ascii = "utvalgte_sykdommer",
    report_name_nn = "Utvalde sjukdommar",
    report_name_file_nn_utf = "utvalde_sjukdommar",
    report_name_file_nn_ascii = "utvalde_sjukdommar"
  ),
  data.table(
    report_tag = "general_and_unspecified",
    report_name_en = "General and unspecified",
    report_name_nb = "Allment og uspesifisert",
    report_name_file_nb_utf = "allment_og_uspesifisert",
    report_name_file_nb_ascii = "allment_og_uspesifisert",
    report_name_nn = "Allment og uspesifisert",
    report_name_file_nn_utf = "allment_og_uspesifisert",
    report_name_file_nn_ascii = "allment_og_uspesifisert"
  ),
  data.table(
    report_tag = "blood_and_immune",
    report_name_en = "Blood, blood forming organs, and immune mechanism",
    report_name_nb = "Blod, bloddannende organer og immunsystemet",
    report_name_file_nb_utf = "blod_bloddannende_organer_og_immunsystemet",
    report_name_file_nb_ascii = "blod_bloddannende_organer_og_immunsystemet",
    report_name_nn = "Blod, bloddannande organ og immunsystemet",
    report_name_file_nn_utf = "blod_bloddannande_organer_og_immunsystemet",
    report_name_file_nn_ascii = "blod_bloddannande_organer_og_immunsystemet"
  ),
  data.table(
    report_tag = "digestive",
    report_name_en = "Digestive",
    report_name_nb = "Fordøyelsessystemet",
    report_name_file_nb_utf = "fordøyelsessystemet",
    report_name_file_nb_ascii = "fordoyelsessystemet",
    report_name_nn = "Fordøyingssystemet",
    report_name_file_nn_utf = "fordøyingssystemet",
    report_name_file_nn_ascii = "fordoyingssystemet"
  ),
  data.table(
    report_tag = "eye",
    report_name_en = "Eye",
    report_name_nb = "Øye",
    report_name_file_nb_utf = "øye",
    report_name_file_nb_ascii = "oye",
    report_name_nn = "Auge",
    report_name_file_nn_utf = "auge",
    report_name_file_nn_ascii = "auge"
  ),
  data.table(
    report_tag = "ear",
    report_name_en = "Ear",
    report_name_nb = "Øre",
    report_name_file_nb_utf = "øre",
    report_name_file_nb_ascii = "ore",
    report_name_nn = "Øre",
    report_name_file_nn_utf = "øre",
    report_name_file_nn_ascii = "ore"
  ),
  data.table(
    report_tag = "respiratory",
    report_name_en = "Respiratory",
    report_name_nb = "Luftveier",
    report_name_file_nb_utf = "luftveier",
    report_name_file_nb_ascii = "luftveier",
    report_name_nn = "Luftvegar",
    report_name_file_nn_utf = "luftvegar",
    report_name_file_nn_ascii = "luftvegar"
  ),
  data.table(
    report_tag = "skin",
    report_name_en = "Skin",
    report_name_nb = "Hud",
    report_name_file_nb_utf = "hud",
    report_name_file_nb_ascii = "hud",
    report_name_nn = "Hud",
    report_name_file_nn_utf = "hud",
    report_name_file_nn_ascii = "hud"
  ),
  data.table(
    report_tag = "all",
    report_name_en = "All",
    report_name_nb = "Alle",
    report_name_file_nb_utf = "alle",
    report_name_file_nb_ascii = "alle",
    report_name_nn = "Alle",
    report_name_file_nn_utf = "alle",
    report_name_file_nn_ascii = "alle"
  )
))
reports$icpc2 <- list()
reports$icpc2$selected <- c("a78", "a04", "gastroenteritis", "respiratory_infections", "covid19", "r80", "r71", "r72")
reports$icpc2$general_and_unspecified <- icpc2[bodysystem_en %in% c("General and unspecified")]$icpc2group_tag
reports$icpc2$blood_and_immune <- icpc2[bodysystem_en %in% c("Blood, blood forming organs, and immune mechanism")]$icpc2group_tag
reports$icpc2$digestive <- icpc2[bodysystem_en %in% c("Digestive")]$icpc2group_tag
reports$icpc2$eye <- icpc2[bodysystem_en %in% c("Eye")]$icpc2group_tag
reports$icpc2$ear <- icpc2[bodysystem_en %in% c("Ear")]$icpc2group_tag
reports$icpc2$respiratory <- icpc2[bodysystem_en %in% c("Respiratory")]$icpc2group_tag
reports$icpc2$skin <- icpc2[bodysystem_en %in% c("Skin")]$icpc2group_tag
reports$icpc2$all <- icpc2$icpc2group_tag

#' @export
fida_pilot <- list()
fida_pilot$icpc2 <- icpc2[icpc2group_tag %in% c("r71", "r80", "covid19")]

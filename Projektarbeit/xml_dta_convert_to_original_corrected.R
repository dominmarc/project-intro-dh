library(xml2)
library(dplyr)
library(stringr)

# functions
clean_text <- function(x) {
  x %>%
    # Zeilenumbrüche entfernen
    str_replace_all("[\\r\\n]+", " ") %>%
    # Leerzeichen normalisieren
    str_replace_all("[\\u00A0\\u202F]", " ") %>%
    # Leerzeichen vor Satzzeichen entfernen
    str_replace_all("\\s+([.,;:!?])", "\\1") %>% 
    # Leerzeichen vor Klammer entfernen
    str_replace_all("([\\(\\[\\{])\\s+", "\\1") %>%
    # Leerzeichen nach Klammer entfernen
    str_replace_all("\\s+([\\)\\]\\}])", "\\1") %>%
    # nur ein zusammenhängendes Leerzeichen
    str_replace_all("\\s+", " ") %>%
    # auf " beschränken
    str_replace_all("[„“]", "\"") %>%
    # auf ' beschränken
    str_replace_all("[‚‘’]", "'") %>%
    # auf - beschränken
    str_replace_all("[–—]", "-") %>%
    # trim start + end
    str_trim() %>%
    # Unicode Normalisierung (Vereinheitlichung von Sonderzeichen)
    stringi::stri_trans_nfc()
}

read_xml_create_text <- function(doc_name) {
  # XML einlesen
  doc <- read_xml(paste(doc_name, ".tcf.xml", sep =""))
  ns <- xml_ns(doc)
  
  # Original einlesen
  doc_tokens <- xml_find_all(doc, ".//d3:token", ns)
  
  doc_token_ids <- xml_attr(doc_tokens, "ID")
  doc_words_array <- xml_text(doc_tokens)   
  
  # Original-Text ausgeben
  doc_text_original <- paste(doc_words_array, collapse = " ")
  # Original-Text formattieren
  doc_text_original <- clean_text(doc_text_original)
  # Original-Text in Datei schreiben
  writeLines(doc_text_original, paste(doc_name, "_original.txt", sep =""))
  
  
  # orthographische Korrektur einlesen
  doc_corrections <- xml_find_all(doc, ".//d3:correction", ns)
  
  doc_corr_token_ids  <- xml_attr(doc_corrections, "tokenIDs")
  doc_corr_words_array <- xml_text(doc_corrections)
  # Mapping erstellen
  doc_corr_map <- setNames(doc_corr_words_array,doc_corr_token_ids)
  # Korrektur-Text zusammenfügen
  doc_corr_final_words_array <- ifelse(
    doc_token_ids %in% names(doc_corr_map),
    doc_corr_map[doc_token_ids],
    doc_words_array
  )
  
  # Korrektur-Text erstellen
  doc_text_correct <- paste(doc_corr_final_words_array, collapse = " ")
  # Korrektur-Text formattieren
  doc_text_correct <- clean_text(doc_text_correct)
  # Korrektur-Text in Datei schreiben
  writeLines(doc_text_correct, paste(doc_name, "_correct.txt", sep =""))
}

read_xml_create_texts <- function(doc_names) {
  for (doc_name in doc_names){
    read_xml_create_text(doc_name)
  }
}

# set base working directory
setwd("[...]/Projektarbeit/korpus/dta")

# https://www.deutschestextarchiv.de/book/show/kant_rvernunft_1781
# https://www.deutschestextarchiv.de/book/download_fulltcf/16204
kant_krv_doc_name <- "kant_rvernunft_1781"
# https://www.deutschestextarchiv.de/book/show/kant_urtheilskraft_1790
# https://www.deutschestextarchiv.de/book/download_fulltcf/16401
kant_ku_doc_name <- "kant_urtheilskraft_1790"
# https://www.deutschestextarchiv.de/book/show/kant_pvernunft_1788
# https://www.deutschestextarchiv.de/book/download_fulltcf/16400
kant_kpv_doc_name <- "kant_pvernunft_1788"

# specifiy to Kant working directory
setwd("Kant")
read_xml_create_texts(list(kant_krv_doc_name, kant_ku_doc_name, kant_kpv_doc_name))

# https://www.deutschestextarchiv.de/book/show/schiller_erziehung01_1795
# https://www.deutschestextarchiv.de/book/download_fulltcf/30525
schiller_bae_doc1_name <- "schiller_erziehung01_1795"
# https://www.deutschestextarchiv.de/book/show/schiller_erziehung02_1795
# https://www.deutschestextarchiv.de/book/download_fulltcf/30526
schiller_bae_doc2_name <- "schiller_erziehung02_1795"
# https://www.deutschestextarchiv.de/book/show/schiller_erziehung03_1795
# https://www.deutschestextarchiv.de/book/download_fulltcf/30527
schiller_bae_doc3_name <- "schiller_erziehung03_1795"

# update to Schiller working directory
setwd("../Schiller")
# create texts
read_xml_create_texts(list(schiller_bae_doc1_name, schiller_bae_doc2_name, schiller_bae_doc3_name))

# https://www.deutschestextarchiv.de/book/show/schiller_naive01_1795
# https://www.deutschestextarchiv.de/book/download_fulltcf/16483
schiller_nsd_doc1_name <- "schiller_naive01_1795"
# https://www.deutschestextarchiv.de/book/show/schiller_naive02_1795
# https://www.deutschestextarchiv.de/book/download_fulltcf/17246
schiller_nsd_doc2_name <- "schiller_naive02_1795"
# https://www.deutschestextarchiv.de/book/show/schiller_naive03_1796
# https://www.deutschestextarchiv.de/book/download_fulltcf/17247
schiller_nsd_doc3_name <- "schiller_naive03_1796"
# create texts
read_xml_create_texts(list(schiller_nsd_doc1_name, schiller_nsd_doc2_name, schiller_nsd_doc3_name))

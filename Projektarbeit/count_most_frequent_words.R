library(xml2)
library(dplyr)
library(stringr)
library(stringi)

# functions
read_xml_analyze_top <- function(doc_names, t_type) {
  topWords <- doc_names %>%
    str_c(".tcf.xml") %>% 
    lapply(function(f) {
      doc <- read_xml(f)
      xml_find_all(doc, str_c(".//d3:", t_type), ns = xml_ns(doc)) %>% 
        xml_text()
    }) %>%
    unlist() %>%
    tibble(word = .) %>%
    filter(stringi::stri_length(word) >= 5) %>%
    filter(!str_detect(word, "^[.,;:!?()\\[\\]{}\"'—–-]+$")) %>%
    count(word, sort = TRUE) %>%
    slice_head(n = 150) %>% 
    mutate(rank = row_number()) %>% 
    select(rank, word, n)
  
  topWordsTxt <- paste(topWords$rank, topWords$n, topWords$word, sep = "\t")
  writeLines(topWordsTxt, sprintf("%s_top_%s.txt", doc_names[[1]], t_type))
}

# set base working directory
setwd("[...]/Projektarbeit/korpus/dta")
t_types <- list("token", "lemma")

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
# create top words txt
for (t_type in t_types){
  read_xml_analyze_top(list(kant_kpv_doc_name), t_type)
  read_xml_analyze_top(list(kant_ku_doc_name), t_type)
  read_xml_analyze_top(list(kant_krv_doc_name), t_type)
}

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
# create top words txt
for (t_type in t_types){
  read_xml_analyze_top(list(schiller_bae_doc1_name, schiller_bae_doc2_name, schiller_bae_doc3_name), t_type)
}

# https://www.deutschestextarchiv.de/book/show/schiller_naive01_1795
# https://www.deutschestextarchiv.de/book/download_fulltcf/16483
schiller_nsd_doc1_name <- "schiller_naive01_1795"
# https://www.deutschestextarchiv.de/book/show/schiller_naive02_1795
# https://www.deutschestextarchiv.de/book/download_fulltcf/17246
schiller_nsd_doc2_name <- "schiller_naive02_1795"
# https://www.deutschestextarchiv.de/book/show/schiller_naive03_1796
# https://www.deutschestextarchiv.de/book/download_fulltcf/17247
schiller_nsd_doc3_name <- "schiller_naive03_1796"
# create top words txt
for (t_type in t_types){
  read_xml_analyze_top(list(schiller_nsd_doc1_name, schiller_nsd_doc2_name, schiller_nsd_doc3_name), t_type)
}


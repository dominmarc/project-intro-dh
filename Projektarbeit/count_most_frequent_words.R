library(xml2)
library(dplyr)
library(stringr)
library(stringi)

# globald results of the top words (per doc)
all_topwords <- list()

# functions
read_xml_analyze_top <- function(doc_names, t_type) {
  doc_id <- doc_names[[1]]
  
  topWords <- doc_names %>%
    # append file type
    str_c(".tcf.xml") %>% 
    # read words from XML
    lapply(function(f) {
      doc <- read_xml(f)
      xml_find_all(doc, str_c(".//d3:", t_type), ns = xml_ns(doc)) %>% 
        xml_text()
    }) %>%
    # vectorize
    unlist() %>%
    # create table (tibble)
    tibble(word = .) %>%
    # min length = 5
    filter(stringi::stri_length(word) >= 5) %>%
    # avoid special characters
    filter(!str_detect(word, "^[.,;:!?()\\[\\]{}\"'—–-]+$")) %>%
    # count words and sort dsc
    count(word, sort = TRUE) %>%
    # TOP 150 of counted words
    slice_head(n = 150) %>% 
    # add rank for better visualization
    mutate(rank = row_number()) %>% 
    # get created columns
    select(rank, word, n)
  
  # save top word results in global variable with doc related key
  key <- paste(doc_id, t_type, sep = "_")
  all_topwords[[key]] <<- topWords     # superassignment
  
  # write top word results to file
  topWordsTxt <- paste(topWords$rank, topWords$n, topWords$word, sep = "\t")
  writeLines(topWordsTxt, sprintf("%s_top_%s.txt", doc_id, t_type))
}

append_overlap_results <- function(base_doc, t_type) {
  base_key <- paste(base_doc, t_type, sep = "_")
  base_words <- all_topwords[[base_key]]$word
  
  # calculate overlap for each doc
  for (key in names(all_topwords)) {
    if (grepl(t_type, key) & key != base_key) {
      
      compare_words <- all_topwords[[key]]$word
      overlap <- intersect(base_words, compare_words)
      
      # create text for calculated overlap result
      overlap_text <- c(
        "",
        "==============================",
        paste("Overlap with:", key),
        paste("Number of shared MFW:", length(overlap)),
        "Words:",
        paste(overlap, collapse = ", ")
      )
      
      # append each file with calculated overlap 
      file_name <- sprintf("%s_top_%s.txt", base_doc, t_type)
      write(overlap_text, file = file_name, append = TRUE)
    }
  }
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
# create top words txt for Kant
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
# create top words txt for Schiller (Ästhetische Erziehung)
for (t_type in t_types){
  read_xml_analyze_top(
    list(schiller_bae_doc1_name, schiller_bae_doc2_name, schiller_bae_doc3_name),
    t_type
  )
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
# create top words txt for Schiller (Naive sentimentalische Dichtung)
for (t_type in t_types){
  read_xml_analyze_top(
    list(schiller_nsd_doc1_name, schiller_nsd_doc2_name, schiller_nsd_doc3_name),
    t_type
  )
}

# calculate token and lemma overlaps
for (t_type in t_types){
  # calculate only for Schillers 'Ästhetische Erziehung'
  # multiple docs -> first doc is the key
  append_overlap_results("schiller_erziehung01_1795", t_type)
}
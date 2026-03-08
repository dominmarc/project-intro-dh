library(rvest)
library(xml2)
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

scrape_text <- function(url) {
  url %>%
    read_html() %>%
    html_element("div.book-reader__chapter-text") %>%
    html_text2() %>%
    paste(collapse = " ")
}

create_text <- function(url, name, chapters){
  text <- ""
  for (chapter in 1:chapters){
    text_part <- scrape_text(paste(url, "chapter/", chapter, "/", sep = ""))
    text <- paste(text, text_part, sep = " ")
  }
  
  text <- clean_text(text)
  
  writeLines(text, paste0(name, "_c.txt"))
}

urls <- list(
  # Grundlegung zur Metaphysik der Sitten
  gms = list(
    value="https://projekt-gutenberg.org/authors/immanuel-kant/books/grundlegung-zur-metaphysik-der-sitten/",
    chapters=5
  ),
  # Die Metaphysik der Sitten
  mds = list(
    value="https://projekt-gutenberg.org/authors/immanuel-kant/books/die-metaphysik-der-sitten/",
    chapters=8
  ),
  # Prolegomena zu einer jeden künftigen Metaphysik die als Wissenschaft wird auftreten können
  prol = list(
    value="https://projekt-gutenberg.org/authors/immanuel-kant/books/prolegomena-zu-einer-jeden-kuenftigen-metaphysik-die-als-wissenschaft-wird-auftreten-koennen/",
    chapters=1
  ),
  # Untersuchung über die Deutlichkeit der Grundsätze der natürlichen Theologie und der Moral
  grundTheoMoral = list(
    value="https://projekt-gutenberg.org/authors/immanuel-kant/books/immanuel-kant-untersuchung-ueber-die-deutlichkeit-der-grundsaetze-der-natuerlichen-theologie-und-der-moral/",
    chapters=11
  ),
  # Beobachtungen über das Gefühl des Schönen und Erhabenen
  gefuehlSchErh = list(
    value="https://projekt-gutenberg.org/authors/immanuel-kant/books/immanuel-kant-beobachtungen-ueber-das-gefuehl-des-schoenen-und-erhabenen/",
    chapters=5
  )
)

# in Ziel-Ausgabe-Ordner wechseln
setwd("[...]/Projektarbeit/korpus/projekt-gutenberg/Kant")

# Texte aus alle Quellen generieren
for (name in names(urls)){
  urlEntry <- urls[[name]]
  urlValue <- urlEntry$value
  cat(urlValue, "\n")
  create_text(urlValue, name, urlEntry$chapters)
  cat(name, "(done)\n")
}
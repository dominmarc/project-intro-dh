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
    html_element("div.entry-content") %>%
    {
      html_elements(., "div.pageNaviTop, div.intextGA, div.adBottom, div.prepostnav, div.imus-share, div.topnav") %>%
        xml_remove()
      .
    } %>%
    html_text2() %>%
    paste(collapse = " ")
}

create_text <- function(url, name){
  text <- scrape_text(url)
  text <- clean_text(text)
  
  writeLines(text, paste0(name, "_c.txt"))
}

# Web-Quellen der Texte
urls <- list(
  # Über den Zusammenhang der thierischen Natur des Menschen mit seiner geistigen
  zsmThierGeist = "https://www.friedrich-schiller-archiv.de/philosophische-schriften/ueber-den-zusammenhang-der-thierischen-natur-des-menschen-mit-seiner-geistigen/",
  # Zerstreute Betrachtungen über verschiedene ästhetische Gegenstände
  betrAestGeg = "https://www.friedrich-schiller-archiv.de/philosophische-schriften/zerstreute-betrachtungen-ueber-verschiedene-aesthetische-gegenstaende/",
  # Philosophie der Physiologie
  physiol = "https://www.friedrich-schiller-archiv.de/philosophische-schriften/philosophie-der-physiologie/",
  # Über Anmuth und Würde
  anmuth = "https://www.friedrich-schiller-archiv.de/philosophische-schriften/ueber-anmuth-und-wuerde/",
  # Über das Pathetische
  pathet = "https://www.friedrich-schiller-archiv.de/philosophische-schriften/ueber-das-pathetische/",
  # Über das Erhabene
  erhaben = "https://www.friedrich-schiller-archiv.de/philosophische-schriften/ueber-das-erhabene/",
  # Über den Grund des Vergnügens an tragischen Gegenständen
  vergnTragGeg = "https://www.friedrich-schiller-archiv.de/philosophische-schriften/ueber-den-grund-des-vergnuegens-an-tragischen-gegenstaenden/",
  # Über die nothwendigen Grenzen beim Gebrauch schöner Formen
  grenzForm = "https://www.friedrich-schiller-archiv.de/philosophische-schriften/ueber-die-nothwendigen-grenzen-beim-gebrauch-schoener-formen/"
)

# in Ziel-Ausgabe-Ordner wechseln
setwd("[...]/Projektarbeit/korpus/schiller-archiv")

# Texte aus alle Quellen generieren
for (urlName in names(urls)){
  urlValue <- urls[[urlName]]
  cat(urlValue, "\n")
  create_text(urlValue, urlName)
  cat(urlName, "(done)\n")
}

library(stylo)
library(dplyr)

# Deine Einstellungen
segment_sizes <- c(3000, 5000, 7000)
culling_values <- c(0, 20, 50)

# Pfad zum Korpus
corpus_path <- "[...]/Projektarbeit/Stylo_Kant_Schiller_all_corrected/corpus"

# Pfad zur Ausgabe
results_path <- "[...]/Projektarbeit/Stylo_Kant_Schiller_all_corrected"
dir.create(results_path, showWarnings = FALSE)

# in Hauptordner springen
setwd(results_path)

# Pipeline-Schleifen
for(seg in segment_sizes){
  # Unterordner pro Segmentierungsstufe anlegen
  out_dir <- file.path(results_path, paste0("stylo_1_segment_", seg))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  # zur Ausgabe der PNG in Ausgabe-Ordner wechseln
  setwd(out_dir)
  # die Culling-Stufen testen
  for(cull in culling_values){
    # Stylo-Analyse mit CA und PCA (with correlation) ausführen
    for (type in list("CA", "PCR")){
      stylo(
        corpus.dir = corpus_path,       # Korpus-Verzeichnis
        corpus.lang = "German",
        analysis.type = type,           # Analyse-Typ
        distance.measure = "delta",     # Burrows’ Delta
        sample.size = seg,              # in Segmente mit X Wörtern teilen
        sampling = "normal.sampling",   # Samples der Teile erstellen
        mfw.min = 100,
        mfw.max = 1000,
        mfw.incr = 300, 
        culling.min = cull,             # ab unter X% Vorkommen abschneiden
        culling.max = cull,
        culling.incr = 1,
        delete.stop.words = FALSE,      # Funktionswörter behalten
        preserve.case = TRUE,           # Groß-/Klein behalten
        write.png.file = TRUE,          # als PNG ausgeben
        gui = FALSE                     # GUI nicht öffnen
      )
    }
    
    # zurück in Hauptordner springen
    setwd(out_dir)
    
    cat(paste("Segment:", seg, "Culling:", cull, " (done)\n"))
  }
}
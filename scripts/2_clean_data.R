# Filtering rows where "ja" is present in the agreement column
data_19 <- data_19_raw %>%
  filter(grepl("ja", data_19_raw$X...WICHTIG.....Ich.bin.damit.einverstanden..dass.die.mit.dieser.Umfrage.gesammelten..anonymen..Daten.zum.Zwecke.der.Veranschaulichung.von.Methoden.in.den.Vorlesungen.Statistik.1...2.sowie.in.den.dazugehoerigen.Tutorien.genutzt.werden..Dieses.Einverstaendnis.kann.jederzeit.durch.Nachricht.an.den.Dozenten.widerrufen.werden..Potenziell.sensible.Informationen.werden.nicht.an.Dritte.weitergegeben.., ignore.case = TRUE))

data_20 <- data_20_raw %>%
  filter(grepl("ja", data_20_raw$X...WICHTIG.....Ich.bin.damit.einverstanden..dass.die.mit.dieser.Umfrage.gesammelten..anonymen..Daten.vom.Dozenten.zum.Zwecke.der.Veranschaulichung.von.Methoden.genutzt.werden..Dieses.Einverstaendnis.kann.jederzeit.durch.Nachricht.an.den.Dozenten.widerrufen.werden..Potenziell.sensible.Informationen.werden.nicht.an.Dritte.weitergegeben.., ignore.case = TRUE))

data_21 <- data_21_raw %>%
  filter(grepl("ja", data_21_raw$X...WICHTIG.....Ich.bin.damit.einverstanden..dass.die.mit.dieser.Umfrage.gesammelten..anonymen..Daten.vom.Dozenten.zum.Zwecke.der.Veranschaulichung.von.Methoden.genutzt.werden..Dieses.Einverstaendnis.kann.jederzeit.durch.Nachricht.an.den.Dozenten.widerrufen.werden..Potenziell.sensible.Informationen.werden.nicht.an.Dritte.weitergegeben.., ignore.case = TRUE))

data_22 <- data_22_raw %>%
  filter(grepl("ja", data_22_raw$X...WICHTIG.....Ich.bin.damit.einverstanden..dass.die.mit.dieser.Umfrage.gesammelten..anonymen..Daten.vom.Dozenten.zum.Zwecke.der.Veranschaulichung.von.Methoden.genutzt.werden..Dieses.Einverstaendnis.kann.jederzeit.durch.Nachricht.an.den.Dozenten.widerrufen.werden..Potenziell.sensible.Informationen.werden.nicht.an.Dritte.weitergegeben.., ignore.case = TRUE))

data_23 <- data_23_raw %>%
  filter(grepl("ja", data_23_raw$X...WICHTIG.....Ich.bin.damit.einverstanden..dass.die.mit.dieser.Umfrage.gesammelten..anonymen..Daten.vom.Dozenten.zum.Zwecke.der.Veranschaulichung.von.Methoden.genutzt.werden..Potenziell.sensible.Informationen.werden.nicht.an.Dritte.weitergegeben.., ignore.case = TRUE))

data_24 <- data_24_raw %>%
  filter(grepl("ja", data_24_raw$X...WICHTIG.....Ich.bin.damit.einverstanden..dass.die.mit.dieser.Umfrage.gesammelten..anonymen..Daten.vom.Dozenten.zum.Zwecke.der.Veranschaulichung.von.Methoden.genutzt.werden..Potenziell.sensible.Informationen.werden.nicht.an.Dritte.weitergegeben.., ignore.case = TRUE))


#Creating a new column with year
data_19 <- data_19 %>%
  mutate(Jahr = year(ymd_hms(data_19$Timestamp)))

data_20 <- data_20 %>%
  mutate(Jahr = year(ymd_hms(data_20$Zeitstempel)))

data_21 <- data_21 %>%
  mutate(Jahr = year(ymd_hms(data_21$Zeitstempel)))

data_22 <- data_22 %>%
  mutate(Jahr = year(ymd_hms(data_22$Zeitstempel)))

data_23 <- data_23 %>%
  mutate(Jahr = year(ymd_hms(data_23$Zeitstempel)))

data_24 <- data_24 %>%
  mutate(Jahr = year(ymd_hms(data_24$Zeitstempel)))


#Renaming the columns
dictionary <- list(
  "Zeitstempel" = c("zeitstempel", "timestamp"),
  "Einverstaendniserklaerung" = c("einverstanden", "einverstaendnis", "Zwecke.der.Veranschaulichung"),
  "Geschlecht" = c("Geschlecht"),
  "Schuhgroesse" = c("Schuhgroesse"),
  "Koerpergroesse" = c("Koerpergroesse", "Koerpergroesse..in.cm..", "Koerpergroesse..steht.in.Ihrem.Perso"),
  "Liegestuetze" = c("Liegestuetze"),
  "Statistik" = c("interessieren.Sie.sich.fuer.Statistik"),
  "Wirtschaft" = c("interessieren.Sie.sich.fuer.wirtschaftliche.Zusammenhaenge"),
  "Mathe" = c("interessieren.Sie.sich.fuer.Mathematik"),
  "Coden" = c("Programmiertaetigkeiten"),
  "Abi" = c("Abischnitt"),
  "Punkte_Mathe" = c("Punkte.hatten.Sie.in.der.Schule.zuletzt.in.Mathematik"),
  "Dr" = c("Dr..zu.machen"),
  "Entfernung" = c("entfernt.von.der.Uni"),
  "Dauer" = c("Weg.zur.Uni", "Dauer..in.Minuten"),
  "Partnerschaft" = c("Partner", "Partnerin"),
  "Partei" = c("Partei"),
  "Bildungsgrad" = c("Bildungsgrad"),
  "Arbeit" = c("Studium.zu.finanzieren"),
  "Einstiegsgehalt" = c("Einstiegsgehalt"),
  "Ostwestfalen" = c("Ostwestfalen"),
  "Handy" = c("Handy.Bildschirmzeit"),
  "Alkohol" = c("Alkohol"),
  "Rauchen" = c("Rauchen"),
  "Kiffen" = c("gekifft"),
  "Gendern" = c("gendern")
)


rename_columns <- function(df, dictionary) {
  for (standard_name in names(dictionary)) {
    synonyms <- dictionary[[standard_name]]
    for (synonym in synonyms) {
      # Find columns that match the synonym (partial or full match)
      match_idx <- grep(synonym, names(df), ignore.case = TRUE)
      if (length(match_idx) > 0) {
        names(df)[match_idx] <- standard_name
      }
    }
  }
  return(df)
}

data_19 <- rename_columns(data_19, dictionary)
data_20 <- rename_columns(data_20, dictionary)
data_21 <- rename_columns(data_21, dictionary)
data_22 <- rename_columns(data_22, dictionary)
data_23 <- rename_columns(data_23, dictionary)
data_24 <- rename_columns(data_24, dictionary)


'
datasets <- list(Umfrage19, Umfrage20, Umfrage21, Umfrage22, Umfrage23, Umfrage24)

all_columns <- unique(unlist(lapply(datasets, colnames)))

datasets <- lapply(datasets, function(df) {
  missing_cols <- setdiff(all_columns, colnames(df))
  for (col in missing_cols) {
    df[[col]] <- "no values in the original dataset"
  }
  return(df)
})

merged_dataset <- do.call(rbind, datasets)
'
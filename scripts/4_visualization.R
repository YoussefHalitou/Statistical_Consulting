#Slide 74

#simple tables
table(df_19$Bildungsgrad)
table(df_19$Bildungsgrad)/184

#sophisticated tables
total <- nrow(df_19)

education_table <- df_19 %>%
  count(Bildungsgrad) %>%  
  mutate(
    relative_frequency_formula = paste0(n, "/", total, " ≈ ", round(n / total, 3)),
    relative_frequency_value = round(n / total, 3)
  ) %>%
  rename(
    Ausprägung = Bildungsgrad,    
    absolute_hk = n              
  )

kable(
  education_table,
  col.names = c("Ausprägung", "absolute Häufigkeiten hₖ", "relative Häufigkeiten fₖ (Formel)", "relative Häufigkeiten fₖ (Wert)"),
  caption = paste("Häufigkeitstabelle für Merkmal X = Bildungsgrad der Eltern (n =", total, ").")
)

#Slide 75
total <- nrow(df_19)

party_table <- df_19 %>%
  count(Partei) %>%  
  mutate(
    relative_frequency_formula = paste0(n, "/", total, " ≈ ", round(n / total, 3)),
    relative_frequency_value = round(n / total, 3)
  ) %>%
  rename(
    Ausprägung = Partei,    
    absolute_hk = n         
  )

kable(
  party_table,
  col.names = c("Ausprägung", "absolute Häufigkeiten hₖ", "relative Häufigkeiten fₖ (Formel)", "relative Häufigkeiten fₖ (Wert)"),
  caption = paste("Häufigkeitstabelle für Merkmal X = Parteipräferenz. (n =", total, ").")
)

#Nicht dargestellt hier: “darf nicht wählen” (2%), “werde nicht wählen” (21%!!). <- entfernen bei Bedarf

#Slide 76
total_22 <- nrow(df_22)
total_23 <- nrow(df_23)

party_table_22 <- df_22 %>%
  count(Partei) %>%
  mutate(
    relative_frequency_2022 = round(n / total_22, 3)
  ) %>%
  rename(
    Ausprägung = Partei,
    absolute_hk_2022 = n
  )

party_table_23 <- df_23 %>%
  count(Partei) %>%
  mutate(
    relative_frequency_2023 = round(n / total_23, 3)
  ) %>%
  rename(
    Ausprägung = Partei,
    absolute_hk_2023 = n
  )

party_table_comparison <- full_join(party_table_22, party_table_23, by = "Ausprägung")

kable(
  party_table_comparison,
  col.names = c("Ausprägung", "absolute Häufigkeiten 2022", "relative Häufigkeiten fₖ 2022", 
                "absolute Häufigkeiten 2023", "relative Häufigkeiten fₖ 2023"),
  caption = paste("Häufigkeitstabelle für Merkmal X = Parteipräferenz, Vergleich Kohorte 2022 (n =", total_22, 
                  ") vs. Kohorte 2023 (n =", total_23, ").")
)

exclude_values <- c("nicht waehlen")

df_22_filtered <- df_22 %>%
  filter(!Partei %in% exclude_values & !is.na(Partei))  

df_23_filtered <- df_23 %>%
  filter(!Partei %in% exclude_values & !is.na(Partei))  

total_22 <- nrow(df_22_filtered)
total_23 <- nrow(df_23_filtered)

party_table_22 <- df_22_filtered %>%
  count(Partei) %>%
  mutate(
    relative_frequency_2022 = round(n / total_22, 3)
  ) %>%
  rename(
    Ausprägung = Partei,
    absolute_hk_2022 = n
  )

party_table_23 <- df_23_filtered %>%
  count(Partei) %>%
  mutate(
    relative_frequency_2023 = round(n / total_23, 3)
  ) %>%
  rename(
    Ausprägung = Partei,
    absolute_hk_2023 = n
  )


party_table_comparison <- full_join(party_table_22, party_table_23, by = "Ausprägung")


kable(
  party_table_comparison,
  col.names = c("Ausprägung", "absolute Häufigkeiten 2022", "relative Häufigkeiten fₖ 2022", 
                "absolute Häufigkeiten 2023", "relative Häufigkeiten fₖ 2023"),
  caption = paste("Häufigkeitstabelle für Merkmal X = Parteipräferenz, Vergleich Kohorte 2022 (n =", total_22, 
                  ") vs. Kohorte 2023 (n =", total_23, ").")
)

#Slide 80
barplot(table(df_19$Partnerschaft), col = "darkred", ylab = "Anzahl")
partnerschaft_counts <- table(df_19$Partnerschaft)


partnerschaft_counts <- sort(partnerschaft_counts, decreasing = TRUE)
barplot(partnerschaft_counts, col = "darkred", ylab = "Anzahl", main = "Sind Sie in einer Partnerschaft?")


#Slide 81
barplot(table(df_19$Partnerschaft) / length(df_19$Partnerschaft), col = "darkred", ylab = "Relative Frequency")
partnerschaft_relative_freq <- table(df_19$Partnerschaft) / length(df_19$Partnerschaft)


partnerschaft_relative_freq <- sort(partnerschaft_relative_freq, decreasing = TRUE)

barplot(partnerschaft_relative_freq, col = "darkred", ylab = "Relative Frequency", main = "Partnerschaft Relative Frequency (Descending Order)")


partnerschaft_relative_freq <- table(df_19$Partnerschaft) / length(df_19$Partnerschaft)


partnerschaft_relative_freq <- sort(partnerschaft_relative_freq, decreasing = TRUE)


barplot(
  partnerschaft_relative_freq,
  col = "darkred",
  ylab = "Relative Frequency",
  main = "Partnerschaft Relative Frequency (Descending Order)",
  ylim = c(0, 0.6)  # Extend y-axis limit to 0.6
)


#Slide 82
barplot(
  table(df_24$Alkohol), 
  col = "darkblue", 
  ylab = "Anzahl", 
  cex.lab = 1,  # Adjust the value to make it smaller
  cex.names = 1,
  horiz = T
)


#Slide 83
barplot(
  table(df_24$Alkohol) / length(df_24$Alkohol), 
  col = "darkblue", 
  ylab = "Anzahl", 
  cex.lab = 1,  # Adjust the value to make it smaller
  cex.names = 0.4,
  horiz = T,
)

#Slide 84
counts <- table(df_24$Kiffen)  # Get counts of each category

percentages <- round(100 * counts / sum(counts), 1)
labels <- paste0(percentages, "%")  # Create labels with percentages

pie(
  counts, 
  labels = labels,  # Use percentage labels
  col = c("skyblue", "orange"),  # Custom colors for each category
  main = "Schonmal gekifft?”"
)

legend("topright", legend = names(counts), fill = c("skyblue", "orange"))

#Slide 87
counts <- table(df_24$Partei)  # Get counts of each category

party_colors <- c(
  "SPD" = "red",
  "CDU/CSU" = "black",
  "AfD" = "blue",
  "FDP" = "yellow",
  "Gruene" = "green",
  "Die Linke" = "pink"
)

percentages <- round(100 * counts / sum(counts), 1)
labels <- paste0(percentages, "%")  # Add percentage signs to labels

pie(
  counts, 
  col = party_colors[names(counts)],  # Map colors to categories
  main = "Political Preference Distribution",
  cex = 0.5
)

exclude_values <- c("nicht waehlen", "BSW" , "eine andere Partei")

data_subset <- df_24$Partei[!(df_24$Partei %in% exclude_values)]


table_data <- table(data_subset)

colors_to_use <- party_colors[names(data_subset)]


pie(table_data, 
    col = colors_to_use, 
    main = "Pie Chart Excluding Specific Entries")


legend("topright", legend = names(table_data), fill = party_colors[names(counts)], cex = 1)






















#Slide 160
boxplot(df_24$Abi, bty = "n", ylab= "Abischnitt" , frame = FALSE)

boxplot(df_24$Abi, col="gray", bty="n", ylab="Abiturschnitt", frame = TRUE)


boxplot(df_24$Abi, col = "gray", ylab = "Abiturschnitt", frame = FALSE, ylim = c(1.0, 4.0))


#Slide 162
boxplot(Abi ~ Partei, data = df_24, 
        col = "grey", 
        ylab = "Value", 
        xlab = "Category",
        ylim = c(1.0, 4.0), # Adjust ylim as needed
        frame = F)

#Slide 169
table(df_24$Alkohol, df_24$Partnerschaft)

#Slide 170
table(df_24$Alkohol, df_24$Partnerschaft)/length(df_24$Alkohol)
table(df_24$Alkohol, df_24$Partnerschaft)/length(df_24$Partnerschaft)


table_data <- table(df_24$Partnerschaft, df_24$Alkohol)


mosaicplot(table_data, main = "Mosaic Plot of Alkohol vs. Partnerschaft", 
           xlab = "Partnerschaft", ylab = "Alkohol", col = TRUE)


#Slide 175
df_24$Entfernung_binned <- cut(df_24$Entfernung, breaks = 10)  
df_24$Dauer_binned <- cut(df_24$Dauer, breaks = 10)            

table_data <- table(df_24$Entfernung_binned, df_24$Dauer_binned)

heatmap(as.matrix(table_data), Rowv = NA, Colv = NA, 
        col = heat.colors(256), scale = "none",
        xlab = "Dauer (binned)", ylab = "Entfernung (binned)", 
        main = "Heatmap of Entfernung vs Dauer")


heatmap.2(as.matrix(table_data), dendrogram = "none", trace = "none",
          col = bluered(256),  # Color scheme
          key = TRUE, key.title = "Count",
          xlab = "Dauer (binned)", ylab = "Entfernung (binned)", 
          main = "Heatmap of Entfernung vs Dauer")

#Slide 179
plot(df_24$Handy, df_24$Abi, 
     xlab = "Handyzeit pro Tag in Stunden", 
     ylab = "Abischnitt",
     frame = F)

plot(df_24$Handy, df_24$Abi, 
     xlab = "Handyzeit pro Tag in Stunden", 
     ylab = "Abischnitt", 
     frame = FALSE, 
     col = "black", 
     pch = 16) # Use pch to set the point character (shape)

#Slide 180
plot(jitter(df_24$Handy), df_24$Abi, 
     xlab = "Handyzeit pro Tag in Stunden", 
     ylab = "Abischnitt", 
     frame = FALSE, 
     col = "black", 
     pch = 16)

plot(df_24$Handy, jitter(df_24$Abi), 
     xlab = "Handyzeit pro Tag in Stunden", 
     ylab = "Abischnitt", 
     frame = FALSE, 
     col = "black", 
     pch = 16)

#Slide 181
plot(df_24$Abi, df_24$Einstiegsgehalt, 
     xlab = "Abischnitt", 
     ylab = "Einstiegsgehalt", 
     frame = FALSE, 
     col = "black", 
     pch = 16) # Use pch to set the point character (shape)


#Slide 183
point_symbol <- ifelse(df_24$Geschlecht == "maennlich", 4, 1)  # 4 for "x", 1 for "o"


plot(df_24$Koerpergroesse, df_24$Schuhgroesse,
     pch = point_symbol, col = "blue", 
     xlab = "Körpergröße", ylab = "Schuhgröße", 
     main = "Scatterplot with Symbols as Third Variable")


legend("topright", legend = c("Maennlich", "Weiblich"), 
       pch = c(4, 1), col = "blue", bty = "n")




point_size <- ifelse(df_24$Geschlecht == "maennlich", 0.5, 1.5)


plot(df_24$Koerpergroesse, df_24$Schuhgroesse,
     cex = point_size, pch = 16, col = "blue", 
     xlab = "Körpergröße", ylab = "Schuhgröße", 
     main = "Scatterplot with Size as Third Variable")



#Slide 184
colors <- ifelse(df_24$Geschlecht == "maennlich", "blue", "pink")

plot(df_24$Koerpergroesse, df_24$Schuhgroesse, 
     col = colors, pch = 16, 
     xlab = "Körpergröße", ylab = "Schuhgröße", 
     main = "Scatterplot with 3 Variables (Color by Geschlecht)")


legend("topright", legend = c("Maennlich", "Weiblich"), 
       col = c("blue", "pink"), pch = 16, 
       bty = "n")  # Removes the box around the legend


colors <- ifelse(df_24$Geschlecht == "maennlich", "blue", "pink")


plot(df_24$Koerpergroesse, df_24$Schuhgroesse, 
     col = colors, pch = 16, 
     xlab = "Körpergröße", ylab = "Schuhgröße", 
     main = "Scatterplot with 3 Variables (Color by Geschlecht)")


legend("topright", legend = c("Maennlich", "Weiblich"), 
       col = c("blue", "pink"), pch = 16)  


#Slide 185
scatterplot3d(df_24$Koerpergroesse, df_24$Schuhgroesse, df_24$Liegestuetze, 
              xlab = "Koerpergroesse", 
              ylab = "Schuhgroesse", 
              zlab = "Liegestuetze")


scatterplot3d(df_24$Koerpergroesse, df_24$Schuhgroesse, df_24$Liegestuetze,
              xlab = "Koerpergroesse", 
              ylab = "Schuhgroesse", 
              zlab = "Liegestuetze",
              color = "black",  # Sets the color of the points (outline for non-filled symbols)
              pch = 16)         # Sets the point character to filled circles

#Slide 186
scatterplot3d(df_24$Koerpergroesse, df_24$Schuhgroesse, df_24$Liegestuetze, 
              xlab = "Koerpergroesse", 
              ylab = "Schuhgroesse", 
              zlab = "Liegestuetze",
              color = colors,
              pch = 16)

#Slide 187
df_subset <- df_24[, c("Liegestuetze", "Koerpergroesse", "Schuhgroesse")]

pairs(df_subset, 
      main = "Scatterplot Matrix von Koerpergroesse, Schuhgroesse, and Liegestuetze",
      pch = 16,  
      col = "blue")  


#Slide 221
set.seed(123)  # Set a seed for reproducibility


n_rolls <- 1000


rolls <- sample(1:6, n_rolls, replace = TRUE)


is_even <- rolls %% 2 == 0
cumulative_proportion <- cumsum(is_even) / (1:n_rolls)



plot(1:n_rolls, cumulative_proportion, type = "l", 
     xlab = "Anzahl Würfe n", 
     ylab = "rel. Häufigk. von geraden Zahlen", 
     main = "Relative Frequency of Even Numbers in Dice Rolls",
     frame = F)


abline(h = 0.5, col = "red", lty = 2)

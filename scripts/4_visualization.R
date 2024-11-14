# At the beginning of each script
source("scripts/0_setup.R")


#Slide 74

#simple tables
table(df_19$Bildungsgrad)
table(df_19$Bildungsgrad)/184

#sophisticated tables
# Calculate the total number of observations
total <- nrow(df_19)

# Calculate frequency table with flexible total count
education_table <- df_19 %>%
  count(Bildungsgrad) %>%  # Count occurrences of each education level
  mutate(
    relative_frequency_formula = paste0(n, "/", total, " ≈ ", round(n / total, 3)),
    relative_frequency_value = round(n / total, 3)
  ) %>%
  rename(
    Ausprägung = Bildungsgrad,    # Rename column to match the table example
    absolute_hk = n               # Rename column for absolute frequency
  )

# Display the table using knitr::kable
kable(
  education_table,
  col.names = c("Ausprägung", "absolute Häufigkeiten hₖ", "relative Häufigkeiten fₖ (Formel)", "relative Häufigkeiten fₖ (Wert)"),
  caption = paste("Häufigkeitstabelle für Merkmal X = Bildungsgrad der Eltern (n =", total, ").")
)

#Slide 75
# Calculate the total number of observations
total <- nrow(df_19)

# Calculate frequency table with flexible total count
party_table <- df_19 %>%
  count(Partei) %>%  # Count occurrences of each education level
  mutate(
    relative_frequency_formula = paste0(n, "/", total, " ≈ ", round(n / total, 3)),
    relative_frequency_value = round(n / total, 3)
  ) %>%
  rename(
    Ausprägung = Partei,    # Rename column to match the table example
    absolute_hk = n         # Rename column for absolute frequency
  )

# Display the table using knitr::kable
kable(
  party_table,
  col.names = c("Ausprägung", "absolute Häufigkeiten hₖ", "relative Häufigkeiten fₖ (Formel)", "relative Häufigkeiten fₖ (Wert)"),
  caption = paste("Häufigkeitstabelle für Merkmal X = Parteipräferenz. (n =", total, ").")
)

#Nicht dargestellt hier: “darf nicht wählen” (2%), “werde nicht wählen” (21%!!). <- entfernen bei Bedarf

#Slide 76
# Calculate the total number of observations
total_22 <- nrow(df_22)
total_23 <- nrow(df_23)

# Calculate frequency table for 2022
party_table_22 <- df_22 %>%
  count(Partei) %>%
  mutate(
    relative_frequency_2022 = round(n / total_22, 3)
  ) %>%
  rename(
    Ausprägung = Partei,
    absolute_hk_2022 = n
  )

# Calculate frequency table for 2023
party_table_23 <- df_23 %>%
  count(Partei) %>%
  mutate(
    relative_frequency_2023 = round(n / total_23, 3)
  ) %>%
  rename(
    Ausprägung = Partei,
    absolute_hk_2023 = n
  )

# Merge the tables by the Ausprägung column
party_table_comparison <- full_join(party_table_22, party_table_23, by = "Ausprägung")

# Display the combined table using knitr::kable
kable(
  party_table_comparison,
  col.names = c("Ausprägung", "absolute Häufigkeiten 2022", "relative Häufigkeiten fₖ 2022", 
                "absolute Häufigkeiten 2023", "relative Häufigkeiten fₖ 2023"),
  caption = paste("Häufigkeitstabelle für Merkmal X = Parteipräferenz, Vergleich Kohorte 2022 (n =", total_22, 
                  ") vs. Kohorte 2023 (n =", total_23, ").")
)

# Define the values to exclude
exclude_values <- c("nicht waehlen")

df_22_filtered <- df_22 %>%
  filter(!Partei %in% exclude_values & !is.na(Partei))  

# Filter out specific entries and exclude NAs in the 2023 dataset
df_23_filtered <- df_23 %>%
  filter(!Partei %in% exclude_values & !is.na(Partei))  

# Calculate total after filtering
total_22 <- nrow(df_22_filtered)
total_23 <- nrow(df_23_filtered)

# Calculate frequency table for filtered 2022 data
party_table_22 <- df_22_filtered %>%
  count(Partei) %>%
  mutate(
    relative_frequency_2022 = round(n / total_22, 3)
  ) %>%
  rename(
    Ausprägung = Partei,
    absolute_hk_2022 = n
  )

# Calculate frequency table for filtered 2023 data
party_table_23 <- df_23_filtered %>%
  count(Partei) %>%
  mutate(
    relative_frequency_2023 = round(n / total_23, 3)
  ) %>%
  rename(
    Ausprägung = Partei,
    absolute_hk_2023 = n
  )

# Merge the tables by Ausprägung
party_table_comparison <- full_join(party_table_22, party_table_23, by = "Ausprägung")

# Display the combined table using knitr::kable
kable(
  party_table_comparison,
  col.names = c("Ausprägung", "absolute Häufigkeiten 2022", "relative Häufigkeiten fₖ 2022", 
                "absolute Häufigkeiten 2023", "relative Häufigkeiten fₖ 2023"),
  caption = paste("Häufigkeitstabelle für Merkmal X = Parteipräferenz, Vergleich Kohorte 2022 (n =", total_22, 
                  ") vs. Kohorte 2023 (n =", total_23, ").")
)

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

#Slide 80
barplot(table(df_19$Partnerschaft), col = "darkred", ylab = "Anzahl")
barplot(table(df_19$Partnerschaft), col = "darkred", ylab = "Anzahl", )

# Create a frequency table for the Partnerschaft column
partnerschaft_counts <- table(df_19$Partnerschaft)

# Sort the table in descending order
partnerschaft_counts <- sort(partnerschaft_counts, decreasing = TRUE)

# Create a bar plot with the sorted table
barplot(partnerschaft_counts, col = "darkred", ylab = "Anzahl", main = "Sind Sie in einer Partnerschaft?")


#Slide 81
barplot(table(df_19$Partnerschaft) / length(df_19$Partnerschaft), col = "darkred", ylab = "Relative Frequency")

# Create a frequency table for the Partnerschaft column and calculate relative frequencies
partnerschaft_relative_freq <- table(df_19$Partnerschaft) / length(df_19$Partnerschaft)

# Sort the relative frequencies in descending order
partnerschaft_relative_freq <- sort(partnerschaft_relative_freq, decreasing = TRUE)

# Create a bar plot with the sorted relative frequencies
barplot(partnerschaft_relative_freq, col = "darkred", ylab = "Relative Frequency", main = "Partnerschaft Relative Frequency (Descending Order)")









# Create a frequency table for the Partnerschaft column and calculate relative frequencies
partnerschaft_relative_freq <- table(df_19$Partnerschaft) / length(df_19$Partnerschaft)

# Sort the relative frequencies in descending order
partnerschaft_relative_freq <- sort(partnerschaft_relative_freq, decreasing = TRUE)

# Create a bar plot with the sorted relative frequencies and extend the y-axis to 0.6
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
# Sample data (replace with your actual data)
counts <- table(df_24$Kiffen)  # Get counts of each category

# Calculate percentages
percentages <- round(100 * counts / sum(counts), 1)
labels <- paste0(percentages, "%")  # Create labels with percentages

# Create a pie chart
pie(
  counts, 
  labels = labels,  # Use percentage labels
  col = c("skyblue", "orange"),  # Custom colors for each category
  main = "Schonmal gekifft?”"
)

# Add the legend to label each category with text
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

# Calculate percentages for labels
percentages <- round(100 * counts / sum(counts), 1)
labels <- paste0(percentages, "%")  # Add percentage signs to labels


pie(
  counts, 
  col = party_colors[names(counts)],  # Map colors to categories
  main = "Political Preference Distribution",
  cex = 0.5
)

exclude_values <- c("nicht waehlen", "BSW" , "eine andere Partei")

# Subset data to exclude multiple values
data_subset <- df_24$Partei[!(df_24$Partei %in% exclude_values)]

# Create a table of counts
table_data <- table(data_subset)

colors_to_use <- party_colors[names(data_subset)]

# Create the pie chart
pie(table_data, 
    col = colors_to_use, 
    main = "Pie Chart Excluding Specific Entries")

# Add a legend to clarify which color corresponds to each party
legend("topright", legend = names(table_data), fill = party_colors[names(counts)], cex = 1)






















#Slide 160
boxplot(df_24$Abi, bty = "n", ylab= "Abischnitt" , frame = FALSE)
# Sample code to generate a boxplot without an outer box
boxplot(df_24$Abi, col="gray", bty="n", ylab="Abiturschnitt", frame = FALSE)

# Create the boxplot with specific y-axis limits
boxplot(df_24$Abi, col = "gray", ylab = "Abiturschnitt", frame = FALSE, ylim = c(1.0, 4.0))


#Slide 162
# Assume 'data' is your dataframe, 'Category' is the categorical variable, and 'Value' is the numeric variable
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




# Create a contingency table of the two variables
table_data <- table(df_24$Partnerschaft, df_24$Alkohol)

# Generate the mosaic plot
mosaicplot(table_data, main = "Mosaic Plot of Alkohol vs. Partnerschaft", 
           xlab = "Partnerschaft", ylab = "Alkohol", col = TRUE)


#Slide 175
# Create bins for both variables
df_24$Entfernung_binned <- cut(df_24$Entfernung, breaks = 10)  # Adjust `breaks` as needed
df_24$Dauer_binned <- cut(df_24$Dauer, breaks = 10)            # Adjust `breaks` as needed

# Create a contingency table
table_data <- table(df_24$Entfernung_binned, df_24$Dauer_binned)

# Basic heatmap
heatmap(as.matrix(table_data), Rowv = NA, Colv = NA, 
        col = heat.colors(256), scale = "none",
        xlab = "Dauer (binned)", ylab = "Entfernung (binned)", 
        main = "Heatmap of Entfernung vs Dauer")


# Enhanced heatmap
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
# Assign symbols based on Geschlecht
point_symbol <- ifelse(df_24$Geschlecht == "maennlich", 4, 1)  # 4 for "x", 1 for "o"

# Plot with different symbols for Geschlecht
plot(df_24$Koerpergroesse, df_24$Schuhgroesse,
     pch = point_symbol, col = "blue", 
     xlab = "Körpergröße", ylab = "Schuhgröße", 
     main = "Scatterplot with Symbols as Third Variable")

# Add a legend to explain symbols
legend("topright", legend = c("Maennlich", "Weiblich"), 
       pch = c(4, 1), col = "blue", bty = "n")




# Scatter plot with point size representing the third variable
# Assign sizes based on Geschlecht
point_size <- ifelse(df_24$Geschlecht == "maennlich", 0.5, 1.5)

# Plot with adjusted point sizes
plot(df_24$Koerpergroesse, df_24$Schuhgroesse,
     cex = point_size, pch = 16, col = "blue", 
     xlab = "Körpergröße", ylab = "Schuhgröße", 
     main = "Scatterplot with Size as Third Variable")

# Note: `cex` controls the size of the points. The values of `z` should be scaled appropriately.


#Slide 184
# Define colors for each gender
colors <- ifelse(df_24$Geschlecht == "maennlich", "blue", "pink")

# Create the scatter plot with gender as color
plot(df_24$Koerpergroesse, df_24$Schuhgroesse, 
     col = colors, pch = 16, 
     xlab = "Körpergröße", ylab = "Schuhgröße", 
     main = "Scatterplot with 3 Variables (Color by Geschlecht)")

# Add a legend without a surrounding box
legend("topright", legend = c("Maennlich", "Weiblich"), 
       col = c("blue", "pink"), pch = 16, 
       bty = "n")  # Removes the box around the legend

# Define colors for each gender
colors <- ifelse(df_24$Geschlecht == "maennlich", "blue", "pink")

# Create the scatter plot with gender as color
plot(df_24$Koerpergroesse, df_24$Schuhgroesse, 
     col = colors, pch = 16, 
     xlab = "Körpergröße", ylab = "Schuhgröße", 
     main = "Scatterplot with 3 Variables (Color by Geschlecht)")

# Add a legend without a surrounding box
legend("topright", legend = c("Maennlich", "Weiblich"), 
       col = c("blue", "pink"), pch = 16)  


#Slide 185
scatterplot3d(df_24$Koerpergroesse, df_24$Schuhgroesse, df_24$Liegestuetze, 
              xlab = "Koerpergroesse", 
              ylab = "Schuhgroesse", 
              zlab = "Liegestuetze")

# Create a 3D scatter plot with filled black points
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
# Create a subset of df_24 with only the desired columns
df_subset <- df_24[, c("Liegestuetze", "Koerpergroesse", "Schuhgroesse")]

# Generate the scatterplot matrix
pairs(df_subset, 
      main = "Scatterplot Matrix of Koerpergroesse, Schuhgroesse, and Liegestuetze",
      pch = 16,  # Sets solid dots for points
      col = "blue")  # Sets color of the points
















#Slide 221
set.seed(123)  # Set a seed for reproducibility

# Number of rolls
n_rolls <- 1000

# Simulate rolling a fair six-sided die
rolls <- sample(1:6, n_rolls, replace = TRUE)

# Calculate cumulative proportion of even numbers
is_even <- rolls %% 2 == 0
cumulative_proportion <- cumsum(is_even) / (1:n_rolls)


# Plot the cumulative proportion of even numbers
plot(1:n_rolls, cumulative_proportion, type = "l", 
     xlab = "Anzahl Würfe n", 
     ylab = "rel. Häufigk. von geraden Zahlen", 
     main = "Relative Frequency of Even Numbers in Dice Rolls",
     frame = F)

# Add a reference line at 0.5 (the expected probability of rolling an even number)
abline(h = 0.5, col = "red", lty = 2)

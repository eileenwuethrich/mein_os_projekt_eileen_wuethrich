rm(list = ls())

## --------------------------------------
## Hypothetische Datenanalyse - Open Science Projekt
## Vorhersage von Rückfälligkeit mit Dark Triad Werte
## --------------------------------------


# Alles soll reproduzierbar sein.
set.seed(123)


# Datensätze einlsen
compas <- read.csv("~/Documents/Studium/MSc Psychologie/Herbstsemester 2025/Fakultär/Open Science/07 Termin/Mein OS Projekt/COMPAS-Datensatz/compas-scores-two-years-violent.csv")
dark_triad <- read.table("~/Documents/Studium/MSc Psychologie/Herbstsemester 2025/Fakultär/Open Science/07 Termin/Mein OS Projekt/Dark-Triad-Datensatz/data.csv", header = TRUE, sep = "", fill = TRUE)

View(compas)
View(dark_triad)


# Skalen berechnen (Mittelwerte über die 9 Items pro Skala)
dark_triad$machiavellianism <- rowMeans(dark_triad[, paste0("M", 1:9)], na.rm = TRUE)
dark_triad$narcissism       <- rowMeans(dark_triad[, paste0("N", 1:9)], na.rm = TRUE)
dark_triad$psychopathy      <- rowMeans(dark_triad[, paste0("P", 1:9)], na.rm = TRUE)


# Kontrolle: sind die neuen Spalten da und plausibel?
names(dark_triad)


#Relevante Spalten auswählen
compas_small <- compas[, c("id", "age", "sex", "is_recid")]
dark_triad_small <- dark_triad[, c("machiavellianism", "narcissism", "psychopathy")]


# Hypothetisches Kombinieren / Simulieren
# gleiche Länge sicherstellen – falls nötig darktriad zufällig samplen
dark_triad_sample <- dark_triad_small[sample(nrow(dark_triad_small), nrow(compas_small), replace = TRUE), ]
dark_triad_sample


# Neue kombinierte Daten
data <- cbind(compas_small, dark_triad_sample)
data


# Erste Exploration der Daten
summary(data)
table(data$sex)
hist(data$psychopathy, main="Verteilung Psychopathie-Werte")

hist(data$psychopathy,
     main = "Verteilung der Psychopathie-Werte",
     xlab = "Durchschnittlicher Psychopathie-Score",
     ylab = "Anzahl der Personen",
     col = "lightgray",
     border = "white")
abline(v = mean(data$psychopathy, na.rm = TRUE), 
       col = "red", lwd = 2)
text(mean(data$psychopathy, na.rm = TRUE) + 0.1, 1400, 
     "Mittelwert", col = "red", cex = 0.8)


# Erste einfache Analyse
# Logistische Regression: sagt Psychopathie Rückfälligkeit voraus?
model <- glm(is_recid ~ psychopathy + machiavellianism + narcissism + age + sex,
             data = data, 
             family = binomial)

summary(model)


# Fairness-Check (sehr einfach gehalten)

# Male vs. Female getrennt analysieren
model_m <- glm(is_recid ~ psychopathy, 
               data = subset(data, sex == "Male"), family = binomial)
model_f <- glm(is_recid ~ psychopathy, 
               data = subset(data, sex == "Female"), family = binomial)

cat("Effekt Psychopathie (Männer):\n")
print(summary(model_m)$coefficients)

cat("Effekt Psychopathie (Frauen):\n")
print(summary(model_f)$coefficients)


# Interpretation
# Wenn der Zusammenhang bei Männern und Frauen deutlich unterschiedlich ist, wäre das ein Hinweis auf mögliche Verzerrungen (Bias).

# Bei beiden Gruppen ist der Effekt positiv -> höhere Psychopathie-Werte gehen tendenziell mit höherem Rückfallrisiko einher.

#!! Aber: Beide p-Werte sind > .05, also nicht signifikant.
# -> Das heißt, statistisch gesehen kannst du nicht sicher sagen, dass Psychopathie mit Rückfälligkeit zusammenhängt.
# Ausserdem sind die Schätzungen (0.08 vs. 0.17) recht ähnlich –
# -> also keine starken Unterschiede zwischen Männern und Frauen -> kein Hinweis auf Bias.

# In meiner hypothetischen Analyse zeigte sich, dass Psychopathie tendenziell mit einem leicht höheren Rückfallrisiko verbunden war, sowohl bei Männern als auch bei Frauen. Der Effekt war jedoch in beiden Gruppen nicht signifikant. Das spricht dafür, dass das Modell in diesem Beispiel keine klare Verzerrung zwischen den Geschlechtern aufweist.


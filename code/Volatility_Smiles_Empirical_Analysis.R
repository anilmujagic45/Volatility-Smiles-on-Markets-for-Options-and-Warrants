# ------------------------------------------------------------------------------
# Name: Anil Mujagic
# Matrikelnummer: 2962191
# Datum: 17.11.2024
# Abgabedatum: 18.11.2024
# Beschreibung: Bachelorarbeit
# Thema: Volatility Smiles auf Märkten für Optionen und Optionsscheine - 
#         Eine empirische Analyse mit Daten der EUREX und EUWAX
# ------------------------------------------------------------------------------
#==============================================================================#
#==============================================================================#
#============================                      ============================#
#============================       ÜBERSICHT      ============================#
#============================                      ============================#
#==============================================================================#
#==============================================================================#
# 1. Pakete
# 2. Datenimport
# 3. Datenaufbereitung
# 4. Berechnung der Impliziten Volatilität
# 5. Weitere Datenaufbereitung
# 6. Volatility Smile Schätzungen
# 7. Plots, Korrelationsmatrix, Renditen
#==============================================================================#
#==============================================================================#

#==============================================================================#
#============================       1. Pakete      ============================#
#==============================================================================#

rm(list = ls())
library(rio)
library(tidyverse)
library(foreach)
library(doParallel)
library(derivmkts)
library(ggplot2)
library(xtable)
library(stargazer)
library(car)
library(caret)
library(readxl)
options(max.print = 15000000)



#==============================================================================#
#=========================       2. Datenimport      ==========================#
#==============================================================================#



# Erstellen eines temporären Verzeichnis & Unzip der Daten darauf
# Anschließend Erstellen einer Liste mit den benötigten Dateinamen

temp_dir <- tempdir()  
zip_file <- "C:\\Users\\anilm\\Downloads\\data.zip"  
unzip(zip_file, exdir = temp_dir)
csv_files <- list.files(
  temp_dir, 
  pattern = "eurex_tradingData|daxQuotes|euwax_TimesAndSales|euwax_tradingData|euwax_WKN.*\\.csv$", 
  full.names = TRUE, 
  recursive = TRUE
)


# Erstellen separater Listen, für das eigentliche Importieren

dax_files <- csv_files[grepl("daxQuotes", csv_files)]
eurex_files <- csv_files[grepl("eurex_tradingData", csv_files)]
euwax_wkn_files <- csv_files[grepl("euwax_WKN_", csv_files)]
euwax_timesandsales_files <- csv_files[grepl("euwax_TimesAndSales", csv_files)]
euwax_tradingdata_files <- csv_files[grepl("euwax_tradingData", csv_files)]


# Import der DAX Daten durch paralleles Abarbeiten mehrerer kleiner 
# Teilprobleme. Zusätzlich dazu nutzen wir eine Hilfsfunktion, 
# um die Daten aus der Liste dax_files einzulesen

chunks <- 12

dax_import <- function(dax_files, chunks) {
  # Festlegen der verfügbaren Kerne
  numCores <- detectCores() - 1
  registerDoParallel(numCores)
  
  # Länge der Dateiliste und Chunk-Größe bestimmen
  dax_files_length <- length(dax_files)
  chunksize <- ceiling(dax_files_length / chunks)
  
  # Parallele Verarbeitung der DAX-Dateien in festgelegten Chunks
  dax_data <- foreach(
    i = 1:chunks,
    .combine = bind_rows,
    .packages = c("dplyr", "purrr", "readr", "rio")
  ) %dopar% {
    
    # Hilfsfunktion zum Einlesen und Verarbeiten der Dateien
    read_and_process_csv <- function(file_path) {
      file_name <- basename(file_path)
      time_info <- sub(".*_(\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}).*", "\\1", file_name)
      time <- as.POSIXct(time_info, format = "%Y-%m-%d-%H-%M")
      
      if (grepl("daxQuotes", file_name)) {
        data <- import(file_path) %>%
          mutate(time = time)%>%
          select(time,Last, Open, High, Low)
          
        return(data)
      }
    }
    
    # Festlegen der Indexgrenzen für den aktuellen Chunk
    lower <- (i - 1) * chunksize + 1
    upper <- min(i * chunksize, dax_files_length)
    chunk_files <- dax_files[lower:upper]
    
    # Laden und Verarbeiten aller Dateien im aktuellen Chunk
    chunk_data <- chunk_files %>%
      set_names(nm = basename(chunk_files)) %>%
      map_df(read_and_process_csv, .id = "source_file")
    
    return(chunk_data)
  }
  
  # Beenden der parallelen Verarbeitung
  stopImplicitCluster()
  
  # Rückgabe der kombinierten DAX-Daten
  return(dax_data)
}



dax_csv <- dax_import(dax_files, chunks)  


summary(dax_csv)


# Selbes Prozedere für die EUREX-Daten


eurex_import <- function(eurex_files, chunks) {
  numCores <- detectCores() - 1
  registerDoParallel(numCores)
  
  eurex_files_length <- length(eurex_files)
  chunksize <- ceiling(eurex_files_length / chunks)
  
  eurex_data <- foreach(
    i = 1:chunks,
    .combine = bind_rows,
    .packages = c("dplyr", "purrr", "readr", "rio")
  ) %dopar% {
    
    read_and_process_csv <- function(file_path) {
      file_name <- basename(file_path)
      time_info <- sub(".*_(\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}).*", "\\1", file_name)
      time <- as.POSIXct(time_info, format = "%Y-%m-%d-%H-%M")
      
      if (grepl("eurex_tradingData", file_name)) {
        data <- import(file_path) %>%
          # Die einzelnen Spalten ins richtige Format bringen
          select(product, Traded.contracts, type, Strike.price, Last.price, ttm) %>%
          mutate(
            product = as.character(product),
            Traded.contracts = gsub(",", "", Traded.contracts),
            Traded.contracts = as.numeric(Traded.contracts),
            time = time,
            Strike.price = gsub(",", "", Strike.price),
            Strike.price = as.numeric(Strike.price),
            Last.price = gsub(",", "", Last.price),
            Last.price = as.numeric(Last.price),
            type = as.character(type)
          ) %>%
          rename(volumen = Traded.contracts, strike = Strike.price)
        
        return(data)
      }
    }
    
    lower <- (i - 1) * chunksize + 1
    upper <- min(i * chunksize, eurex_files_length)
    chunk_files <- eurex_files[lower:upper]
    
    chunk_data <- chunk_files %>%
      set_names(nm = basename(chunk_files)) %>%
      map_df(read_and_process_csv, .id = "source_file")
    
    return(chunk_data)
  }
  
  stopImplicitCluster()
  
  return(eurex_data)
}


eurex_csv <- eurex_import(eurex_files,chunks)

head(eurex_csv)
summary(eurex_csv)



# Idem für die EUWAX-Daten

# TimesAndSales import:

euwax_timesandsales_import <- function(euwax_timesandsales_files, chunks) {
  numCores <- detectCores() - 1
  registerDoParallel(numCores)
  
  euwax_timesandsales_files_length <- length(euwax_timesandsales_files)
  chunksize <- ceiling(euwax_timesandsales_files_length / chunks)
  
  euwax_data <- foreach(
    i = 1:chunks,
    .combine = bind_rows,
    .packages = c("dplyr", "purrr", "readr", "rio")
  ) %dopar% {
    
    read_and_process_csv <- function(file_path) {
      file_name <- basename(file_path)
      WKN_name <- sub(".*_(.*)\\.csv", "\\1", file_name)
      
      if (grepl("euwax_TimesAndSales", file_name)) {
        data <- import(file_path) %>%
          mutate(
            date = as.Date(Datum, format = "%d.%m.%Y"),
            WKN = WKN_name,
            volumen = as.numeric(`Volumen Einheiten`),
            Preis = as.numeric(gsub(",", ".", gsub("[^0-9,]", "", Preis))),
            time = format(as.POSIXct(Zeit, format = "%H:%M:%OS"), format = "%H:%M:%S")
          ) %>%
          select(date, time, WKN, Preis, volumen)
        
        return(data)
      }
    }
    
    lower <- (i - 1) * chunksize + 1
    upper <- min(i * chunksize, euwax_timesandsales_files_length)
    chunk_files <- euwax_timesandsales_files[lower:upper]
    
    chunk_data <- chunk_files %>%
      set_names(nm = basename(chunk_files)) %>%
      map_df(read_and_process_csv, .id = "source_file")
    
    return(chunk_data)
  }
  
  stopImplicitCluster()
  
  return(euwax_data)
}


euwax_timesandsales <- euwax_timesandsales_import(euwax_timesandsales_files, chunks)


head(euwax_timesandsales)
summary(euwax_timesandsales)




# TradingData import: 

euwax_tradingdata_import <- function(euwax_tradingdata_files, chunks) {
  numCores <- detectCores() - 1
  registerDoParallel(numCores)
  
  euwax_tradingdata_files_length <- length(euwax_tradingdata_files)
  chunksize <- ceiling(euwax_tradingdata_files_length / chunks)
  
  euwax_data <- foreach(
    i = 1:chunks, 
    .combine = bind_rows, 
    .packages = c("dplyr", "purrr", "readr", "rio")
  ) %dopar% {
    
    read_and_process_csv <- function(file_path) {
      file_name <- basename(file_path)
      time_info <- sub(".*_(\\d{4}-\\d{2}-\\d{2}).*", "\\1", file_name)
      date <- as.Date(time_info, format = "%Y-%m-%d")
      
      if (grepl("euwax_tradingData", file_name)) {
        data <- import(file_path) %>%
          mutate(
            date = as.Date(date),
            WKN = wkns,
            letzerBewertungstag = as.Date(letzerBewertungstag, format = "%d.%m.%Y"),
            strike = as.numeric(basispreis),
            IV_EUWAX = as.numeric(implVola_perc) / 100,
            ttm = as.numeric(letzerBewertungstag - date),
            type = optionsart,
            bezugsverhaeltnis = as.numeric(sub(" .*", "", bezugsverhaeltnis)) / 
              as.numeric(sub(".*: ", "", bezugsverhaeltnis))
          ) %>%
          select(
            date, WKN, emittent, type, strike, ttm, IV_EUWAX, bezugsverhaeltnis
          )
        
        return(data)
      }
    }
    
    lower <- (i - 1) * chunksize + 1
    upper <- min(i * chunksize, euwax_tradingdata_files_length)
    chunk_files <- euwax_tradingdata_files[lower:upper]
    
    chunk_data <- chunk_files %>%
      set_names(nm = basename(chunk_files)) %>%
      map_df(read_and_process_csv, .id = "source_file")
    
    return(chunk_data)
  }
  
  stopImplicitCluster()
  
  return(euwax_data)
}



euwax_tradingdata <- euwax_tradingdata_import(euwax_tradingdata_files, chunks)  
head(euwax_tradingdata)
summary(euwax_tradingdata)




# €STR und Rohstoff Import: 

str <- import("ECB Data Portal_20241031131808.csv")
gold_data <- read_excel("Rohstoffe.xlsx", sheet = "gold")
silver_data <- read_excel("Rohstoffe.xlsx", sheet = "silver")
oil_data <- read_excel("Rohstoffe.xlsx", sheet = "oil")
gas_data <- read_excel("Rohstoffe.xlsx", sheet = "gas")




#==============================================================================#
#======================       3. Datenaufbereitung      =======================#
#==============================================================================#


# Aufbereiten der Zinsraten:

str <- str %>%
  rename(
    interest_rate = "Euro short-term rate - Volume-weighted trimmed mean rate (EST.B.EU000A2X2A25.WT)"
  ) %>%
  mutate(
    date = as.Date(DATE),
    interest_rate = interest_rate / 100
  ) %>%
  select(date, interest_rate)


# DAX/EUREX/Interest_rate Join

eurex_dax_combined <- inner_join(dax_csv, eurex_csv, by = "time") %>%
  rename(
    DAX_Last = Last,
    EUREX_Last = Last.price
  ) %>%
  # NA entfernen, Numerics erzeugen, positives Handelsvolumen und ttm < 30, da
  # die EUWAX-Daten nur niedrige ttm aufweisen
  filter(
    !is.na(DAX_Last) & is.numeric(as.numeric(DAX_Last)),
    !is.na(EUREX_Last) & is.numeric(as.numeric(EUREX_Last)),
    !is.na(strike) & is.numeric(as.numeric(strike)),
    volumen > 0,
    ttm < 30
  ) %>%
  mutate(
    # Extraktion von Datum und Uhrzeit
    date = as.Date(time, format = "%d.%m.%Y"),
    time_only = format(as.POSIXct(time), format = "%H:%M:%S"),
    
    # Umwandlung des Optionstyps in Kleinbuchstaben
    type = tolower(type),
    
    # Berechnung des Moneyness
    moneyness = ifelse(type == "call", 
                       DAX_Last / strike, 
                       strike / DAX_Last),
    
    # Hinzufügen einer neuen Spalte 'market'
    market = "EUREX"
  ) %>%
  inner_join(str, by = "date") %>%
  
  # Auswahl der finalen Spalten
  select(
    date, time_only, interest_rate, DAX_Last, market, product, type, volumen, 
    EUREX_Last, strike, moneyness, ttm
  )



head(eurex_dax_combined)
summary(eurex_dax_combined)
write.csv(eurex_dax_combined, "eurex_dax_combined.csv")




# Da wir bei den EUWAX Daten sehr unterschieldiche Handelszeiten haben, nutzen
# wir hier durschnittliche DAX-Tageskurse


dax_euwax <- dax_csv %>%
  mutate(date = as.Date(time)) %>%   
  group_by(date) %>%                 
  summarise(average_dax = mean(Last, na.rm = TRUE))



# DAX/EUWAX/Interest_rate JoiN:


# Positives Handelsvolumen filtern
euwax_combined <- euwax_timesandsales %>%
  filter(volumen > 0) %>% 
  inner_join(euwax_tradingdata, by = c("WKN", "date")) %>% 
  inner_join(dax_euwax, by = "date") %>% 
  inner_join(str, by = "date") %>% 
  
  # Berechnung des Moneyness, Handelspreis mit Bezugsverhältnis multiplizieren
  
  mutate(
    moneyness = ifelse(type == "call", 
                       average_dax / strike, 
                       strike / average_dax),
    Preis_BSM = Preis * bezugsverhaeltnis,
    market = "EUWAX"
  ) %>%
  
  # Auswahl der finalen Spalten
  
  select(
    date, time, interest_rate, average_dax, market, WKN, emittent, type, 
    volumen, Preis, Preis_BSM, strike, moneyness, ttm
  )


head(euwax_combined)
summary(euwax_combined)


### Rohstoffe Datenaufbereitung:


# Hilfsfunktion zur Bereinigung der Rohstoffdaten

clean_data <- function(df) {
  df %>%
    mutate(
      date = as.Date(df$Date, format = "%d.%m.%Y"), 
      PX_LAST = as.numeric(gsub(",", ".", PX_LAST))
    ) %>%
    filter(!is.na(PX_LAST)) %>%
    arrange(date) %>%  
    filter(date < "2023-03-16") %>%  
    select(date, PX_LAST)
}

cleaned_gold <- clean_data(gold_data)
cleaned_silver <- clean_data(silver_data)
cleaned_oil <- clean_data(oil_data)
cleaned_gas <- clean_data(gas_data)


# Umbenennen und Filtern 

gold <- cleaned_gold %>%
  rename(Gold = PX_LAST)

silver <- cleaned_silver %>%
  rename(Silber = PX_LAST) 

oil <- cleaned_oil %>%
  rename(Erdöl = PX_LAST) 


gas <- cleaned_gas %>%
  rename(Erdgas = PX_LAST) 







#==============================================================================#
#============       4. Berechnung der Impliziten Volatilität      =============#
#==============================================================================#

# EUREX IV durch paralleles Abarbeiten mehrerer kleiner Teilprobleme.:


bsimpvol_eurex <- function(chunks) {
  eurex_dax_combined <- import(file = "eurex_dax_combined.csv")
  
  # Dividende auf 0, da der DAX eim Performance Index ist
  D <- 0
  numCores <- detectCores() - 1  
  registerDoParallel(numCores)   
  
  # Erstellen eines leeren Vektors
  IV_vector <- numeric(nrow(eurex_dax_combined)) 
  
  # Parallele Verarbeitung der Daten in Chunks
  IV_results <- foreach(chunk = 1:chunks, .combine = 'c', .packages = 'derivmkts') %dopar% {
    chunk_size <- ceiling(nrow(eurex_dax_combined) / chunks)
    lower <- (chunk - 1) * chunk_size + 1
    upper <- min(chunk * chunk_size, nrow(eurex_dax_combined))
    
    # Lokaler Vektor für die berechneten IV-Werte im aktuellen Chunk
    local_IV_vector <- numeric(upper - lower + 1)
    
    # Schleife zur Berechnung der IV 
    for (i in lower:upper) {
      tryCatch({
        row <- eurex_dax_combined[i, ]
        if (row$type == "call") {
          local_IV_vector[i - lower + 1] <- bscallimpvol(
            row$DAX_Last, row$strike, 
            r = row$interest_rate, 
            tt = row$ttm / 252, 
            D, price = row$EUREX_Last
          )
        } else if (row$type == "put") {
          local_IV_vector[i - lower + 1] <- bsputimpvol(
            row$DAX_Last, row$strike, 
            r = row$interest_rate, 
            tt = row$ttm / 252, 
            D, price = row$EUREX_Last
          )
        } else {
          local_IV_vector[i - lower + 1] <- NA  # Falls der Typ weder call noch put ist
        }
      }, error = function(e) {
        local_IV_vector[i - lower + 1] <- NA  # Bei Fehlern NA
      })
    }
    
    return(local_IV_vector)
  }
  
  stopImplicitCluster()  # Beenden der parallelen Verarbeitung
  
  # Kombinieren der Ergebnisse aus allen Chunks
  IV_vector <- unlist(IV_results)
  
  # Rückgabe des Vektors mit den berechneten IV
  return(IV_vector)
}


IV_EUREX <- bsimpvol_eurex(12)

length(IV_EUREX)
nrow(eurex_dax_combined) # stimmen überein




# EUWAX IV:

# Hier nutzen wir nur eine  for Schleife, da für die EUWAX, viel weniger
# Daten zu berechnen sind


IV_BSM <- numeric(nrow(euwax_combined))

for (i in 1:nrow(euwax_combined)){
  DAX <- euwax_combined$average_dax[i]
  strike <- euwax_combined$strike[i]
  price <- euwax_combined$Preis_BSM[i]
  ttm <- euwax_combined$ttm[i]
  r <- euwax_combined$interest_rate[i]
  D <- 0
  
  if (euwax_combined$type[i] == "call") {
    tryCatch({
      IV_BSM[i] <- bscallimpvol(DAX, strike, r, tt = ttm / 252, D, price)
    }, error = function(e) {
      IV_BSM[i] <- NA  
    })
  } else if (euwax_combined$type[i] == "put") {
    tryCatch({
      IV_BSM[i] <- bsputimpvol(DAX, strike, r, tt = ttm / 252, D, price)
    }, error = function(e) {
      IV_BSM[i] <- NA  
    })
  }
  else {
    IV_BSM[i] <- NA  
  }
}






#==============================================================================#
#==================       5. Weitere Datenaufbereitung      ===================#
#==============================================================================#



# IV hinzufügen und NA's entfernen

# EUREX:

nrow(eurex_dax_combined) #Um zu prüfen, wie viele NA's entfernt wurden

eurex_dax_combined <- eurex_dax_combined %>%
  mutate(IV = as.numeric(IV_EUREX),
         date = as.Date(date)) %>%   
  filter(!is.na(IV))

nrow(eurex_dax_combined)


# EUWAX:

nrow(euwax_combined)

euwax_combined <- euwax_combined %>%
  mutate(IV = as.numeric(IV_BSM)) %>%   
  filter(!is.na(IV))

nrow(euwax_combined)

# Summary:

summary(eurex_dax_combined)
summary(euwax_combined)




#Finaler Datensatz
# Filtern auf 2023-03-16, da die EUREX-Daten nur bis zu diesem Datum gehen
# Filtern der Moneyness-Werte auf den Bereich zwischen 0.7 und 1.3
# Neue Variable als Referenztag, für die Nord-Stream Anschläge
# Umwandlung der Variablen market, type und NordStream in einen Faktor, da Dummy-Variable
# Join mit den Rohstoffpreisen



final_data <- bind_rows(eurex_dax_combined, euwax_combined) %>%
  select(date, market, product, type, volumen, strike, moneyness, ttm,  emittent, IV, volumen)%>%
  filter(date < "2023-03-16", moneyness > 0.7, moneyness <= 1.3) %>%
  mutate(
    date = as.Date(date),
    market = as.factor(market),
    type = as.factor(type),
    NordStream = ifelse(date > "2022-09-26", "POST", "PRE"),
    NordStream = as.factor(NordStream)
  )%>% 
  inner_join(gas, by = "date") %>%
  inner_join(oil, by = "date") %>%
  inner_join(silver, by = "date") %>%
  inner_join(gold, by = "date")



 
head(final_data)
summary(final_data)

write.csv(final_data, "final_data.csv")


# Aufteilen in Datensatz mit kurzen und längeren Restlaufzeiten

table(euwax_combined$ttm)
table(eurex_dax_combined$ttm)


# 1. Datensatz ttm in (1:3)
# 2. Datensatz ttm in (17:21)


final_data_short <- final_data %>% filter(ttm %in% c(1:3))
final_data_long <- final_data %>% filter(ttm %in% (17:21))

# Anzahl der Call und Put Produkte für final_data_short

final_data_short %>%
  group_by(market, type) %>%  
  summarise(anzahl_optionen = n(), .groups = "drop") 

# Anzahl der Call und Put Produkte für final_data_long

final_data_long %>%
  group_by(market, type) %>%  
  summarise(anzahl_optionen = n(), .groups = "drop") 

# Produkte:

table(final_data_short$product)
table(final_data_long$product)

# Emittenten: 

table(final_data_short$emittent)
table(final_data_long$emittent)






# Die Regressionsanalyse wird in einem komplexeren Modell mit mehreren 
# Dummy-Variablen für die Datensätze final_data_short und _long durchgeführt.
# Um einen Überblick über die erwarteten Koeffezienten zu verschaffen, 
# führen wir die VS-Schätzungen für folgende Teildatensätze durch.


# Call & Puts zusammen für die jeweiligen Märkte und Laufzeiten

final_data_short_eurex <- final_data %>% filter(ttm %in% (1:3), market=="EUREX")
final_data_short_euwax <- final_data %>% filter(ttm %in% (1:3), market=="EUWAX")

final_data_long_eurex <- final_data %>% filter(ttm %in% (17:21), market == "EUREX")
final_data_long_euwax <- final_data %>% filter(ttm %in% (17:21), market == "EUWAX")

# Für kurze Laufzeiten (1-3) Calls & Puts isoliert

final_data_short_call<- final_data_short %>% filter(type == "call")
final_data_short_put <- final_data_short %>% filter(type == "put")

final_data_short_call_eurex <- final_data_short %>% filter(type == "call", market == "EUREX")
final_data_short_call_euwax <- final_data_short %>% filter(type == "call", market == "EUWAX")

final_data_short_put_eurex <- final_data_short %>% filter( type == "put",market == "EUREX")
final_data_short_put_euwax <- final_data_short %>% filter(type == "put",market == "EUWAX")

# Für lange Laufzeiten (17-21) Calls & Puts isoliert

final_data_long_call <- final_data_long %>% filter(type == "call")
final_data_long_put <- final_data_long %>% filter(type == "put")

final_data_long_call_eurex <- final_data_long %>% filter(type == "call", market == "EUREX")
final_data_long_call_euwax <- final_data_long %>% filter(type == "call", market == "EUWAX")

final_data_long_put_eurex <- final_data_long %>% filter(type == "put", market == "EUREX")
final_data_long_put_euwax <- final_data_long %>% filter(type == "put", market == "EUWAX")






#==============================================================================#
#================       6. Volatility Smile Schätzungen      ==================#
#==============================================================================#

 

# Kurze Laufzeiten (1-3)

model_short <- lm(IV ~ market*(moneyness + I(moneyness^2)),data = final_data_short)

model_short_call_eurex <- lm(IV ~ moneyness + I(moneyness^2), data = final_data_short_call_eurex)
model_short_call_euwax <- lm(IV ~ moneyness + I(moneyness^2), data = final_data_short_call_euwax)
model_short_call <- lm(IV ~ market*(moneyness + I(moneyness^2)), data = final_data_short_call)


model_short_put_eurex <- lm(IV ~ moneyness + I(moneyness^2), data = final_data_short_put_eurex)
model_short_put_euwax <- lm(IV ~ moneyness + I(moneyness^2), data = final_data_short_put_euwax)
model_short_put <- lm(IV ~  market*(moneyness + I(moneyness^2)), data = final_data_short_put)

model_short_type <- lm(IV ~ market*(moneyness + I(moneyness^2))*type,data = final_data_short)


# Lange Laufzeiten (17-21)

model_long <- lm(IV ~ market * (moneyness + I(moneyness^2)), data = final_data_long)


model_long_call_eurex <- lm(IV ~ moneyness + I(moneyness^2), data = final_data_long_call_eurex)
model_long_call_euwax <- lm(IV ~ moneyness + I(moneyness^2), data = final_data_long_call_euwax)
model_long_call <- lm(IV ~ market * (moneyness + I(moneyness^2)), data = final_data_long_call)



model_long_put_eurex <- lm(IV ~ moneyness + I(moneyness^2), data = final_data_long_put_eurex)
model_long_put_euwax <- lm(IV ~ moneyness + I(moneyness^2), data = final_data_long_put_euwax)
model_long_put <- lm(IV ~ market * (moneyness + I(moneyness^2)), data = final_data_long_put)

model_long_type <- lm(IV ~ market*(moneyness + I(moneyness^2))*type,data = final_data_long)




# Summary für Short Modelle

summary(model_short)

summary(model_short_call_eurex)
summary(model_short_call_euwax)
summary(model_short_call)

# Die Moneyness Koeffizienten der EUREX und EUWAX für Call-Optionen unterscheiden
# sich signifikant.
# Für die grafischen Darstellungen der VS nutzen wir in Abschnitt 7. die Modelle:
# model_short_call_eurex & model_short_call_euwax

summary(model_short_put_eurex)
summary(model_short_put_euwax)
summary(model_short_put)

# Auch für Put-Optionen, erkennen wir signifkante Unterschiede
# Für die grafischen Darstellungen der VS nutzen wir in Abschnitt 7. die Modelle:
# model_short_put_eurex & model_short_put_euwax



# Summary für Long Modelle

summary(model_long)

summary(model_long_call_eurex)
summary(model_long_call_euwax)
summary(model_long_call)

summary(model_long_put_eurex)
summary(model_long_put_euwax)
summary(model_long_put)


# Dieselben Unterschiede beobachten wir auch bei längeren Laufzeiten, 
# allerdings sind sie dort weniger ausgeprägt.


# Die folgenden beiden Modelle dienen als Grundlage für die Schätzungen in 
# Verbindung mit den Rohstoffdaten.


summary(model_short_type)
summary(model_long_type)



# Nun untersuchen wir diese beiden Modelle in Verbindung mit den Rohstoffpreisen 
# und unterschiedlichen Interaktionen

# 1. Modell: model_short_type mit den Rohstoffpreisen als Zusatz
model_short_rohstoffe1 <- lm(IV ~ market*(moneyness + I(moneyness^2))*type + Erdgas + Erdöl + Silber + Gold,
                             data = final_data_short)

# 2. Modell: Rohstoffpreise mit einer Dummy-Variable (market)
model_short_rohstoffe2 <- lm(IV ~ market*(moneyness + I(moneyness^2))*type + market:(Erdgas + Erdöl + Silber + Gold),
                             data = final_data_short)


# 3. Modell: Rohstoffpreise mit 2 Dummy-Variablen (market, NordStream)
# Hinzufügen von NordStream als Interaktionsvariable zu den Rohstoffpreisen
model_short_rohstoffe3 <- lm(IV ~ market*(moneyness + I(moneyness^2))*type + NordStream:market:(Erdgas + Erdöl + Silber + Gold),
                             data = final_data_short)

# 4. Modell: Rohstoffpreise mit 2 Dummy-Variablen (market, NordStream)
# und Hinzufügen der Interaktion zwischen NordStream und der Moneyness-Funktion
model_short_rohstoffe4 <- lm(IV ~ NordStream*market*(moneyness + I(moneyness^2))*type + NordStream:market:(Erdgas + Erdöl + Silber + Gold),
                             data = final_data_short)

# 5. Modell: Rohstoffpreise mit 2 Dummy-Variablen (market, NordStream)
# zusätzlich Moneyness-Variable bei den Rohstoffpreisen
model_short_rohstoffe5 <- lm(IV ~ market*(moneyness + I(moneyness^2))*type + moneyness:NordStream:market:(Erdgas + Erdöl + Silber + Gold),
                             data = final_data_short)

# 6. Modell: Erweiterung von Modell 5, wird NordStream auch auf 
# die Moneyness-Funktion angewendet
model_short_rohstoffe6 <- lm(IV ~ NordStream*market*(moneyness + I(moneyness^2))*type + moneyness:NordStream:market:(Erdgas + Erdöl + Silber + Gold),
                             data = final_data_short)





# Modellgüte-Kennzahlen:

summary(model_short_type)

summary(model_short_rohstoffe1)

# Das Hinzufügen der Rohstoffpreise verbessert die Modellgüte

summary(model_short_rohstoffe2)

# Die Differenzierung nach Markt führt zu einer minimalen Verbesserung der Modellgüte

summary(model_short_rohstoffe3)

# Die Berücksichtigung von NordStream verbessert das Modell signifikant

summary(model_short_rohstoffe4)

# Die Integration von NordStream in die Moneyness-Funktion verbessert ebenfalls das Modell

summary(model_short_rohstoffe5)

# Das Hinzufügen der Moneyness-Variable führt zu einer kleinen Erhöhung des R^2

summary(model_short_rohstoffe6)

# Die Erweiterung von Modell 5 um eine zusätzliche NordStream-Variable
# führt zu einer weiteren Verbesserung des R^2


# Modellvergleich anhand von AIC und BIC:


AIC(model_short_type)
AIC(model_short_rohstoffe1)
AIC(model_short_rohstoffe2)
AIC(model_short_rohstoffe3)
AIC(model_short_rohstoffe4)
AIC(model_short_rohstoffe5)
AIC(model_short_rohstoffe6)

BIC(model_short_rohstoffe1)
BIC(model_short_rohstoffe2)
BIC(model_short_rohstoffe3)
BIC(model_short_rohstoffe4)
BIC(model_short_rohstoffe5)
BIC(model_short_rohstoffe6)

# Die Anpassungsmaße (AIC und BIC) zeigen eine kontinuierliche Verbesserung mit 
# zunehmender Komplexität der Modelle. Sowohl die Einführung der Rohstoffe,
# als auch der NordStream Variable, verbessern das Modell. Allerdings führen die
# zusätzlichen Variablen zu Verzerrungen in den geschätzten Koeffizienten des 
# Volatility Smile. Als Kompromiss zwischen einer besseren Modellgüte 
# und der Vermeidung von Verzerrungen , wählen wir das 
# model_short_rohstoffe3 für unsere Analyse.



# Dieselben Schätzungen und Gütekontrollen für final_data_long

model_long_rohstoffe1 <- lm(IV ~ market*(moneyness + I(moneyness^2))*type + Erdgas + Erdöl + Silber + Gold,
                            data = final_data_long)

model_long_rohstoffe2 <- lm(IV ~ market*(moneyness + I(moneyness^2))*type + market:(Erdgas + Erdöl + Silber + Gold),
                            data = final_data_long)


model_long_rohstoffe3 <- lm(IV ~ market*(moneyness + I(moneyness^2))*type + NordStream:market:(Erdgas + Erdöl + Silber + Gold),
                            data = final_data_long)


model_long_rohstoffe4 <- lm(IV ~ NordStream*market*(moneyness + I(moneyness^2))*type + NordStream:market:(Erdgas + Erdöl + Silber + Gold),
                            data = final_data_long)

model_long_rohstoffe5 <- lm(IV ~ market*(moneyness + I(moneyness^2))*type + moneyness:NordStream:market:(Erdgas + Erdöl + Silber + Gold),
                            data = final_data_long)

model_long_rohstoffe6 <- lm(IV ~ NordStream*market*(moneyness + I(moneyness^2))*type + moneyness:NordStream:market:(Erdgas + Erdöl + Silber + Gold),
                            data = final_data_long)



summary(model_long_type)
summary(model_long_rohstoffe1)
summary(model_long_rohstoffe2)
summary(model_long_rohstoffe3)
summary(model_long_rohstoffe4)
summary(model_long_rohstoffe5)
summary(model_long_rohstoffe6)


AIC(model_long_rohstoffe1)
AIC(model_long_rohstoffe2)
AIC(model_long_rohstoffe3)
AIC(model_long_rohstoffe4)
AIC(model_long_rohstoffe5)
AIC(model_long_rohstoffe6)

BIC(model_long_rohstoffe1)
BIC(model_long_rohstoffe2)
BIC(model_long_rohstoffe3)
BIC(model_long_rohstoffe4)
BIC(model_long_rohstoffe5)
BIC(model_long_rohstoffe6)


# Cross-Validation:
# In diesem Abschnitt prüfen wir, ob eine Gefahr von Overfitting besteht:

control <- trainControl(method = "cv", number = 10)

formula <- IV ~ market * (moneyness + I(moneyness^2)) * type + 
  NordStream:market:(Erdgas + Erdöl + Silber + Gold)

# Führe die Cross-Validation durch
# Falls dieser Fehler vorkommt: 
# Fehler in summary.connection(connection) : ungültige Verbindung
# -> R-Studio neustarten, bei mir hat es dann funktioniert

cv_model_short <- train(formula, data = final_data_short, method = "lm", trControl = control)
cv_model_long <- train(formula, data = final_data_long, method = "lm", trControl = control)

print(cv_model_short)
print(cv_model_long)




# Das Modell zeigt gute CV-Ergebnisse, was auf eine gute Generalisierbarkeit 
# und keine Überanpassung hinweist. Der Short-Datensatz weist jedoch mehr 
# Ausreißer auf, was durch die höhere Volatilität bei kurzlaufenden Optionen 
# erklärt werden kann, da diese empfindlicher auf Marktreaktionen reagieren.




# Export der Modellergebnisse in LaTex

summary(model_short_rohstoffe3)
summary(model_long_rohstoffe3)

summary(model_short_put)


stargazer(model_short_rohstoffe3, model_long_rohstoffe3, 
          type = "latex", 
          title = "Modellzusammenfassung", 
          column.labels = c("Kurzfristiges Modell", "Langfristiges Modell"),
          out = "model_summary.tex",  
          no.space = TRUE)



# Da in unserem Regressionsergebnis die Koeffizienten der verschiedenen 
# Interaktionsterme nur den jeweiligen Unterschied zur Referenzkategorie 
# darstellen, listen wir für eine bessere Übersicht die aufaddierten und 
# eigentlichen Koeffizienten der Moneyness Funktion auf.


# Tabelle mit den Koeffizienten:

# Short:

coef_short <- summary(model_short_rohstoffe3)$coefficients

# Addierten Moneyness-Koeffizienten:

moneyness_short <- data.frame(
  Modell = "Short",
  Markt = c("EUREX", "EUWAX", "EUREX", "EUWAX"),
  Optionstyp = c("Call", "Call", "Put", "Put"),
  Lineare_Moneyness= c(
    coef_short["moneyness", "Estimate"],
    coef_short["moneyness", "Estimate"] + coef_short["marketEUWAX:moneyness", "Estimate"],
    coef_short["moneyness", "Estimate"] + coef_short["moneyness:typeput", "Estimate"],
    coef_short["moneyness", "Estimate"] + coef_short["marketEUWAX:moneyness", "Estimate"] +
      coef_short["moneyness:typeput", "Estimate"] + coef_short["marketEUWAX:moneyness:typeput", "Estimate"]
  ),
  Quadratische_Moneyness  = c(
    coef_short["I(moneyness^2)", "Estimate"],
    coef_short["I(moneyness^2)", "Estimate"] + coef_short["marketEUWAX:I(moneyness^2)", "Estimate"],
    coef_short["I(moneyness^2)", "Estimate"] + coef_short["I(moneyness^2):typeput", "Estimate"],
    coef_short["I(moneyness^2)", "Estimate"] + coef_short["marketEUWAX:I(moneyness^2)", "Estimate"] +
      coef_short["I(moneyness^2):typeput", "Estimate"] + coef_short["marketEUWAX:I(moneyness^2):typeput", "Estimate"]
  )
)


# Long:

coef_long <- summary(model_long_rohstoffe3)$coefficients

# Addierten Moneyness-Koeffizienten:

moneyness_long <- data.frame(
  Modell = "Long",
  Markt = c("EUREX", "EUWAX", "EUREX", "EUWAX"),
  Optionstyp = c("Call", "Call", "Put", "Put"),
  Lineare_Moneyness = c(
    coef_long["moneyness", "Estimate"],
    coef_long["moneyness", "Estimate"] + coef_long["marketEUWAX:moneyness", "Estimate"],
    coef_long["moneyness", "Estimate"] + coef_long["moneyness:typeput", "Estimate"],
    coef_long["moneyness", "Estimate"] + coef_long["marketEUWAX:moneyness", "Estimate"] +
      coef_long["moneyness:typeput", "Estimate"] + coef_long["marketEUWAX:moneyness:typeput", "Estimate"]
  ),
  Quadratische_Moneyness = c(
    coef_long["I(moneyness^2)", "Estimate"],
    coef_long["I(moneyness^2)", "Estimate"] + coef_long["marketEUWAX:I(moneyness^2)", "Estimate"],
    coef_long["I(moneyness^2)", "Estimate"] + coef_long["I(moneyness^2):typeput", "Estimate"],
    coef_long["I(moneyness^2)", "Estimate"] + coef_long["marketEUWAX:I(moneyness^2)", "Estimate"] +
      coef_long["I(moneyness^2):typeput", "Estimate"] + coef_long["marketEUWAX:I(moneyness^2):typeput", "Estimate"]
  )
)



# Zusätzlich dazu, berechnen wir auch die Minima der jewiligen VS

# Funktion zur Berechnung der Minimum

calculate_minimum <- function(a, b) {
  -a / (2 * b)
}

# Minimumstellen für short:

moneyness_short$Minimum <- mapply(
  calculate_minimum,
  moneyness_short$Lineare_Moneyness,
  moneyness_short$Quadratische_Moneyness
)

# Minimumstellen für long: 

moneyness_long$Minimum <- mapply(
  calculate_minimum,
  moneyness_long$Lineare_Moneyness,
  moneyness_long$Quadratische_Moneyness
)


# Bind:

moneyness_results_with_minima <- rbind(moneyness_short, moneyness_long)

print(moneyness_results_with_minima)


# Vergleich mit den eigentlichen Koeffizienten aus den präziseren Modellen:
 
summary(model_short_call_eurex)
summary(model_short_call_euwax)
summary(model_short_put_eurex)
summary(model_short_put_euwax)


# LaTeX Export der Koeffizienten-Tabelle:

xtable(moneyness_results_with_minima, 
       caption = "Moneyness-Terme und Minimumstellen für Short- und Long-Modelle", 
       label = "tab:moneyness_results")






#==============================================================================#
#===========       7. Plots, Korrelationsmatrix, Renditen      ================#
#==============================================================================#


#------------------------------------------------------------------------------

# DAX Plot:

# Anpassen des Datensatzes an unsere Stichprobe

dax_euwax_filtered <- dax_euwax %>% filter(date < "2023-03-16")

ggplot(data = dax_euwax_filtered, aes(x = date, y = average_dax)) +
  geom_line(color = "blue", size = 0.8) +
  labs(
    x = "Datum", 
    y = "Schlusskurs"
  ) +
  geom_vline(xintercept = as.Date("2022-09-26"), linetype = "dashed", color = "black", size = 1) +
  scale_x_date(
    date_breaks = "2 month",  
    date_labels = "%b %Y"     
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_line(color = "lightgray", size = 0.25), 
    legend.position = c(0.85, 0.95), 
    legend.justification = c("right", "top"), 
    legend.title = element_blank(),  
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    legend.text = element_text(size = 12)  
  )

ggsave("dax_plot.pdf", width = 10, height = 6, dpi = 300)


#-------------------------------------------------------------------------------


# Rohstoffe Plot:

rohstoffe_combined <- bind_rows(
  cleaned_gold %>% mutate(Rohstoff = "Gold"),
  cleaned_silver %>% mutate(Rohstoff = "Silber"),
  cleaned_oil %>% mutate(Rohstoff = "Öl"),
  cleaned_gas %>% mutate(Rohstoff = "Erdgas")
)

head(rohstoffe_combined)

ggplot(rohstoffe_combined, aes(x = date, y = PX_LAST, color = Rohstoff)) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.Date("2022-09-26"), linetype = "dashed", color = "black") +  # Vertikale Linie
  labs(
    x = "Datum",
    y = "Schlusskurs"
  ) +
  scale_color_manual(values = c("Erdöl" = "navy", "Erdgas" = "red", "Silber" = "gray", "Gold" = "gold")) +
  scale_y_continuous(breaks = seq(0, max(rohstoffe_combined$PX_LAST, na.rm = TRUE), by = 25)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_line(color = "lightgray", linewidth = 0.25),  # Hier size -> linewidth
    legend.position = c(0.95, 0.75), 
    legend.justification = c("right", "top"), 
    legend.title = element_blank(),  
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    legend.text = element_text(size = 12)  
  )

ggsave("Kursverlaeufe_Rohstoffe.pdf", width = 10, height = 6)


#------------------------------------------------------------------------------#


# €STR Plot:

str_percent <- str %>%
  mutate(interest_rate = interest_rate * 100)  


ggplot(str_percent, aes(x = date, y = interest_rate)) +
  geom_step(color = "blue", size = 1) +  
  geom_vline(xintercept = as.Date("2022-05-30"), linetype = "dashed", color = "black") +  
  geom_vline(xintercept = as.Date("2023-03-15"), linetype = "dashed", color = "black") +  
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.001, scale = 1),
                     expand = expansion(mult = c(0.05, 0.05)),  
                     limits = c(-1, NA)) +  
  scale_x_date(date_labels = "%d.%m.%Y",  # X-Achse im Format TT.MM.JJJJ
               breaks = c(as.Date("2020-05-30"), as.Date("2021-05-30"), as.Date("2022-05-30"), 
                          as.Date("2023-03-15"), as.Date("2024-05-30")),  
               expand = expansion(mult = c(0.05, 0.05))) +  
  labs(
    x = "Datum",
    y = "€STR (%)") +
  geom_rect(aes(xmin = as.Date("2022-05-30"), xmax = as.Date("2023-03-15"),
                ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.01) + 
  theme(
    panel.background = element_rect(fill = "white", color = "black"),  
    panel.grid.major = element_line(color = "gray", size = 0.5),  
    panel.grid.minor = element_line(color = "lightgray", size = 0.25), 
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    legend.text = element_text(size = 12)  
  )



ggsave("zinsentwicklung.pdf", width = 10, height = 6, dpi = 300)


#------------------------------------------------------------------------------#


# Da die Rohstoffmodelle einen Einfluss auf die Moneyness-Koeffizienten haben,
# nutzen wir zur grafischen Darstellung der Volatility Smiles diese zuvor
# berechneten Modelle. 


final_data_short_call$IV_pred <- predict(model_short_call, newdata = final_data_short_call)
final_data_short_put$IV_pred <- predict(model_short_put, newdata = final_data_short_put)
final_data_long_call$IV_pred <- predict(model_long_call, newdata = final_data_long_call)
final_data_long_put$IV_pred <- predict(model_long_put, newdata = final_data_long_put)




# Short-Call-Plot:

ggplot(final_data_short_call, aes(x = moneyness, y = IV_pred, color = market)) +
  geom_line(stat = "summary", fun = "mean", size = 1)+
  labs( 
    x = "Moneyness", 
    y = "Implizite volatilität (IV)") +
  ylim(0,1)+
  theme(
    panel.background = element_rect(fill = "white", color = "black"),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_line(color = "lightgray", size = 0.25), 
    legend.position = c(0.85, 0.95), 
    legend.justification = c("right", "top"), 
    legend.title = element_blank(),  
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    legend.text = element_text(size = 12)  
  )

ggsave("final_data_short_call.pdf", width = 10, height = 6, dpi = 300)




# Short-Put-Plot:


ggplot(final_data_short_put, aes(x = moneyness, y = IV_pred, color = market)) +
  geom_line(stat = "summary", fun = "mean", size = 1)+
  labs( 
    x = "Moneyness", 
    y = "Implizite volatilität (IV)") +
  ylim(0,1)+
  theme(
    panel.background = element_rect(fill = "white", color = "black"),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_line(color = "lightgray", size = 0.25), 
    legend.position = c(0.85, 0.95), 
    legend.justification = c("right", "top"), 
    legend.title = element_blank(),  
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    legend.text = element_text(size = 12)  
  )

ggsave("final_data_short_put.pdf", width = 10, height = 6, dpi = 300)


# Long-Call-Plot:


ggplot(final_data_long_call, aes(x = moneyness, y = IV_pred, color = market)) +
  geom_line(stat = "summary", fun = "mean", size = 1)+
  labs( 
    x = "Moneyness", 
    y = "Implizite volatilität (IV)") +
  ylim(0,1)+
  theme(
    panel.background = element_rect(fill = "white", color = "black"),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_line(color = "lightgray", size = 0.25), 
    legend.position = c(0.85, 0.95), 
    legend.justification = c("right", "top"), 
    legend.title = element_blank(),  
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    legend.text = element_text(size = 12)  
  )

ggsave("final_data_long_call.pdf", width = 10, height = 6, dpi = 300)


# Long-Put-Plot:


ggplot(final_data_long_put, aes(x = moneyness, y = IV_pred, color = market)) +
  geom_line(stat = "summary", fun = "mean", size = 1) +
  labs( 
    x = "Moneyness", 
    y = "Implizite volatilität (IV)"
  ) +
  ylim(0, 1) +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_line(color = "lightgray", size = 0.25), 
    legend.position = c(0.85, 0.95), 
    legend.justification = c("right", "top"), 
    legend.title = element_blank(),  
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    legend.text = element_text(size = 12)  
  )


ggsave("final_data_long_put.pdf", width = 10, height = 6, dpi = 300)


#------------------------------------------------------------------------------#
head(dax_euwax)

# Im letzten Teil berechnen noch die Renditen und Korrelationsmatrix der 
# einzelnen Basiswerte.


# Vorbereiten der DAX-Kurse:

dax_renditen <- dax_euwax %>% rename(PX_LAST = average_dax )


# Aufteilen in Vor- und Nachperiode der Nord-Stream-Anschläge:

Crash <- "2022-09-26"

pre_dax <- dax_renditen %>% filter(date< Crash)
post_dax <- dax_renditen %>% filter(date >= Crash)

cleaned_gas_pre <- cleaned_gas %>% filter(date < Crash)
cleaned_gas_post <- cleaned_gas %>% filter(date >= Crash)

cleaned_gold_pre <- cleaned_gold %>% filter(date < Crash)
cleaned_gold_post <- cleaned_gold %>% filter(date >= Crash)

cleaned_oil_pre <- cleaned_oil %>% filter(date < Crash)
cleaned_oil_post <- cleaned_oil %>% filter(date >= Crash)

cleaned_silver_pre <- cleaned_silver %>% filter(date < Crash)
cleaned_silver_post <- cleaned_silver %>% filter(date >= Crash)


# Funktion zur Berechnung der Renditen

rendite <- function(data) {
  data %>%
    # Tagesrenditen berechnen
    mutate(Renditen = (PX_LAST - lag(PX_LAST)) / lag(PX_LAST) * 100) %>%
    na.omit() %>% 
    # Gesamtrendite berechnen
    summarise(
      Gesamte_Rendite = prod(1 + Renditen / 100, na.rm = TRUE) - 1
    ) %>%
    mutate(Gesamte_Rendite = Gesamte_Rendite * 100) # Umwandlung in Prozent
}





gesamtrendite_gas <- rendite(cleaned_gas)
pre_rendite_gas <- rendite(cleaned_gas_pre)
post_rendite_gas <- rendite(cleaned_gas_post)

gesamtrendite_gold <- rendite(cleaned_gold)
pre_rendite_gold <- rendite(cleaned_gold_pre)
post_rendite_gold <- rendite(cleaned_gold_post)

gesamtrendite_oil <- rendite(cleaned_oil)
pre_rendite_oil <- rendite(cleaned_oil_pre)
post_rendite_oil <- rendite(cleaned_oil_post)

gesamtrendite_silver <- rendite(cleaned_silver)
pre_rendite_silver <- rendite(cleaned_silver_pre)
post_rendite_silver <- rendite(cleaned_silver_post)

gesamtrendite_dax <- rendite(dax_renditen)
pre_rendite_dax <- rendite(pre_dax)
post_rendite_dax <- rendite(post_dax)

renditen_df <- data.frame(
  Basiswert = c("Gas", "Gold", "Oil", "Silver", "DAX"),
  Pre_Rendite = c(pre_rendite_gas$Gesamte_Rendite, 
                  pre_rendite_gold$Gesamte_Rendite, 
                  pre_rendite_oil$Gesamte_Rendite, 
                  pre_rendite_silver$Gesamte_Rendite, 
                  pre_rendite_dax$Gesamte_Rendite),
  Post_Rendite = c(post_rendite_gas$Gesamte_Rendite, 
                   post_rendite_gold$Gesamte_Rendite, 
                   post_rendite_oil$Gesamte_Rendite, 
                   post_rendite_silver$Gesamte_Rendite, 
                   post_rendite_dax$Gesamte_Rendite),
  Gesamtrendite = c(gesamtrendite_gas$Gesamte_Rendite, 
                    gesamtrendite_gold$Gesamte_Rendite, 
                    gesamtrendite_oil$Gesamte_Rendite, 
                    gesamtrendite_silver$Gesamte_Rendite, 
                    gesamtrendite_dax$Gesamte_Rendite)
)


renditen_df
colnames(renditen_df) <- c("Basiswert", "Pre 22.09.2022", "Post 22.09.2022", "Gesamtrendite")

xtable(renditen_df)



###Korrelationsmatrix

# Daten nach dem gemeinsamen Datum zusammenführen
gemeinsame_daten <- reduce(
  list(
    cleaned_gas %>% select(date, Erdgas = PX_LAST),
    cleaned_oil %>% select(date, Erdöl = PX_LAST),
    cleaned_gold %>% select(date, Gold = PX_LAST),
    cleaned_silver %>% select(date, Silber = PX_LAST),
    dax_renditen %>% select(date, DAX = PX_LAST)
  ),
  full_join, by = "date"
)

# Entfernen von Zeilen mit fehlenden Werten
korrelationsdaten_kurse <- gemeinsame_daten %>%
  select(-date) %>%  
  na.omit()

# Korrelationsmatrix berechnen
korrelationsmatrix_kurse <- cor(korrelationsdaten_kurse)


print(korrelationsmatrix_kurse)

# LaTeX Export

xtable(
  korrelationsmatrix_kurse, 
  caption = "Korrelationsmatrix der Rohstoffe und des DAX",
  label = "tab:korrelationsmatrix"
)



#==============================================================================#
#=====================      Persönliche Anmerkungen     =======================#
#==============================================================================#


# An dieser Stelle möchte ich mich herzlich bei Ihnen, Herr Brunner, 
# für Ihre Unterstützung während des Schreibens meiner Bachelorarbeit bedanken.
# Besonders möchte ich mich auch für das Modul BW09 bedanken, das mir von einem 
# nicht vorhandenen Vorwissen ein fundiertes Basiswissen in der Datenanalyse 
# vermittelt hat. Dieses Wissen erwies sich vor allem in den letzten Wochen 
# als äußerst hilfreich. Ich konnte das von Ihnen gelernte in einem Praktikum
# erfolgreich in anderen Sprachen wie SAS und SQL anwenden.
# Ich hoffe, dass Ihnen mein Code gefallen hat 
# und bedanke mich nochmals für Ihre Unterstützung und Hilfe.


#==============================================================================#
#=============================      Ende     ==================================#
#==============================================================================#




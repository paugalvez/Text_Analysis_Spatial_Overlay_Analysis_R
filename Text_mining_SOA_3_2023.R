library(readxl) #get data out of an excel file (.xlsx) into R 
library(tidyverse) #data science library to transform and better present data


# The Webscraping code is located in a separate github file.
--------
  
#1 FILTERING THE DATABASES BY LOCATION = CATALONIA
  #1.1 First, we need to import the databases for activities and resource, format .xlsx files encoded as UTF-8, 

activitats <- read_excel("C:\\Users\\Pau\\PhD_Phase2\\databases\\Actius salut\\webscrapping\\FinalmesdescripAssets\\Act231222.xlsx")
recursos <- read_excel("C:\\Users\\Pau\\PhD_Phase2\\databases\\Actius salut\\webscrapping\\FinalmesdescripAssets\\Rec241222.xlsx")

  #1.2 Filter a subset of the databases with the Health Assets located in Catalonia
activitats_cat <- activitats %>% 
  filter(str_detect(location, "Catalunya"))
recursos_cat <- recursos %>% 
  filter(str_detect(locations, "Catalunya"))

#2 TEXT PROCESSING AND MINING

  #2.1 libraries
  
        library(tm) #Text Mining Package
        library(qdap) #Quantitative Discourse Analysis Package
        library(wordcloud) #visualization bag-of-words
        library(viridis) #colour palette for graphs
  
  #2.2. Creating the R corpus object. 

        #2.2.1 Merge the title and descriptions columns in each database
        activitats_cat$TD <- paste(activitats_cat$title, 
                                   activitats_cat$description)
        recursos_cat$TD <- paste(recursos_cat$title, 
                                 recursos_cat$description)
    
        #2.2.2 Delete former columns description and title
        activitats_cat <- activitats_cat[,-c(1,2)]
        recursos_cat <- recursos_cat[,-c(1,2)]
        
        #2.2.3 Convert new column TD to vectorsource and corpus
          #Activities
        Activities_TD <-  VectorSource(activitats_cat$TD) #Creates a vector containing the text content of title and descriptions
        Activities_TDc <- VCorpus(Activities_TD) #Converts the vector in a corpus object, which can be used for text analysis
    
          #Resources
        Resources_TD <- VectorSource(recursos_cat$TD)
        Resources_TDc <- VCorpus(Resources_TD)
        
  #2.3 Cleaning and preprocessing
        
        #2.3.1 Function to clean the corpus. When this function is called, 
        #it will remove whitespaces, punctuation, lowercase words, remove stop words, 
        #and convert words to their root form from the corpus of health asset titles and descriptions.
  
        clean_corpus <- function(corpus){         
        corpus <- tm_map(corpus, stripWhitespace) 
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removeWords, stopwords("catalan")) #select language of text
        corpus <- tm_map(corpus, stemDocument)
        return(corpus)
      }
        #2.3.2 Calling the function to clean the corpus of title and descriptions

        Activities_CTDc <-clean_corpus(Activities_TDc) 
        Resources_CTDc <- clean_corpus(Resources_TDc)

  #2.4 Text analysis
        
        #2.4.1 Creating the Document-Term Matrix for each database
          #Activities
          ActivitiesTD_dtm <- TermDocumentMatrix(Activities_CTDc) #creates a term-document matrix from the "Activities_CTDc" dataset, which contains text data.
          ActivitiesTD_dtmM <- as.matrix(ActivitiesTD_dtm)#converts the term-document matrix to a regular matrix.
          ActivitiesTD_sort <- sort(rowSums(ActivitiesTD_dtmM),decreasing=TRUE) #calculates the total frequency of each word (term) in the dataset, sorts them in decreasing order, and saves the result as "ActivitiesTD_sort".
          ActivitiesTD_df <- data.frame(word = names(ActivitiesTD_sort),freq=ActivitiesTD_sort) #creates a new data frame called "ActivitiesTD_df" that contains two columns: "word" and "freq". The "word" column contains the names of each word (term), and the "freq" column contains the frequency of each word (term).
          head(ActivitiesTD_df,500) #displays the first 500 rows of the "ActivitiesTD_df" data frame to show the most frequently occurring words.
        
          #Resources
          ResourcesTD_dtm <- TermDocumentMatrix(Resources_CTDc) #creates a term-document matrix from the "Activities_CTDc" dataset, which contains text data.
          ResourcesTD_dtmM <- as.matrix(ResourcesTD_dtm)#converts the term-document matrix to a regular matrix.
          ResourcesTD_sort <- sort(rowSums(ResourcesTD_dtmM),decreasing=TRUE) #calculates the total frequency of each word (term) in the dataset, sorts them in decreasing order, and saves the result as "ActivitiesTD_sort".
          ResourcesTD_df <- data.frame(word = names(ResourcesTD_sort),freq=ResourcesTD_sort) #creates a new data frame called "ActivitiesTD_df" that contains two columns: "word" and "freq". The "word" column contains the names of each word (term), and the "freq" column contains the frequency of each word (term).
          head(ResourcesTD_df,500) #displays the first 500 rows of the "ActivitiesTD_df" data frame to show the most frequently occurring words.
          
  #2.5 Creating topic-specific dictionaries
          
       #Activities database dictionaries (activitats_cat)
          #The following code adds a new column based on the new variable name (i.e., Gender, Age)
          #to a data frame called activitats_cat1,2,3" using the mutate() function from the "dplyr" 
          #package in R. The "NewVariable" column will be populated with a value based on whether certain keywords are present in another 
          #column called "TD", which contains the title an descriptions of the activities and resources. The keywords
          #are high-frequency words extracted from the text analysis.
          
          activitats_cat1 <- activitats_cat %>% mutate(
            Gender = case_when(    #new variable "Gender" in the activities database
              grepl("Mujer|Dona|Dones|Madre", populationTarget) ~ "Women",   #New categories and lists of words
              grepl("Hombre|Home", populationTarget) ~ "Men",
              grepl("Qualsevol|Cualquiera|cualquiera", populationTarget) ~ "Any",
              grepl("lgtb", populationTarget) ~ "Non Binary",
            ))
          activitats_cat1$Gender <- activitats_cat1$Gender %>% replace_na('Any') #replacing NA for most prevalent category
          
          activitats_cat2 <- activitats_cat1 %>% mutate(
            Age = case_when(       #new variable "Age" in the activities database
              grepl("65|60|jubilad", 
                    populationTarget) ~ "Older adults (>60)",   #New categories and lists of words
              grepl("infants|infantil|Infantil", 
                    populationTarget) ~ "Infants (0-11)",
              grepl("niño| niños/as", 
                    description) ~ "Infants (0-11)",
              grepl("escolar|escolares|adolescente", 
                    populationTarget) ~ "Minors unspecified (<18)",
              grepl("general|General", 
                    populationTarget) ~ "General Population (all ages)",
              grepl("adulta|Adulta", 
                    populationTarget) ~ "Adults unspecified (>18)",
              grepl("jóvenes|joves|Joves", 
                    populationTarget) ~ "Youth (12-29)",
              grepl("entre", 
                    populationTarget) ~ " Adults (29-59)",))
          
          activitats_cat2$Age <- activitats_cat2$Age  %>%  replace_na('General Population (all ages)')  #replacing NA for most prevalent category
          
          activitats_cat3 <- activitats_cat2 %>% mutate(
            Format = case_when(    #new variable "Format" in the activities database
              grepl("domicilio| casa| personaliza",         #New categories and lists of words
                    TD) ~ "Individual",))
           activitats_cat3$Format <- activitats_cat3$Format %>% replace_na('Group') #replacing NA for most prevalent category
          
          activitats_cat4 <- activitats_cat3 %>% mutate(
            Focus = case_when(    #new variable "Focus" in the activities database
              grepl("aisla| sola|solo|                         
          socializ|sociabi|Solitud|soledad",     #New categories and lists of words
                    TD) ~ "Direct",
              grepl("aisla| sola|solo|
          socializ|sociabi|Solitud|soledad",  
                    populationTarget) ~ "Direct",))
          activitats_cat4$Focus <- activitats_cat4$Focus  %>%  replace_na('Indirect')  #replacing NA for most prevalent category
          
          activitats_cat5 <- activitats_cat4 %>% mutate(
            Vulnerable = case_when(   #new variable "Vulnerable Population" in the activities database
              grepl("cuidador|cuidadores|cuidadoras",    #New categories and lists of words
                    TD) ~ "Caregivers",
              grepl("diabetes|Diabetes|caídas|crónicos|
          hipertens|Cáncer|cáncer|síndromes|
          artrosis|anticoagulantes|oncol|fibromialgia", 
                    TD) ~ "Physical diseases",
              grepl("diabetes|Diabetes|caídas|crónicos|
          hipertens|Cáncer|cáncer|síndromes|
          artrosis|anticoagulantes|oncol|fibromialgia", 
                    populationTarget) ~ "Physical diseases",
              grepl("drogas|adicta|parenteral|fuma|alcoh", 
                    TD) ~ "Addictions",
              grepl("deterioro cognitivo|cognoscitiva|mental|
          cognitiva|ansiedad|Ansiedad|alzheimer|demencia", 
                    TD) ~ "Mental health issues",
              grepl("deterioro cognitivo|cognoscitiva|mental|
          cognitiva|ansiedad|Ansiedad|alzheimer|demencia", 
                    populationTarget) ~ "Mental diseases",
              grepl("inmig|inmigrante|inmigradas|
          refugiadas|refugiados", 
                    populationTarget) ~ "Migrants",
              grepl("judiciales|discrimin|estigma|vulnerab", 
                    populationTarget) ~ "Risk social exclusion",
            ))
          activitats_cat5$Vulnerable <- Activities5$Vulnerable %>% replace_na('All') #replacing NA for most prevalent category
          
          activitats_cat6 <- activitats_cat5 %>% mutate(
            Type = case_when(    #new variable "Activity_type" in the activities database
              grepl("físic|Esport|esport|Ejercicio|ejercicio|PAFES|  
          caminar|sedentarismo|marcha|yoga|pilates|
          exercici|Gimnasia|gimnasia|
          Camina|Yoga|Paseos|deportiv|andar|
          Carrera|carrera|postura|Recorrido|entrenamiento|gimnasio|petanca|
          Piscina|piscina|nataci|atletism|acuática",    #New categories and lists of words
                    TD) ~ "Physical activity",
              grepl("masas|visionado|visible|visibilizar|radio|
          medios", 
                    TD) ~ "Awareness campaigns",
              grepl("Atención Primaria|atención Primaria|enfermer|hospital|
          Farmac|diet|nutri|autocuidado|profesionales|servicio|Masaje|masaje|
          consulta|pélvico|sanitar|Lactancia|lactancia|fisioter|crónic|fuma|
          enfermo|paciente|cáncer|incapacidad|laboral|ELA|jurídica|legal|Ley|
          alimentación", 
                    TD) ~ "Health and social care",
              grepl("Encuentro|encuentro|compartir|Compartir|social|Social|
          aisla|soled", 
                    TD) ~ "Social facilitation",
              grepl("autopercepción|cognitiv|emocional|memoria|
           terapeutas|psíquic|medit|ansiedad|psicoterapia|
           psicoonc|estrés|cognitiv|psicología", 
                    TD) ~ "Psychological therapies",
            ))
          activitats_cat6$Type <- activitats_cat6$Type %>% replace_na('Leisure and skill development') #replacing NA for most prevalent category
          
          write_xlsx(activitats_cat6,"C:\\Users\\Pau\\PhD_Phase2\\databases\\Accuracytest.xlsx") 
          #Exporting the new database in .xlsx format for manual verification of the new categories
          
          ----------------------------------------------
            
            #Resources database dictionaries (resources_cat) (same process as above)
            
            recursos_cat1 <- recursos_cat %>% mutate(
            Type = case_when(
              grepl("Biblioteca|Biblioteques", TD) ~ "Public Library",
              grepl("Platja|Playa|Ruta|Parc|Rutes", TD) ~ "Municipal natural and green space", 
              grepl("Creu Roja|CREU ROJA|voluntariat", TD) ~ "Charitable & voluntary organization",
              grepl("voluntariat|VOLUNTARIAT", TD) ~ "Charitable & voluntary organization",
              grepl("CARITAS|Caritas|Càritas", TD) ~ "Faith-based organisation",
              grepl("Ateneu|Casal|Cívic|cívic|Societat|Espai|Casa & Cultura", TD) ~ "Civic center",
              grepl("Cívic|cívic|Espai", TD) ~ "Civic center",
              grepl("estudis|Esplai|esplai|Escola|Agrupament|escola|infants|
                      Educació|educació", TD) ~ "Education institution",
              grepl("estudis|Esplai|esplai|Escola|Agrupament|escola|infants|
                      Educació|educació", TD) ~ "Education institution",
              grepl("Mercat", TD) ~ "Grocery space", 
              grepl("Salut|Sanitari|Generalitat|salut|CAP|PSICOEDUCATIUS|Alletament|postpart
                      |Farmacia|ASSIR|Consultori", TD) ~ "Health institution",
              grepl("Teatre|Bitlles|Museu|museu", TD) ~ "Cultural institution",
              grepl("Associació|Coral|ASSOCIACIÓ|ASSOSIACIÓ|Gegants", TD) ~ "Leisure & cultural association",
              grepl("excursionista|associació|danses|Batucada|batucada|gegants", TD) ~ "Leisure & cultural association",
              grepl("Veïns|Veins", TD) ~ "Neighbourhood association", 
              grepl("AFA |AMPA|Ampa", TD) ~ "School association",
              grepl("LABORAL|laboral|laborals|estrangeria|habitatge|Social|
                      Ocupacional", TD) ~ "Social welfare institution",
              grepl("ocupacional|Ocupació|vulnerable", TD) ~ "Social welfare institution",
              grepl("d'escacs|Atletisme|Bàsquet|Esportiu|
                      Natació|natació|Piscines|Piscina|futbol", TD) ~ "Sports institution",
              grepl("futbol|d'esports|esport|esportiu|esportiva|gimnàs|zumba|pilates|yoga|
                      marcials", 
                    TD) ~ "Sports institution",
              grepl("AECC", TD) ~ "Patient advocacy group",
              grepl("Associació & pacients", TD) ~ "Patient advocacy group",
            ))
          
          recursos_cat1$Type <- recursos_cat1$Type %>% replace_na('Leisure & cultural association')
         
          recursos_cat2 <- recursos_cat1 %>% mutate(
            Vulnerable = case_when(
              grepl("Cáritas|Creu Roja", 
                    title) ~ "Risk social exclusion",
              grepl("vulnerable|vulnerables|atur|exclusió|
          discriminació|discriminacions|racisme|
          habitatge", 
                    description) ~ "Risk social exclusion",
              grepl("mental|Mental|mentals|ansietat|depressió|
          estrès|demències|demència", 
                    description) ~ "Mental diseases ",
              grepl("nouvingudes|estrangeria|migrants|refugiats", 
                    description) ~ "Migrants",
              grepl("drogues|droga", description) ~ "Addictions",
              grepl("Diabetes|càncer|crònica|cròniques|
          hipertens|oncològic", 
                    description) ~ "Physical diseases",
              grepl("cuidador|cuidadores|cuidadors", 
                    description) ~ "Caregivers",
            ))
          recursos_cat2$Vulnerable <-  recursos_cat2$Vulnerable %>% replace_na('All')
          
          recursos_cat3 <- recursos_cat2 %>% mutate(
            Type_activity = case_when(
              grepl("Municipal natural and green space", Type) ~ "environment",
              grepl("Public Library|Leisure|Education|Civic|Recreation|School|Sports", Type) ~ "cultural",
              grepl("cultural|educació|Associació|associació|escola|agrupació|Grup|grup", description) ~ "cultural",
              grepl("Charitable|Church", Type) ~ "multiple social activity",
              grepl("atur|laboral|ocupacional|feina|ocupació", description) ~ "economic",
              grepl("Healthcare|Sports|Grocery", Type) ~ "health",
              grepl("embaràs|claudicació", description) ~ "health",
              grepl("sanitari|Sanitari", title) ~ "health",
              grepl("nouvingudes|estrangeria|migrants|refugiats", 
                    description) ~ "politic",
            ))
          recursos_cat3$Type_activity <-  recursos_cat3$Type_activity %>% replace_na('cultural')
          
          recursos_cat4 <- recursos_cat3 %>% mutate(
            Gender = case_when(
              grepl("Dona|Dones|Mare|Mares|Mestress", title) ~ "Women",
              grepl("Dona|Dones|Mare|Mares|Mestress", description) ~ "Women",
              grepl("Home|Homes", title) ~ "Men",
              grepl("Home|Homes", description) ~ "Men",
              grepl("LGTB", description) ~ "Non Binary",
              grepl("LGTB", title) ~ "Non Binary",
            ))
          recursos_cat4$Gender <- recursos_cat4$Gender %>% replace_na('Any')
          
          recursos_cat5 <- recursos_cat4 %>% mutate(
            Age = case_when(
              grepl("mares|pares|embarassades|
          ocupacional|Ocupacional|laboral|Ampa|AMPA|cuidador|cuidadora", 
                    description)  ~ "Adults unspecified (>18)",
              grepl("Jubilats|Pensionistes|jubilats|pensionistes", 
                    title) ~ "Older adults (>60)",
              grepl("Jubilats|Pensionistes|jubilats|pensionistes", 
                    description) ~ "Older adults (>60)",
              grepl("Esplai|esplai|Escola|Agrupament|escola|infants|
          Educació|educació", title) ~ "Minors unspecified (<18)",
              grepl("Esplai|esplai|Escola|Agrupament|escola|infants|
          Educació|educació", description) ~ "Minors unspecified (<18)",
              ))
          recursos_cat5$Age <- recursos_cat5$Age %>% replace_na('General Population (all ages)')
         
          recursos_cat6 <- recursos_cat5 %>% mutate(
            Focus = case_when(
              grepl("aisla|socializ|sociabi|Solitud|soledad", 
                    description) ~ "Direct",
            ))
          recursos_cat6$Focus <- recursos_cat6$Focus  %>%  replace_na('Indirect')
          
          write_xlsx(Recursos6,"C:\\Users\\Pau\\PhD_Phase2\\databases\\Recursos4.xlsx")
          #Exporting the new database in .xlsx format for manual verification of the new categories

          
#3 SPATIAL OVERLAY ANALYSIS
          #Libraries
          library("ggplot2")                
          library("sf")
          library("magrittr")
          
          #3.1 Importing the databases manually verified by two researchers, with columns longitude and latitude for each address
          
          Activities_Coords<- read_xlsx("G:\\My Drive\\PhD- UOFT\\PhD Project\\Project-COMSalut-Angeli Shared Folder\\Phase 2- Compartit amb Angeli\\Analisis descriptivo (Tabla 1 y 2)\\HActivitatsws_RevisadoFinal_v2.xlsx")
          Resources_Coords<- read_xlsx("G:\\My Drive\\PhD- UOFT\\PhD Project\\Project-COMSalut-Angeli Shared Folder\\Phase 2- Compartit amb Angeli\\Analisis descriptivo (Tabla 1 y 2)\\HResourcesFinalJan23.xlsx")
          
          #3.2 Filtering databases: excluding health assets based on eligibility criteria
          
          Activities_Coords <- Activities_Coords %>% 
            filter(!str_detect(Age,"59|Infants|Youth|Minors|Standarize"))
          
          Activities_Coords <- Activities_Coords %>% 
            filter(str_detect(Include,"Yes"))
          
          Resources_Coords <- Resources_Coords %>% 
            filter(!str_detect(Age1,"59|Infants|Youth|Minors|minors|youth|Social"))
          
          #3.3 Importing the multipolygon map layer including local jurisdictions (BHAs)
          
          ABS <- st_read("C:\\Users\\Pau\\PhD_Phase2\\ABS_SS_Municipi\\ABS_Mapfolder\\ABS_2022.shp") #MapBHAs
          
          #3.4 Generating spatial point data (coordintes for each health asset)
          Act_Cord <- Activities_Coords[,c("Longitude","Latitude")] #creating a dataframe with just longitude and latitude columns
          Act_Cord$Longitude <- as.numeric(Act_Cord$Longitude) #Converting the columns to numeric data
          Act_Cord$Latitude <- as.numeric(Act_Cord$Latitude)
          
          #3.5 Converting spatial data to a common CRS and Spatial overlay analysis
          
          ABS_CRS <- st_transform(ABS, 2163) #converting map layer to a 2163 CRS ()
          st_crs(ABS_CRS) #Checking the new CRS (EPSG:2163)
          
          #The function below assigns a value to a new column called Code_BHA in the Act_Cord 
          #data frame. The function loops over each row of the Act_Cord dataframe cointaining
          #the longitude and latitude for each health asset of type activity, performing the following:
          
          Act_Cord$Code_BHA <- apply(Act_Cord, 1, function(row) {
            coords <- as.data.frame(matrix(row, nrow = 1,   #Extract the Longitude and Latitude values from each row and convert them to a data.frame object called coords.
                                           dimnames = list("", c("Longitude", "Latitude"))))
            pnt_sf <- st_transform(st_sfc(st_point(row),crs = 4326), 2163)  #Create a POINT object using the Longitude and Latitude values, transform it to the EPSG 2163 CRS, and assign it to a variable called pnt_sf.
            ABS_CRS[which(st_intersects(pnt_sf, ABS_CRS, sparse = FALSE)), ]$CODIABSn  #Use the st_intersects() function to identify which polygons in the ABS_CRS data frame intersect with the pnt_sf object.
          }) #Use the which() function to extract the Code of the BHA, store it the new Code_BHA column.
         
          #3.6 Same process repeated for the resoures database:
          
          Rec_Cord <- Resources_Coords[,c("Longitude","Latitude")] #creating a dataframe with just longitude and latitude columns
          Rec_Cord$Longitude <- as.numeric(Rec_Cord$Longitude) #Converting the columns to numeric data
          Rec_Cord$Latitude <- as.numeric(Rec_Cord$Latitude)
          
          Rec_Cord$Code_BHA <- apply(Rec_Cord, 1, function(row) {
            coords <- as.data.frame(matrix(row, nrow = 1,   #Extract the Longitude and Latitude values from each row and convert them to a data.frame object called coords.
                                           dimnames = list("", c("Longitude", "Latitude"))))
            pnt_sf <- st_transform(st_sfc(st_point(row),crs = 4326), 2163)  #Create a POINT object using the Longitude and Latitude values, transform it to the EPSG 2163 CRS, and assign it to a variable called pnt_sf.
            ABS_CRS[which(st_intersects(pnt_sf, ABS_CRS, sparse = FALSE)), ]$CODIABSn  #Use the st_intersects() function to identify which polygons in the ABS_CRS data frame intersect with the pnt_sf object.
          }) #Use the which() function to extract the Code of the BHA from the column CODIABSn of the map layer dataframe, store it the new Code_BHA column.
          
        
          #3.6 Spatial data visualization
          
          #The two layers of spatial data; a first layer of polygonal data and asecond layer of vector of geographic point data 
          #can be plotted to observe patterns.
          #First, we created a new column in the activities database with four implementation periods
          #depending on when the health assets where registered
          
          Activities_Coords <- Activities_Coords %>% mutate(
            Period= case_when(
              updatedOn <= ymd("2018-01-01") ~ "Period 1",
              updatedOn > ymd("2018-01-02") & updatedOn <= ymd("2020-02-29") ~ "Period 2",
              updatedOn > ymd("2020-03-01") & updatedOn <= ymd("2021-05-31") ~ "Period 3",
              updatedOn > ymd("2021-06-01") ~ "Period 4",
            ))
          
          Resources_Coords <- Resources_Coords %>% mutate(
            Period= case_when(
              updatedOn <= ymd("2018-01-01") ~ "Period 1",
              updatedOn > ymd("2018-01-02") & updatedOn <= ymd("2020-02-29") ~ "Period 2",
              updatedOn > ymd("2020-03-01") & updatedOn <= ymd("2021-05-31") ~ "Period 3",
              updatedOn > ymd("2021-06-01") ~ "Period 4",
            ))
          
          #Subset database, just including coordinates and period 
          Act_coords <- activitats[,c("Longitude","Latitude","Period")]
          Act_coords <- Act_coord[Act_coord$Longitude > 0, ]  #removing erroneous coordinates
          
          Rec_coord <- recursos[,c("Longitude","Latitude","Period")]
          Rec_coord <- Rec_coord[Rec_coord$Longitude > 0, ]   #removing erroneous coordinates
          
          Act_coord <- Act_coord %>% drop_na()   #removing NAs (non-identified coordinates)
          Rec_coord <- Rec_coord %>% drop_na()
          
          #Setting the CRS of the spatial POINT data (vector of coordinates health assets)
          Act_coord1 <- st_as_sf(Act_coord, coords = c("Longitude", "Latitude"), crs = "+proj=longlat")
          Rec_coord1 <- st_as_sf(Rec_coord, coords = c("Longitude", "Latitude"), crs = "+proj=longlat")
          
          #Setting the CRS of the map layer to the same coordinate system as the Point data 
          ABS <- st_transform(ABS, crs = "+proj=longlat")
          st_crs(ABS)
          
          #Polotting the maps overlapping the two layers, showing the four periods.
          MAP_Act <- ggplot()+
            geom_sf(data = ABS, fill = NA, color=alpha("darkgrey",0.6))+
            geom_sf (data = Act_coord1, aes(color = "Activities"),  size = 1.5, alpha = 0.5)+
            scale_y_continuous(breaks = c(40.5,41.5,42.5))+
            scale_x_continuous(breaks = c(1.0,2.0,3.0)) +
            scale_color_manual("", values=c("darkgreen"))+
            theme_classic()+
            labs(title="")+
            facet_wrap("Period")
          
          MAP_Res <- ggplot()+
            geom_sf(data = ABS, fill = NA, color=alpha("darkgrey",0.6))+
            geom_sf (data = Rec_coord1, aes(color = "Resources"),  size = 1.5, alpha = 0.5)+
            scale_y_continuous(breaks = c(40.5,41.5,42.5))+
            scale_x_continuous(breaks = c(1.0,2.0,3.0)) +
            scale_color_manual("", values=c("blue"))+
            theme_classic()+
            labs(title="")+
            facet_wrap("Period")
          
      
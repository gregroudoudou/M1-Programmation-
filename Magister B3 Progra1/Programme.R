install.packages("rvest")
install.packages("httr")
install.packages("xml2")

library(rvest)
library(httr)

scrape_wikipedia <- function(page_title) {
  # Construire l'URL de Wikipédia
  url <- paste0("https://fr.wikipedia.org/wiki/", gsub(" ", "_", page_title))
  
  # Envoyer une requête avec un User-Agent pour éviter les blocages
  page <- GET(url, user_agent("Mozilla/5.0"))
  
  # Vérifier si la requête a réussi
  if (http_status(page)$category != "Success") {
    print(paste("Erreur:", http_status(page)$message))
    return(NULL)
  }
  
  # Lire le contenu HTML de la page
  page_content <- read_html(page)
  
  # Extraire le premier paragraphe
  first_paragraph <- page_content %>% html_element("p") %>% html_text(trim = TRUE)
  cat("\nRésumé de l'article:\n", first_paragraph, "\n")
  
  # Extraire les liens internes vers d'autres articles Wikipédia
  links <- page_content %>% html_elements("a") %>% html_attr("href")
  
  # Filtrer les liens internes Wikipédia
  wiki_links <- unique(links[grepl("^/wiki/", links) & !grepl(":", links)])  # Exclure les liens spéciaux (fichiers, catégories, etc.)
  
  # Afficher quelques liens extraits
  cat("\nQuelques liens internes extraits :\n")
  for (i in 1:min(10, length(wiki_links))) {
    cat(i, "- https://fr.wikipedia.org", wiki_links[i], "\n")
  }
}

# 🔥






  # On installe les packages si vous ne les avez pas déjà installés
install.packages("rvest")
install.packages("stringr")

# On charge les packages
library(rvest)
library(stringr)

# On souhaite compter le nombre d'occurence d'un mot sur une page web.
compter_mots <- function(url, mot) {

# On souhaite lire  le contenu HTML de la page web
  page <- read_html(url)

# On souhaite extraire le texte de la page
  texte <- html_text(page)

  # On utilise la fonction str_count pour compter le nombre d'occurrences du mot
  nombre_occurrences <- str_count(texte, fixed(mot))

  # On obtient enfin  le résultat
  return(nombre_occurrences)
}

# Voici un exemple d'utilisation pour une page web que j'indique :
url <- "https://fr.wikipedia.org/wiki/Emmanuel_Macron"
mot_a_rechercher <- "électorale"
resultat <- compter_mots(url, mot_a_rechercher)

cat("Le mot '", mot_a_rechercher, "' apparaît", resultat, "fois dans la page web.\n")

#Maintenant j'approfondis mon programme et je propose à mon utilisateur de choisir une page web :
url <- readline(prompt = "Entrez l'URL de la page web : ")

#On souhaite chercher un mot spécifique que je peux entrer dans le terminal sur cette même page web .
mot_a_rechercher <- readline(prompt = "Entrez le mot à rechercher : ")

resultat <- compter_mots(url, mot_a_rechercher)

#Ensuite je souhaite faire afficher  le comptage de mot en une phrase :
cat("Le mot '", mot_a_rechercher, "' apparaît", resultat, "fois dans la page web.\n")

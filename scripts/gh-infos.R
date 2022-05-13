
#  ------------------------------------------------------------------------
#
# Title : Infos sur les repos d'une organisation GitHub
#    By : Etienne
#  Date : 2019-09-05
#
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------


library(gh)
library(httr)
library(jsonlite)



# Paramètres --------------------------------------------------------------


org <- "rte-antares-rpackage"



# Initialisation du dataframe final ---------------------------------------


# liste de tous les packages de l'organisation sur github
pkg <- gh("/orgs/:org/repos", org = org, .limit = Inf)

# leurs noms
name <- vapply(pkg, FUN = `[[`, FUN.VALUE = character(1), "name")

# le dataframe final synthétisant les infos
org_pkg <- data.frame(name, stringsAsFactors = FALSE)



# Contributeurs et commits ------------------------------------------------


# fonction qui génère pour un package donné un named vector listant les contributeurs
# et leurs nombres de contributions, par ordre décroissant
contrib <- function(pkg_name) {
  contributors <- gh(
    "/repos/:owner/:repo/contributors", owner = org, repo = pkg_name,
    .limit = Inf
  )
  res <- vapply(contributors, FUN = `[[`, FUN.VALUE = integer(1), "contributions")
  names <- vapply(contributors, FUN = `[[`, FUN.VALUE = character(1), "login")
  names(res) <- names
  sort(res, decreasing = TRUE)
}

# Ajout colonnes avec nombre de commits, nombre de contributeurs et liste des
# contributeurs dans le dataframe org_pkg.
# NB : contributor != collaborator (cf https://help.github.com/en/articles/github-glossary)
# Et on ne peut pas récupérer la liste des collaborateurs d'un repo si on n'en fait
# pas soi-même partie.
commits <- lapply(org_pkg$name, FUN = contrib)
org_pkg$commits <- vapply(commits, FUN = sum, FUN.VALUE = integer(1))
org_pkg$contributors_nb <- vapply(commits, FUN = length, FUN.VALUE = integer(1))
org_pkg$contributors_names <- vapply(commits,
                                     FUN = function(x) paste(names(x), collapse = ", "),
                                     FUN.VALUE = character(1))

# date du dernier commit sur la branche master de chaque package
org_pkg$last_commit <- vapply(
  org_pkg$name,
  FUN = function(x) gh("/repos/:owner/:repo/commits/master", owner = org, repo = x)$commit$author$date,
  FUN.VALUE = character(1)
)
org_pkg$last_commit <- as.Date(org_pkg$last_commit)



# Issues et pull requests -------------------------------------------------


# Fonction qui retourne la longueur d'une liste, et 0 quand l'input n'est pas une liste.
# En effet quand il n'y a aucun élément (par exemple aucune PR ouverte) l'API
# retourne "", or length("") est 1 et pas 0.
len <- function(x) if (is.list(x)) length(x) else 0L

# NB : comme indiqué ici https://developer.github.com/v3/issues/#list-issues-for-a-repository
# "GitHub's REST API v3 considers every pull request an issue, but not every issue is a pull request."

# NB2 : On pourrait compter les PR et issues ouvertes et fermées en utilisant l'API search mais :
# - les résultats sont parfois faux : essayer par exple :
#     gh("/search/issues?q=repo:tidyverse/dplyr+type:issue+state:closed")
# - l'API search a un rate limit de seulement 30 requêtes par minute, même avec un token.

# pull requests ouvertes
org_pkg$open_pr <- vapply(
  org_pkg$name,
  FUN = function(x) len(gh("/repos/:owner/:repo/pulls", owner = org, repo = x,
                           state = "open", .limit = Inf)),
  FUN.VALUE = integer(1)
)

# pull requests fermées
org_pkg$closed_pr <- vapply(
  org_pkg$name,
  FUN = function(x) len(gh("/repos/:owner/:repo/pulls", owner = org, repo = x,
                           state = "closed", .limit = Inf)),
  FUN.VALUE = integer(1)
)

# (issues ET pull requests) ouvertes
open_issues_and_pr <- vapply(pkg, FUN = `[[`, FUN.VALUE = integer(1), "open_issues")

# (issues ET pull requests) fermées
# NB :
closed_issues_and_pr <- vapply(
  org_pkg$name,
  FUN = function(x) len(gh("/repos/:owner/:repo/issues", owner = org, repo = x,
                           state = "closed", .limit = Inf)),
  FUN.VALUE = integer(1)
)

# issues ouvertes
org_pkg$open_issues <- open_issues_and_pr - org_pkg$open_pr

# issues fermées
org_pkg$closed_issues <- closed_issues_and_pr - org_pkg$closed_pr




# Téléchargements ---------------------------------------------------------


# package sur le CRAN ou pas
org_pkg$on_cran <- org_pkg$name %in% available.packages(repos = "https://cran.rstudio.com")[, "Package"]

# fonction qui ramène le nombre de téléchargements total des packages
# depuis le CRAN RStudio entre la date 'orig' et aujourd'hui
nb_dl <- function(orig) {
  cranlogs <- GET(
    paste0(
      "https://cranlogs.r-pkg.org/downloads/total/",
      orig,
      ":",
      Sys.Date(),
      "/",
      paste(org_pkg$name[org_pkg$on_cran], collapse = ",")
    )
  )
  res <- fromJSON(content(cranlogs, as = "text"))[, c("package", "downloads")]
  res$downloads <- as.integer(res$downloads)
  res
}

nb_dl_1month <- nb_dl(seq(Sys.Date(), by = "-1 month", length = 2)[2])
names(nb_dl_1month) <- c("name", "last_month_downloads")

nb_dl_total <- nb_dl(as.Date("2000-01-01"))
names(nb_dl_total) <- c("name", "total_downloads")

# Ajout des colonnes donnant le nb de téléchargements total et le nb de
# téléchargements sur le dernier mois.
# Attention base::merge change l'ordre des lignes (même avec sort=FALSE).
org_pkg <- merge(org_pkg, nb_dl_1month, by = "name", all.x = TRUE)
org_pkg <- merge(org_pkg, nb_dl_total, by = "name", all.x = TRUE)



# Titres ------------------------------------------------------------------


# fonction qui récupère le titre d'un package depuis le DESCRIPTION
# (à condition que le repo soit bien un package R...)
read_title <- function(pkg_name, org) {
  desc_file <- paste0("https://raw.githubusercontent.com/", org, "/", pkg_name, "/master/DESCRIPTION")
  con <- url(desc_file)
  desc <- tryCatch(read.dcf(con, fields = "Title")[1, 1], warning = function(e) NA_character_)
  close(con)
  desc
}

titles <- vapply(
  org_pkg$name,
  FUN = read_title,
  FUN.VALUE = character(1),
  org = org
)




# Ordre des colonnes ------------------------------------------------------

org_pkg <- org_pkg[, c(
  "name", "commits",
  "last_commit", "open_pr", "closed_pr", "open_issues", "closed_issues",
  "on_cran", "last_month_downloads", "total_downloads",
  "contributors_nb", "contributors_names"
)]



# Export ------------------------------------------------------------------

attr(org_pkg, "organization") <- org
attr(org_pkg, "titles") <- titles
attr(org_pkg, "date_maj") <- Sys.Date()

saveRDS(org_pkg, "datas/org_pkg.rds")



# voir aussi tableau Jalal : https://rte-antares-rpackage.github.io/rPackagesRte/



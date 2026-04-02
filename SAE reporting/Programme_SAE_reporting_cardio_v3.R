rm(list = ls())
setwd("C:\BUT SD 2 EMS FI\Site portfolio\SAE reporting\CardioGoodFitness.csv")
.libPaths("Packages")

#Packages nécessaires
install.packages(c("shiny","shinydashboard","tidyverse","DT","readxl",
                    "corrplot","ggcorrplot","FactoMineR","factoextra","scales"))

install.packages("shiny")
install.packages("shinydashboard")
install.packages("tidyverse")
install.packages("DT")
install.packages("readxl")
install.packages("corrplot")
install.packages("ggcorrplot")
install.packages("FactoMineR")
install.packages("factoextra")

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(ggcorrplot)
library(scales)
library(FactoMineR)
library(factoextra)


# ─── Import des données ────────────────────────────────────────
don <- read.delim("CardioGoodFitness.csv", sep = ",")
don <- don %>% mutate(
  Fitness       = factor(Fitness),
  Product       = factor(Product),
  Gender        = factor(Gender),
  MaritalStatus = factor(MaritalStatus),
  Niv = cut(Education, breaks = c(11, 14, 16, 21),
            labels = c("12-14", "14-16", ">16")),
  Usage_cat = case_when(
    Usage <= 3             ~ "Faible",
    Usage > 3 & Usage <= 5 ~ "Modere",
    Usage > 5              ~ "Eleve",
    .default = "Autre"
  ) %>% factor(levels = c("Faible", "Modere", "Eleve"))
)
don <- don %>%
  mutate(
    Gender        = ifelse(Gender        == "Male",   "Homme",       "Femme"),
    MaritalStatus = ifelse(MaritalStatus == "Single", "Célibataire", "En couple")
  )

# ─── Palette de couleurs ───────────────────────────────────────
COL_MAIN   <- "#1A1A2E"
COL_ACCENT <- "#E94560"
pal_accent <- c("#C0392B", "#2980B9", "#8E44AD", "#27AE60", "#E67E22", "#16A085")

# ─── Listes de variables ──────────────────────────────────────
vars_quanti <- c("Age", "Education", "Usage", "Income", "Miles")
vars_quali  <- c("Product", "Gender", "MaritalStatus", "Fitness", "Niv", "Usage_cat")
vars_all    <- c(vars_quanti, vars_quali)

# ══════════════════════════════════════════════════════════════
#  THEME GGPLOT COMMUN
# ══════════════════════════════════════════════════════════════
theme_cardio <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background   = element_rect(fill = "#FFFFFF", color = NA),
      panel.background  = element_rect(fill = "#FFFFFF", color = NA),
      panel.grid.major  = element_line(color = "#eeeeee", linewidth = 0.5),
      panel.grid.minor  = element_blank(),
      text              = element_text(color = "#333333"),
      axis.text         = element_text(color = "#666666", size = 10),
      axis.title        = element_text(color = "#444444", size = 11, face = "bold"),
      plot.title        = element_text(color = "#C0392B", size = 13, face = "bold"),
      legend.background = element_rect(fill = "#FFFFFF", color = NA),
      legend.text       = element_text(color = "#444444"),
      legend.title      = element_text(color = "#C0392B"),
      strip.text        = element_text(color = "#C0392B", face = "bold")
    )
}

# ══════════════════════════════════════════════════════════════
#  UI
# ══════════════════════════════════════════════════════════════
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = tags$span(
      tags$b("CARDIO FITNESS",
             style = "font-size:16px; letter-spacing:3px; color:#E94560;"),
      tags$span(" | Tableau de Bord",
                style = "font-size:13px; color:#aaa;")
    ),
    titleWidth = 320
  ),
  
  dashboardSidebar(
    width = 260,
    tags$style(HTML("
      .main-sidebar, .left-side { background-color: #1A1A2E !important; }
      .sidebar-menu > li > a { color: #ccc !important; font-size: 13px; letter-spacing: 1px; }
      .sidebar-menu > li.active > a,
      .sidebar-menu > li > a:hover { background-color: #C0392B !important; color: #fff !important; }
      .treeview-menu > li > a { color: #aaa !important; }
      .sidebar-menu .treeview-menu > li.active > a { color: #C0392B !important; }
    ")),
    br(),
    tags$div(style = "padding: 0 15px 10px; color:#aaa; font-size:11px; letter-spacing:2px;",
             "NAVIGATION"),
    sidebarMenu(
      menuItem("Présentation",         tabName = "presentation", icon = icon("info-circle")),
      menuItem("Stat. Univariée",      tabName = "univarie",     icon = icon("chart-bar")),
      menuItem("Stat. Bivariée",       tabName = "bivarie",      icon = icon("chart-line")),
      menuItem("Analyse Multivariée",  tabName = "multi",        icon = icon("project-diagram")),
      menuItem("Vision Tabulaire",     tabName = "tableau",      icon = icon("table"))
    ),
    br(),
    tags$div(style = "padding: 10px 15px; border-top: 1px solid #222;",
             tags$p(style = "color:#555; font-size:10px; letter-spacing:1px;", "FILTRES GLOBAUX"),
             selectInput("filtre_gender", "Genre",
                         choices = c("Tous", "Homme", "Femme"), selected = "Tous", width = "100%"),
             selectInput("filtre_product", "Produit",
                         choices = c("Tous", "TM195", "TM498", "TM798"), selected = "Tous", width = "100%"),
             selectInput("filtre_marital", "Statut marital",
                         choices = c("Tous", "Célibataire", "En couple"), selected = "Tous", width = "100%"),
             sliderInput("filtre_age", "Tranche d'âge",
                         min = 18, max = 50, value = c(18, 50), step = 1, width = "100%")
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      body, .content-wrapper, .main-footer { background-color: #F4F6FB !important; color: #1a1a2e; }
      .content { padding: 20px 25px; }
      .box { background: #FFFFFF; border-top: 3px solid #C0392B;
             border-radius: 4px; box-shadow: 0 2px 12px rgba(0,0,0,0.10); }
      .box-title { color: #C0392B !important; letter-spacing: 2px; font-size: 12px; }
      .box-header { background: transparent; border-bottom: 1px solid #eee; }
      .box-body { color: #333; }
      .small-box { border-radius: 4px !important; }
      .small-box h3 { font-size: 32px; }
      .small-box .icon { opacity: 0.25; }
      .nav-tabs { border-bottom: 2px solid #C0392B; }
      .nav-tabs > li > a { color: #555; background: #e8eaf0; border: none;
                           border-radius: 0; letter-spacing: 1px; font-size: 12px; }
      .nav-tabs > li.active > a,
      .nav-tabs > li > a:hover { background: #C0392B !important; color: #fff !important; border: none; }
      .tab-content { background: #fff; padding: 20px; border-radius: 0 0 4px 4px; }
      .dataTables_wrapper { color: #333; }
      table.dataTable thead th { background: #f0f2f8; color: #C0392B;
                                  border-bottom: 2px solid #C0392B; font-size:12px; letter-spacing:1px; }
      table.dataTable tbody tr { background: #fff !important; }
      table.dataTable tbody tr:hover { background: #fef2f2 !important; }
      table.dataTable tbody td { color: #333; border-color: #eee; }
      .dataTables_filter input, .dataTables_length select {
        background: #fff; border: 1px solid #ccc; color: #333; }
      .dataTables_info, .dataTables_paginate { color: #888 !important; }
      .paginate_button { color: #555 !important; }
      .paginate_button.current { background: #C0392B !important; color: #fff !important; border: none !important; }
      .section-title {
        font-size: 11px; letter-spacing: 3px; color: #C0392B;
        border-bottom: 2px solid #C0392B; padding-bottom: 8px;
        margin-bottom: 20px; text-transform: uppercase;
      }
      .pres-card {
        background: #fff; border: 1px solid #e0e0e0; border-radius: 4px; padding: 28px;
        margin-bottom: 20px; box-shadow: 0 2px 8px rgba(0,0,0,0.07);
      }
      .kpi-box {
        background: #fff; border-left: 4px solid #C0392B;
        padding: 16px 20px; border-radius: 2px; text-align: center;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      }
      .kpi-box h2 { color: #C0392B; font-size: 28px; margin: 0; }
      .kpi-box p  { color: #888; font-size: 11px; letter-spacing: 2px; margin: 4px 0 0; }
      .selectize-input { background: #fff !important; border-color: #ccc !important; color: #333 !important; }
      .selectize-dropdown { background: #fff !important; color: #333 !important; }
      .irs-bar, .irs-bar-edge { background: #C0392B !important; border-color: #C0392B !important; }
      .irs-handle { background: #C0392B !important; border-color: #C0392B !important; }
      .irs-from, .irs-to, .irs-single { background: #C0392B !important; }
      /* Résultats de test */
      .test-result-box {
        background: #fdf2f1; border-left: 4px solid #C0392B;
        padding: 14px 18px; border-radius: 2px; margin-top: 12px;
        font-size: 13px; color: #333;
      }
      .test-result-box .test-title {
        font-weight: 700; color: #C0392B; font-size: 12px;
        letter-spacing: 2px; text-transform: uppercase; margin-bottom: 8px;
      }
      .test-result-box .test-concl {
        background: #fff; padding: 8px 12px; border-radius: 2px;
        margin-top: 8px; font-size: 12px; color: #555;
        border: 1px solid #f0c0bb;
      }
    "))),
    
    tabItems(
      
      # ════════════════════════════════════════════════════════
      # 1. PRÉSENTATION
      # ════════════════════════════════════════════════════════
      tabItem(tabName = "presentation",
              
              tags$div(
                style = "background: linear-gradient(135deg, #C0392B 0%, #922B21 100%);
                   padding: 28px 32px; border-radius: 6px; margin-bottom: 22px;
                   box-shadow: 0 4px 18px rgba(192,57,43,0.18);",
                tags$div(style = "font-size:10px; letter-spacing:4px; color:rgba(255,255,255,0.65);
                            text-transform:uppercase; margin-bottom:6px;",
                         "Statistique Descriptive"),
                tags$h1(style = "color:#fff; margin:0; font-size:26px; letter-spacing:2px; font-weight:700;",
                        "CardioGoodFitness"),
                tags$p(style = "color:rgba(255,255,255,0.80); margin:8px 0 0; font-size:14px;",
                       "Tableau de bord interactif — 180 clients  |  11 variables")
              ),
              
              fluidRow(
                column(7,
                       tags$div(class = "pres-card",
                                tags$div(style = "display:flex; align-items:center; gap:10px; margin-bottom:14px;",
                                         tags$span(style = "background:#C0392B; color:#fff; border-radius:50%;
                           width:32px; height:32px; display:flex; align-items:center;
                           justify-content:center; font-size:15px; flex-shrink:0;", "🏃"),
                                         tags$h3(style = "margin:0; color:#C0392B; font-size:15px; letter-spacing:2px;
                                 text-transform:uppercase;", "Résumé de l'étude")
                                ),
                                tags$p(style = "color:#444; line-height:1.85; font-size:13.5px; margin-bottom:10px;",
                                       tags$b("CardioGoodFitness"), " est un jeu de données issu de ",
                                       tags$b(style = "color:#C0392B;", "Kaggle"),
                                       " présentant les caractéristiques de clients ayant acheté un tapis de course.
                 L'objectif est de mettre en avant les éléments de différence et de concordance
                 entre les profils des clients ", tags$b("femmes et hommes"),
                                       ", et entre les différents ", tags$b("modèles de produit"),
                                       " achetés (TM195, TM498, TM798)."
                                ),
                                tags$hr(style = "border-color:#f0f0f0; margin:14px 0;"),
                                tags$div(style = "display:flex; align-items:center; gap:10px; margin-bottom:10px;",
                                         tags$span(style = "background:#C0392B; color:#fff; border-radius:50%;
                           width:32px; height:32px; display:flex; align-items:center;
                           justify-content:center; font-size:15px; flex-shrink:0;", "🎯"),
                                         tags$h3(style = "margin:0; color:#C0392B; font-size:15px; letter-spacing:2px;
                                 text-transform:uppercase;", "Objectifs du tableau de bord")
                                ),
                                tags$div(style = "display:grid; grid-template-columns:1fr 1fr; gap:8px;",
                                         tags$div(style = "background:#fdf2f1; border-left:3px solid #C0392B;
                          padding:10px 12px; border-radius:2px; font-size:12.5px; color:#444;",
                                                  tags$b(style = "color:#C0392B;", "①"),
                                                  " Décrire les distributions des variables démographiques et comportementales"),
                                         tags$div(style = "background:#fdf2f1; border-left:3px solid #C0392B;
                          padding:10px 12px; border-radius:2px; font-size:12.5px; color:#444;",
                                                  tags$b(style = "color:#C0392B;", "②"),
                                                  " Identifier les corrélations entre revenu, éducation, usage et distance"),
                                         tags$div(style = "background:#fdf2f1; border-left:3px solid #C0392B;
                          padding:10px 12px; border-radius:2px; font-size:12.5px; color:#444;",
                                                  tags$b(style = "color:#C0392B;", "③"),
                                                  " Comparer les profils selon le genre, le produit et le statut marital"),
                                         tags$div(style = "background:#fdf2f1; border-left:3px solid #C0392B;
                          padding:10px 12px; border-radius:2px; font-size:12.5px; color:#444;",
                                                  tags$b(style = "color:#C0392B;", "④"),
                                                  " Fournir une vision tabulaire interactive et exportable de la base")
                                )
                       )
                ),
                column(5,
                       tags$div(style = "display:grid; grid-template-columns:1fr 1fr; gap:12px; margin-bottom:14px;",
                                tags$div(class = "kpi-box", tags$h2(textOutput("kpi_n")),      tags$p("CLIENTS")),
                                tags$div(class = "kpi-box", tags$h2(textOutput("kpi_age")),    tags$p("ÂGE MOYEN")),
                                tags$div(class = "kpi-box", tags$h2(textOutput("kpi_income")), tags$p("REVENU MÉD.")),
                                tags$div(class = "kpi-box", tags$h2(textOutput("kpi_miles")),  tags$p("MILES MOY./SEM."))
                       ),
                       tags$div(class = "pres-card", style = "padding:18px;",
                                tags$p(style = "color:#C0392B; font-size:11px; letter-spacing:2px;
                              text-transform:uppercase; margin:0 0 10px; font-weight:700;",
                                       "Source & Contexte"),
                                tags$table(style = "font-size:12.5px; color:#555; width:100%;",
                                           tags$tr(tags$td(style="padding:4px 0; color:#888; width:40%;","Source"),
                                                   tags$td(style="padding:4px 0; color:#333; font-weight:600;","Kaggle")),
                                           tags$tr(tags$td(style="padding:4px 0; color:#888;","Jeu de données"),
                                                   tags$td(style="padding:4px 0; color:#333; font-weight:600;","CardioGoodFitness")),
                                           tags$tr(tags$td(style="padding:4px 0; color:#888;","Taille"),
                                                   tags$td(style="padding:4px 0; color:#333; font-weight:600;","180 observations")),
                                           tags$tr(tags$td(style="padding:4px 0; color:#888;","Variables"),
                                                   tags$td(style="padding:4px 0; color:#333; font-weight:600;","11 (5 numériques, 6 catégorielles)")),
                                           tags$tr(tags$td(style="padding:4px 0; color:#888;","Produits"),
                                                   tags$td(style="padding:4px 0; color:#333; font-weight:600;","TM195 · TM498 · TM798"))
                                )
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       tags$div(class = "pres-card",
                                tags$div(style = "display:flex; align-items:center; gap:10px; margin-bottom:16px;",
                                         tags$span(style = "background:#C0392B; color:#fff; border-radius:50%;
                           width:32px; height:32px; display:flex; align-items:center;
                           justify-content:center; font-size:15px; flex-shrink:0;", "📊"),
                                         tags$h3(style = "margin:0; color:#C0392B; font-size:15px; letter-spacing:2px;
                                 text-transform:uppercase;", "Description détaillée des variables")
                                ),
                                tags$div(style = "display:grid; grid-template-columns: repeat(3, 1fr); gap:14px;",
                                         tags$div(style="border:1px solid #eee; border-radius:4px; padding:14px; background:#fafafa;",
                                                  tags$div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom:8px;",
                                                           tags$span(style="font-weight:700; color:#C0392B; font-size:13px; letter-spacing:1px;","PRODUCT"),
                                                           tags$span(style="background:#fdf2f1; color:#C0392B; font-size:10px; padding:2px 8px; border-radius:10px;","Catégorielle")),
                                                  tags$p(style="color:#555; font-size:12px; margin:0 0 6px;","Modèle de tapis de course acheté."),
                                                  tags$div(style="display:flex; gap:6px; flex-wrap:wrap;",
                                                           tags$span(style="background:#fdf2f1; color:#C0392B; font-size:11px; padding:2px 7px; border-radius:3px;","TM195"),
                                                           tags$span(style="background:#fdf2f1; color:#C0392B; font-size:11px; padding:2px 7px; border-radius:3px;","TM498"),
                                                           tags$span(style="background:#fdf2f1; color:#C0392B; font-size:11px; padding:2px 7px; border-radius:3px;","TM798")
                                                  )
                                         ),
                                         tags$div(style="border:1px solid #eee; border-radius:4px; padding:14px; background:#fafafa;",
                                                  tags$div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom:8px;",
                                                           tags$span(style="font-weight:700; color:#C0392B; font-size:13px; letter-spacing:1px;","AGE"),
                                                           tags$span(style="background:#e8f4fd; color:#2980B9; font-size:10px; padding:2px 8px; border-radius:10px;","Numérique")),
                                                  tags$p(style="color:#555; font-size:12px; margin:0;","Âge du client en années.")
                                         ),
                                         tags$div(style="border:1px solid #eee; border-radius:4px; padding:14px; background:#fafafa;",
                                                  tags$div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom:8px;",
                                                           tags$span(style="font-weight:700; color:#C0392B; font-size:13px; letter-spacing:1px;","GENDER"),
                                                           tags$span(style="background:#fdf2f1; color:#C0392B; font-size:10px; padding:2px 8px; border-radius:10px;","Catégorielle")),
                                                  tags$p(style="color:#555; font-size:12px; margin:0 0 6px;","Genre du client."),
                                                  tags$div(style="display:flex; gap:6px;",
                                                           tags$span(style="background:#eafaf1; color:#27AE60; font-size:11px; padding:2px 7px; border-radius:3px;","Femme"),
                                                           tags$span(style="background:#eaf2fb; color:#2980B9; font-size:11px; padding:2px 7px; border-radius:3px;","Homme")
                                                  )
                                         ),
                                         tags$div(style="border:1px solid #eee; border-radius:4px; padding:14px; background:#fafafa;",
                                                  tags$div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom:8px;",
                                                           tags$span(style="font-weight:700; color:#C0392B; font-size:13px; letter-spacing:1px;","EDUCATION"),
                                                           tags$span(style="background:#e8f4fd; color:#2980B9; font-size:10px; padding:2px 8px; border-radius:10px;","Numérique")),
                                                  tags$p(style="color:#555; font-size:12px; margin:0;","Nombre d'années d'études (entre 12 et 21).")
                                         ),
                                         tags$div(style="border:1px solid #eee; border-radius:4px; padding:14px; background:#fafafa;",
                                                  tags$div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom:8px;",
                                                           tags$span(style="font-weight:700; color:#C0392B; font-size:13px; letter-spacing:1px;","MARITALSTATUS"),
                                                           tags$span(style="background:#fdf2f1; color:#C0392B; font-size:10px; padding:2px 8px; border-radius:10px;","Catégorielle")),
                                                  tags$p(style="color:#555; font-size:12px; margin:0 0 6px;","Statut matrimonial du client."),
                                                  tags$div(style="display:flex; gap:6px; flex-wrap:wrap;",
                                                           tags$span(style="background:#f9f9f9; color:#555; font-size:11px; padding:2px 7px; border-radius:3px; border:1px solid #ddd;","Célibataire"),
                                                           tags$span(style="background:#f9f9f9; color:#555; font-size:11px; padding:2px 7px; border-radius:3px; border:1px solid #ddd;","En couple")
                                                  )
                                         ),
                                         tags$div(style="border:1px solid #eee; border-radius:4px; padding:14px; background:#fafafa;",
                                                  tags$div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom:8px;",
                                                           tags$span(style="font-weight:700; color:#C0392B; font-size:13px; letter-spacing:1px;","USAGE"),
                                                           tags$span(style="background:#e8f4fd; color:#2980B9; font-size:10px; padding:2px 8px; border-radius:10px;","Entier")),
                                                  tags$p(style="color:#555; font-size:12px; margin:0;",
                                                         "Nombre moyen de fois où le client pense utiliser le tapis par semaine (entre 2 et 7).")
                                         ),
                                         tags$div(style="border:1px solid #eee; border-radius:4px; padding:14px; background:#fafafa;",
                                                  tags$div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom:8px;",
                                                           tags$span(style="font-weight:700; color:#C0392B; font-size:13px; letter-spacing:1px;","FITNESS"),
                                                           tags$span(style="background:#f4ecf7; color:#8E44AD; font-size:10px; padding:2px 8px; border-radius:10px;","Ordinale (1–5)")),
                                                  tags$p(style="color:#555; font-size:12px; margin:0 0 6px;","Condition physique auto-évaluée par le client."),
                                                  tags$div(style="font-size:11px; color:#666; line-height:1.7;",
                                                           tags$div("1 – Très mauvaise forme"), tags$div("3 – Forme moyenne"), tags$div("5 – Excellente forme")
                                                  )
                                         ),
                                         tags$div(style="border:1px solid #eee; border-radius:4px; padding:14px; background:#fafafa;",
                                                  tags$div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom:8px;",
                                                           tags$span(style="font-weight:700; color:#C0392B; font-size:13px; letter-spacing:1px;","INCOME"),
                                                           tags$span(style="background:#e8f4fd; color:#2980B9; font-size:10px; padding:2px 8px; border-radius:10px;","Numérique")),
                                                  tags$p(style="color:#555; font-size:12px; margin:0;",
                                                         "Revenu annuel du foyer en ", tags$b("dollars ($)"), ".")
                                         ),
                                         tags$div(style="display:flex; flex-direction:column; gap:12px;",
                                                  tags$div(style="border:1px solid #eee; border-radius:4px; padding:14px; background:#fafafa;",
                                                           tags$div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom:6px;",
                                                                    tags$span(style="font-weight:700; color:#C0392B; font-size:13px; letter-spacing:1px;","MILES"),
                                                                    tags$span(style="background:#e8f4fd; color:#2980B9; font-size:10px; padding:2px 8px; border-radius:10px;","Numérique")),
                                                           tags$p(style="color:#555; font-size:12px; margin:0;","Distance moyenne attendue par semaine, en miles.")
                                                  ),
                                                  tags$div(style="border:1px solid #eee; border-radius:4px; padding:14px; background:#fafafa;",
                                                           tags$div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom:6px;",
                                                                    tags$span(style="font-weight:700; color:#C0392B; font-size:13px; letter-spacing:1px;","NIV"),
                                                                    tags$span(style="background:#fdf2f1; color:#C0392B; font-size:10px; padding:2px 8px; border-radius:10px;","Catégorielle")),
                                                           tags$p(style="color:#555; font-size:12px; margin:0;","Niveau d'éducation catégorisé : 12-14, 14-16, >16 années d'études.")
                                                  ),
                                                  tags$div(style="border:1px solid #eee; border-radius:4px; padding:14px; background:#fafafa;",
                                                           tags$div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom:6px;",
                                                                    tags$span(style="font-weight:700; color:#C0392B; font-size:13px; letter-spacing:1px;","USAGE_CAT"),
                                                                    tags$span(style="background:#fdf2f1; color:#C0392B; font-size:10px; padding:2px 8px; border-radius:10px;","Catégorielle")),
                                                           tags$p(style="color:#555; font-size:12px; margin:0;","Usage catégorisé : Faible (≤3), Modéré (4-5), Élevé (>5) séances/semaine.")
                                                  )
                                         )
                                )
                       )
                )
              )
      ), # fin tabItem presentation
      
      # ════════════════════════════════════════════════════════
      # 2. STATISTIQUES UNIVARIÉES  (NOUVEAU)
      # ════════════════════════════════════════════════════════
      tabItem(tabName = "univarie",
              tags$div(class = "section-title", "Statistiques univariées"),
              
              tabsetPanel(
                
                # ── 2a. Variables continues ──────────────────────────
                tabPanel("Variables continues",
                         br(),
                         fluidRow(
                           column(4,
                                  box(width = 12, title = "VARIABLE À ANALYSER",
                                      selectInput("uni_var_cont", NULL,
                                                  choices  = vars_quanti,
                                                  selected = "Income"),
                                      hr(),
                                      tags$p(style="color:#C0392B; font-size:11px; letter-spacing:1px; font-weight:700;",
                                             "STATISTIQUES DESCRIPTIVES"),
                                      tableOutput("uni_resume_cont"),
                                      hr(),
                                      tags$p(style="color:#C0392B; font-size:11px; letter-spacing:1px; font-weight:700;",
                                             "TEST DE NORMALITÉ (Shapiro-Wilk)"),
                                      tags$div(class = "test-result-box",
                                               tags$div(class = "test-title", "H₀ : distribution normale"),
                                               verbatimTextOutput("uni_shapiro"),
                                               tags$div(class = "test-concl", textOutput("uni_shapiro_concl"))
                                      )
                                  )
                           ),
                           column(8,
                                  box(width = 12, title = "HISTOGRAMME + DENSITÉ",
                                      plotOutput("uni_hist", height = "300px")),
                                  box(width = 12, title = "BOÎTE À MOUSTACHES",
                                      plotOutput("uni_boxplot", height = "220px"))
                           )
                         )
                ),
                
                # ── 2b. Variables catégorielles ──────────────────────
                tabPanel("Variables catégorielles",
                         br(),
                         fluidRow(
                           column(4,
                                  box(width = 12, title = "VARIABLE",
                                      selectInput("uni_var_cat", NULL,
                                                  choices  = vars_quali,
                                                  selected = "Product"),
                                      hr(),
                                      tags$p(style="color:#C0392B; font-size:11px; letter-spacing:1px; font-weight:700;",
                                             "TABLEAU DE FRÉQUENCES"),
                                      tableOutput("uni_freq_table")
                                  )
                           ),
                           column(8,
                                  box(width = 12, title = "DIAGRAMME EN BARRES",
                                      plotOutput("uni_barplot", height = "300px")),
                                  box(width = 12, title = "DIAGRAMME CIRCULAIRE",
                                      plotOutput("uni_pieplot", height = "260px"))
                           )
                         )
                )
              )
      ), # fin tabItem univarie
      
      # ════════════════════════════════════════════════════════
      # 3. STATISTIQUES BIVARIÉES  (NOUVEAU)
      # ════════════════════════════════════════════════════════
      tabItem(tabName = "bivarie",
              tags$div(class = "section-title", "Statistiques bivariées"),
              
              tabsetPanel(
                
                # ── 3a. Quantitative × Quantitative ─────────────────
                tabPanel("Quantitative × Quantitative",
                         br(),
                         fluidRow(
                           column(3,
                                  box(width = 12, title = "PARAMÈTRES",
                                      selectInput("biv_qq_x", "Variable X",
                                                  choices = vars_quanti, selected = "Age"),
                                      selectInput("biv_qq_y", "Variable Y",
                                                  choices = vars_quanti, selected = "Income"),
                                      checkboxInput("biv_qq_lm", "Droite de régression", value = TRUE),
                                      hr(),
                                      tags$p(style="color:#C0392B; font-size:11px; letter-spacing:1px; font-weight:700;",
                                             "CORRÉLATIONS"),
                                      tableOutput("biv_qq_cor_table"),
                                      hr(),
                                      tags$div(class = "test-result-box",
                                               tags$div(class = "test-title", "Test de corrélation"),
                                               tags$p(style="font-size:11px; color:#666; margin-bottom:6px;",
                                                      "H₀ : ρ = 0  |  H₁ : ρ ≠ 0"),
                                               verbatimTextOutput("biv_qq_test"),
                                               tags$div(class = "test-concl", textOutput("biv_qq_concl"))
                                      )
                                  )
                           ),
                           column(9,
                                  box(width = 12, title = "NUAGE DE POINTS",
                                      plotOutput("biv_qq_scatter", height = "400px")),
                                  fluidRow(
                                    column(6,
                                           box(width = 12, title = "DISTRIBUTION DE X",
                                               plotOutput("biv_qq_hist_x", height = "200px"))
                                    ),
                                    column(6,
                                           box(width = 12, title = "DISTRIBUTION DE Y",
                                               plotOutput("biv_qq_hist_y", height = "200px"))
                                    )
                                  )
                           )
                         )
                ),
                
                # ── 3b. Quantitative × Qualitative ──────────────────
                tabPanel("Quantitative × Qualitative",
                         br(),
                         # ── Ligne 1 : paramètres en horizontal ──
                         box(width = 12, title = "PARAMÈTRES",
                             fluidRow(
                               column(3,
                                      selectInput("biv_ql_quant", "Variable quantitative",
                                                  choices = vars_quanti, selected = "Income")
                               ),
                               column(3,
                                      selectInput("biv_ql_qual", "Variable qualitative",
                                                  choices = vars_quali, selected = "Gender")
                               ),
                               column(6,
                                      tags$div(class = "test-result-box", style = "margin-top:0;",
                                               tags$div(class = "test-title", "Test statistique — 2 groupes : t-test | ≥3 groupes : ANOVA"),
                                               tags$p(style="font-size:11px; color:#666; margin-bottom:4px;",
                                                      "H₀ : égalité des moyennes entre groupes"),
                                               tags$div(class = "test-concl", textOutput("biv_ql_concl"))
                                      )
                               )
                             )
                         ),
                         # ── Ligne 2 : stats descriptives + test détaillé ──
                         fluidRow(
                           column(6,
                                  box(width = 12, title = "STAT. DESCRIPTIVES PAR GROUPE",
                                      tableOutput("biv_ql_desc"))
                           ),
                           column(6,
                                  box(width = 12, title = "RÉSULTAT DU TEST",
                                      verbatimTextOutput("biv_ql_test"))
                           )
                         ),
                         # ── Ligne 3 : graphiques ──
                         fluidRow(
                           column(6,
                                  box(width = 12, title = "BOÎTES À MOUSTACHES PAR GROUPE",
                                      plotOutput("biv_ql_box", height = "340px"))
                           ),
                           column(6,
                                  box(width = 12, title = "DENSITÉS PAR GROUPE",
                                      plotOutput("biv_ql_density", height = "340px"))
                           )
                         )
                ),
                
                # ── 3c. Qualitative × Qualitative ───────────────────
                tabPanel("Qualitative × Qualitative",
                         br(),
                         # ── Ligne 1 : paramètres en horizontal ──
                         box(width = 12, title = "PARAMÈTRES",
                             fluidRow(
                               column(3,
                                      selectInput("biv_cc_x", "Variable X",
                                                  choices = vars_quali, selected = "Gender")
                               ),
                               column(3,
                                      selectInput("biv_cc_y", "Variable Y",
                                                  choices = vars_quali, selected = "Product")
                               ),
                               column(6,
                                      tags$div(class = "test-result-box", style = "margin-top:0;",
                                               tags$div(class = "test-title", "Test du χ² d'indépendance"),
                                               tags$p(style="font-size:11px; color:#666; margin-bottom:4px;",
                                                      "H₀ : indépendance  |  H₁ : association"),
                                               tags$div(class = "test-concl", textOutput("biv_cc_concl"))
                                      )
                               )
                             )
                         ),
                         # ── Ligne 2 : table de contingence + résultat test ──
                         fluidRow(
                           column(6,
                                  box(width = 12, title = "TABLE DE CONTINGENCE",
                                      tableOutput("biv_cc_table"))
                           ),
                           column(6,
                                  box(width = 12, title = "RÉSULTAT DU TEST χ²",
                                      verbatimTextOutput("biv_cc_chi2"))
                           )
                         ),
                         # ── Ligne 3 : graphiques ──
                         fluidRow(
                           column(6,
                                  box(width = 12, title = "BARRES EMPILÉES (proportions)",
                                      plotOutput("biv_cc_bar_fill", height = "320px"))
                           ),
                           column(6,
                                  box(width = 12, title = "BARRES GROUPÉES (effectifs)",
                                      plotOutput("biv_cc_bar_dodge", height = "320px"))
                           )
                         )
                )
              )
      ), # fin tabItem bivarie
      
      # ════════════════════════════════════════════════════════
      # 4. ANALYSE MULTIVARIÉE  (ENRICHI)
      # ════════════════════════════════════════════════════════
      tabItem(tabName = "multi",
              tags$div(class = "section-title", "Analyse multivariée"),
              
              tabsetPanel(
                
                # ── 4a. Corrélations ────────────────────────────────
                tabPanel("Corrélations",
                         br(),
                         fluidRow(
                           column(12,
                                  box(width = 12, title = "MATRICE DE CORRÉLATION (variables numériques)",
                                      plotOutput("plot_corr", height = "420px"))
                           )
                         )
                ),
                
                # ── 4b. Nuage de points ─────────────────────────────
                tabPanel("Nuage de points",
                         br(),
                         fluidRow(
                           column(3,
                                  box(width = 12, title = "PARAMÈTRES",
                                      selectInput("scatter_x", "Axe X",
                                                  choices = vars_quanti, selected = "Education"),
                                      selectInput("scatter_y", "Axe Y",
                                                  choices = vars_quanti, selected = "Income"),
                                      selectInput("scatter_col", "Couleur",
                                                  choices = c("Aucune", vars_quali), selected = "Product"),
                                      checkboxInput("scatter_lm", "Droite de régression", value = TRUE)
                                  )
                           ),
                           column(9,
                                  box(width = 12, title = "NUAGE DE POINTS",
                                      plotOutput("plot_scatter", height = "420px"))
                           )
                         )
                ),
                
                # ── 4c. ACP ─────────────────────────────────────────
                tabPanel("ACP",
                         br(),
                         # ── Ligne 1 : paramètres en horizontal ──
                         box(width = 12, title = "PARAMÈTRES ACP",
                             fluidRow(
                               column(4,
                                      tags$p(style="font-size:12px; color:#555; margin-bottom:6px;",
                                             "Variables actives : Age, Education, Usage, Income, Miles"),
                                      selectInput("acp_quali_sup", "Variable quali. supplémentaire (quali.sup)",
                                                  choices = c("Aucune", vars_quali), selected = "Gender")
                               ),
                               column(4,
                                      selectInput("acp_plot_type", "Graphique à afficher",
                                                  choices = c(
                                                    "Éboulis des valeurs propres" = "eig",
                                                    "Graphe des individus"        = "ind",
                                                    "Graphe des variables"        = "var",
                                                    "Biplot"                      = "biplot"
                                                  ),
                                                  selected = "eig")
                               ),
                               column(4,
                                      tags$p(style="color:#C0392B; font-size:11px; letter-spacing:1px; font-weight:700; margin-bottom:6px;",
                                             "VALEURS PROPRES"),
                                      tableOutput("acp_eig_table")
                               )
                             )
                         ),
                         # ── Ligne 2 : graphique pleine largeur ──
                         fluidRow(
                           column(12,
                                  box(width = 12, title = "RÉSULTATS ACP",
                                      plotOutput("acp_plot", height = "500px"))
                           )
                         )
                ),
                
                # ── 4d. ACM ─────────────────────────────────────────
                tabPanel("ACM",
                         br(),
                         # ── Ligne 1 : paramètres en horizontal ──
                         box(width = 12, title = "PARAMÈTRES ACM",
                             fluidRow(
                               column(4,
                                      tags$p(style="font-size:12px; color:#555; margin-bottom:6px;",
                                             "Variables actives : Product, Gender, MaritalStatus, Fitness, Niv, Usage_cat"),
                                      selectInput("acm_quali_sup", "Variable supplémentaire (quali.sup)",
                                                  choices = c("Aucune", vars_quali), selected = "Product")
                               ),
                               column(4,
                                      selectInput("acm_plot_type", "Graphique à afficher",
                                                  choices = c(
                                                    "Éboulis des valeurs propres"     = "eig",
                                                    "Éboulis (règle Kaiser)"          = "eig_kaiser",
                                                    "Biplot individus + modalités"    = "biplot",
                                                    "Individus (ellipses par groupe)" = "ind_ellipse",
                                                    "Contributions Dim.1"             = "contrib1",
                                                    "Contributions Dim.2"             = "contrib2"
                                                  ),
                                                  selected = "eig")
                               ),
                               column(4,
                                      tags$p(style="color:#C0392B; font-size:11px; letter-spacing:1px; font-weight:700; margin-bottom:6px;",
                                             "VALEURS PROPRES"),
                                      tableOutput("acm_eig_table")
                               )
                             )
                         ),
                         # ── Ligne 2 : graphique pleine largeur ──
                         fluidRow(
                           column(12,
                                  box(width = 12, title = "RÉSULTATS ACM",
                                      plotOutput("acm_plot", height = "500px"))
                           )
                         )
                )
              )
      ), # fin tabItem multi
      
      # ════════════════════════════════════════════════════════
      # 5. VISION TABULAIRE
      # ════════════════════════════════════════════════════════
      tabItem(tabName = "tableau",
              tags$div(class = "section-title", "Vision tabulaire de la base de données"),
              
              fluidRow(
                column(12,
                       box(width = 12,
                           title = "BASE DE DONNÉES — CARDIO GOOD FITNESS (180 observations)",
                           tags$p(style = "color:#888; font-size:12px; letter-spacing:1px; margin-bottom:12px;",
                                  "Les filtres globaux (barre latérale) s'appliquent à cette vue.
                 Utilisez la barre de recherche et les colonnes pour trier et filtrer les données."),
                           DTOutput("table_data"),
                           br(),
                           fluidRow(
                             column(4,
                                    downloadButton("dl_csv", "Télécharger CSV",
                                                   style = "background:#C0392B; border:none; letter-spacing:1px;
                             font-size:12px; color:#fff;")
                             ),
                             column(4,
                                    tags$p(style = "color:#555; font-size:11px; margin-top:8px;",
                                           textOutput("nb_lignes_affichees"))
                             )
                           )
                       )
                )
              )
      ) # fin tabItem tableau
    )
  )
)

# ══════════════════════════════════════════════════════════════
#  SERVER
# ══════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  # ── Données filtrées ────────────────────────────────────────
  df_filt <- reactive({
    d <- don
    if (input$filtre_gender  != "Tous") d <- d[d$Gender        == input$filtre_gender,  ]
    if (input$filtre_product != "Tous") d <- d[d$Product        == input$filtre_product, ]
    if (input$filtre_marital != "Tous") d <- d[d$MaritalStatus  == input$filtre_marital, ]
    d <- d[d$Age >= input$filtre_age[1] & d$Age <= input$filtre_age[2], ]
    d
  })
  
  # ── KPIs ────────────────────────────────────────────────────
  output$kpi_n      <- renderText({ nrow(df_filt()) })
  output$kpi_age    <- renderText({ round(mean(df_filt()$Age), 1) })
  output$kpi_income <- renderText({ paste0(round(median(df_filt()$Income) / 1000, 0), "k $") })
  output$kpi_miles  <- renderText({ round(mean(df_filt()$Miles), 1) })
  
  # ══════════════════════════════════════════════════════════
  #  ONGLET 2 — UNIVARIÉ
  # ══════════════════════════════════════════════════════════
  
  # ── Résumé variable continue ─────────────────────────────
  output$uni_resume_cont <- renderTable({
    x <- df_filt()[[input$uni_var_cont]]
    data.frame(
      Statistique = c("Min","Q1","Médiane","Moyenne","Q3","Max","Écart-type","Étendue"),
      Valeur      = round(c(min(x), quantile(x,.25), median(x),
                            mean(x), quantile(x,.75), max(x),
                            sd(x), max(x)-min(x)), 2)
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE,
  rownames = FALSE, align = "lr", digits = 2)
  
  # ── Test de Shapiro-Wilk ─────────────────────────────────
  output$uni_shapiro <- renderPrint({
    x <- df_filt()[[input$uni_var_cont]]
    if (length(x) > 5000) x <- sample(x, 5000)
    shapiro.test(x)
  })
  
  output$uni_shapiro_concl <- renderText({
    x <- df_filt()[[input$uni_var_cont]]
    if (length(x) > 5000) x <- sample(x, 5000)
    p <- shapiro.test(x)$p.value
    if (p < 0.05) {
      paste0("p-value = ", signif(p, 3),
             " < 0.05 → On REJETTE H₀. La distribution n'est pas normale.")
    } else {
      paste0("p-value = ", signif(p, 3),
             " ≥ 0.05 → On ne rejette pas H₀. Normalité plausible.")
    }
  })
  
  # ── Histogramme + densité ────────────────────────────────
  output$uni_hist <- renderPlot({
    d <- df_filt()
    x <- d[[input$uni_var_cont]]
    ggplot(d, aes_string(x = input$uni_var_cont)) +
      geom_histogram(aes(y = after_stat(density)),
                     fill = "#C0392B", alpha = 0.75, color = "#fff", bins = 30) +
      geom_density(color = "#1A1A2E", linewidth = 1.1, linetype = "dashed") +
      geom_vline(xintercept = mean(x), color = "#E67E22", linewidth = 1,
                 linetype = "solid") +
      geom_vline(xintercept = median(x), color = "#2980B9", linewidth = 1,
                 linetype = "dashed") +
      annotate("text", x = mean(x), y = Inf, label = "Moy.", vjust = 2,
               hjust = -0.2, color = "#E67E22", size = 3.5, fontface = "bold") +
      annotate("text", x = median(x), y = Inf, label = "Méd.", vjust = 3.5,
               hjust = -0.2, color = "#2980B9", size = 3.5, fontface = "bold") +
      labs(x = input$uni_var_cont, y = "Densité") +
      theme_cardio()
  }, bg = "#FFFFFF")
  
  # ── Boxplot univarié ─────────────────────────────────────
  output$uni_boxplot <- renderPlot({
    d <- df_filt()
    ggplot(d, aes_string(x = "1", y = input$uni_var_cont)) +
      geom_boxplot(fill = "#C0392B", alpha = 0.7, color = "#555",
                   outlier.color = "#E67E22", outlier.size = 2.5, width = 0.35) +
      geom_jitter(width = 0.08, alpha = 0.25, color = "#C0392B", size = 1) +
      labs(y = input$uni_var_cont, x = "") +
      scale_x_continuous(labels = NULL, breaks = NULL) +
      coord_flip() +
      theme_cardio()
  }, bg = "#FFFFFF")
  
  # ── Tableau de fréquences (catégorielles) ────────────────
  output$uni_freq_table <- renderTable({
    d    <- df_filt()
    freq <- as.data.frame(table(d[[input$uni_var_cat]]))
    names(freq) <- c("Modalité", "Effectif")
    freq$`%`        <- paste0(round(freq$Effectif / sum(freq$Effectif) * 100, 1), " %")
    freq$`% cumulé` <- paste0(cumsum(round(freq$Effectif / sum(freq$Effectif) * 100, 1)), " %")
    freq
  }, striped = TRUE, hover = TRUE, bordered = FALSE, rownames = FALSE, align = "lrrr")
  
  # ── Barplot univarié ─────────────────────────────────────
  output$uni_barplot <- renderPlot({
    d    <- df_filt()
    freq <- as.data.frame(table(d[[input$uni_var_cat]]))
    names(freq) <- c("Modalite", "Effectif")
    freq$Pct <- round(freq$Effectif / sum(freq$Effectif) * 100, 1)
    ggplot(freq, aes(x = reorder(Modalite, -Effectif), y = Effectif, fill = Modalite)) +
      geom_col(alpha = 0.85, show.legend = FALSE) +
      geom_text(aes(label = paste0(Effectif, "\n(", Pct, "%)")),
                vjust = -0.4, color = "#555", size = 3.5) +
      scale_fill_manual(values = pal_accent) +
      labs(x = input$uni_var_cat, y = "Effectif") + theme_cardio()
  }, bg = "#FFFFFF")
  
  # ── Pie chart univarié ───────────────────────────────────
  output$uni_pieplot <- renderPlot({
    d    <- df_filt()
    freq <- as.data.frame(table(d[[input$uni_var_cat]]))
    names(freq) <- c("Modalite", "Effectif")
    freq$Pct   <- round(freq$Effectif / sum(freq$Effectif) * 100, 1)
    freq$label <- paste0(freq$Modalite, "\n", freq$Pct, "%")
    ggplot(freq, aes(x = "", y = Effectif, fill = Modalite)) +
      geom_col(color = "#fff", linewidth = 0.5) +
      coord_polar(theta = "y") +
      geom_text(aes(label = label),
                position = position_stack(vjust = 0.5),
                color = "#fff", size = 3.8, fontface = "bold") +
      scale_fill_manual(values = pal_accent) +
      labs(fill = input$uni_var_cat) +
      theme_void() +
      theme(
        legend.text  = element_text(color = "#444", size = 11),
        legend.title = element_text(color = "#C0392B", size = 11, face = "bold")
      )
  }, bg = "#FFFFFF")
  
  # ══════════════════════════════════════════════════════════
  #  ONGLET 3 — BIVARIÉ
  # ══════════════════════════════════════════════════════════
  
  # ── 3a Quanti × Quanti ──────────────────────────────────
  
  output$biv_qq_cor_table <- renderTable({
    d  <- df_filt()
    xv <- d[[input$biv_qq_x]]
    yv <- d[[input$biv_qq_y]]
    data.frame(
      Méthode  = c("Pearson", "Spearman"),
      r        = round(c(cor(xv, yv, use = "complete.obs"),
                         cor(xv, yv, method = "spearman", use = "complete.obs")), 3)
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE, rownames = FALSE)
  
  output$biv_qq_test <- renderPrint({
    d <- df_filt()
    cor.test(d[[input$biv_qq_x]], d[[input$biv_qq_y]])
  })
  
  output$biv_qq_concl <- renderText({
    d <- df_filt()
    p <- cor.test(d[[input$biv_qq_x]], d[[input$biv_qq_y]])$p.value
    r <- cor(d[[input$biv_qq_x]], d[[input$biv_qq_y]], use = "complete.obs")
    if (p < 0.05) {
      paste0("p = ", signif(p, 3), " < 0.05 → REJETTE H₀. Corrélation significative (r = ",
             round(r, 3), ").")
    } else {
      paste0("p = ", signif(p, 3), " ≥ 0.05 → On ne rejette pas H₀. Pas de corrélation significative.")
    }
  })
  
  output$biv_qq_scatter <- renderPlot({
    d <- df_filt()
    g <- ggplot(d, aes_string(x = input$biv_qq_x, y = input$biv_qq_y)) +
      geom_point(alpha = 0.55, color = "#C0392B", size = 2.2) +
      labs(x = input$biv_qq_x, y = input$biv_qq_y) + theme_cardio()
    if (input$biv_qq_lm)
      g <- g + geom_smooth(method = "lm", se = TRUE,
                           color = "#2980B9", fill = "#2980B9",
                           alpha = 0.15, linewidth = 1.2)
    g
  }, bg = "#FFFFFF")
  
  output$biv_qq_hist_x <- renderPlot({
    d <- df_filt()
    ggplot(d, aes_string(x = input$biv_qq_x)) +
      geom_histogram(fill = "#C0392B", alpha = 0.8, color = "#fff", bins = 25) +
      labs(x = input$biv_qq_x, y = "Effectif") + theme_cardio()
  }, bg = "#FFFFFF")
  
  output$biv_qq_hist_y <- renderPlot({
    d <- df_filt()
    ggplot(d, aes_string(x = input$biv_qq_y)) +
      geom_histogram(fill = "#2980B9", alpha = 0.8, color = "#fff", bins = 25) +
      labs(x = input$biv_qq_y, y = "Effectif") + theme_cardio()
  }, bg = "#FFFFFF")
  
  # ── 3b Quanti × Quali ───────────────────────────────────
  
  output$biv_ql_desc <- renderTable({
    d <- df_filt()
    d %>%
      group_by(.data[[input$biv_ql_qual]]) %>%
      summarise(
        n    = n(),
        Moy  = round(mean(.data[[input$biv_ql_quant]], na.rm = TRUE), 1),
        SD   = round(sd(.data[[input$biv_ql_quant]],  na.rm = TRUE), 1),
        Q1   = round(quantile(.data[[input$biv_ql_quant]], 0.25), 1),
        Méd  = round(median(.data[[input$biv_ql_quant]], na.rm = TRUE), 1),
        Q3   = round(quantile(.data[[input$biv_ql_quant]], 0.75), 1),
        .groups = "drop"
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE, rownames = FALSE)
  
  output$biv_ql_test <- renderPrint({
    d    <- df_filt()
    ngrp <- length(unique(d[[input$biv_ql_qual]]))
    form <- as.formula(paste(input$biv_ql_quant, "~", input$biv_ql_qual))
    if (ngrp == 2) {
      t.test(form, data = d)
    } else {
      summary(aov(form, data = d))
    }
  })
  
  output$biv_ql_concl <- renderText({
    d    <- df_filt()
    ngrp <- length(unique(d[[input$biv_ql_qual]]))
    form <- as.formula(paste(input$biv_ql_quant, "~", input$biv_ql_qual))
    if (ngrp == 2) {
      p <- t.test(form, data = d)$p.value
      test_name <- "t-test"
    } else {
      p <- summary(aov(form, data = d))[[1]][["Pr(>F)"]][1]
      test_name <- "ANOVA"
    }
    if (p < 0.05) {
      paste0(test_name, " : p = ", signif(p, 3),
             " < 0.05 → REJETTE H₀. ",
             input$biv_ql_quant, " dépend significativement de ", input$biv_ql_qual, ".")
    } else {
      paste0(test_name, " : p = ", signif(p, 3),
             " ≥ 0.05 → On ne rejette pas H₀. Pas de différence significative.")
    }
  })
  
  output$biv_ql_box <- renderPlot({
    d <- df_filt()
    means <- d %>%
      group_by(.data[[input$biv_ql_qual]]) %>%
      summarise(moy = mean(.data[[input$biv_ql_quant]], na.rm = TRUE), .groups = "drop")
    ggplot(d, aes_string(x = input$biv_ql_qual, y = input$biv_ql_quant,
                         fill = input$biv_ql_qual)) +
      geom_boxplot(alpha = 0.75, color = "#555", outlier.color = "#C0392B") +
      geom_point(data = means, aes(x = .data[[input$biv_ql_qual]], y = moy),
                 shape = 23, size = 4, fill = "#fff", color = "#1A1A2E") +
      scale_fill_manual(values = pal_accent) +
      labs(x = input$biv_ql_qual, y = input$biv_ql_quant,
           caption = "◇ = moyenne par groupe") +
      theme_cardio() + theme(legend.position = "none")
  }, bg = "#FFFFFF")
  
  output$biv_ql_density <- renderPlot({
    d <- df_filt()
    ggplot(d, aes_string(x = input$biv_ql_quant, fill = input$biv_ql_qual,
                         color = input$biv_ql_qual)) +
      geom_density(alpha = 0.3, linewidth = 1) +
      scale_fill_manual(values  = pal_accent) +
      scale_color_manual(values = pal_accent) +
      labs(x = input$biv_ql_quant, y = "Densité",
           fill = input$biv_ql_qual, color = input$biv_ql_qual) +
      theme_cardio()
  }, bg = "#FFFFFF")
  
  # ── 3c Quali × Quali ────────────────────────────────────
  
  output$biv_cc_table <- renderTable({
    d <- df_filt()
    t <- as.data.frame.matrix(table(d[[input$biv_cc_x]], d[[input$biv_cc_y]]))
    t <- cbind(Modalité = rownames(t), t)
    t
  }, striped = TRUE, hover = TRUE, bordered = FALSE, rownames = FALSE)
  
  output$biv_cc_chi2 <- renderPrint({
    d <- df_filt()
    tbl <- table(d[[input$biv_cc_x]], d[[input$biv_cc_y]])
    # Vérifier si attendus > 5
    chi2 <- chisq.test(tbl, correct = FALSE)
    chi2
  })
  
  output$biv_cc_concl <- renderText({
    d <- df_filt()
    tbl <- table(d[[input$biv_cc_x]], d[[input$biv_cc_y]])
    p   <- chisq.test(tbl, correct = FALSE)$p.value
    if (p < 0.05) {
      paste0("p = ", signif(p, 3),
             " < 0.05 → REJETTE H₀. ",
             input$biv_cc_x, " et ", input$biv_cc_y, " sont significativement liés.")
    } else {
      paste0("p = ", signif(p, 3),
             " ≥ 0.05 → On ne rejette pas H₀. Pas d'association significative.")
    }
  })
  
  output$biv_cc_bar_fill <- renderPlot({
    d <- df_filt()
    ggplot(d, aes_string(x = input$biv_cc_x, fill = input$biv_cc_y)) +
      geom_bar(position = "fill", alpha = 0.85, color = "#fff") +
      scale_fill_manual(values = pal_accent) +
      scale_y_continuous(labels = percent_format()) +
      labs(x = input$biv_cc_x, y = "Proportion",
           fill = input$biv_cc_y) + theme_cardio()
  }, bg = "#FFFFFF")
  
  output$biv_cc_bar_dodge <- renderPlot({
    d <- df_filt()
    ggplot(d, aes_string(x = input$biv_cc_x, fill = input$biv_cc_y)) +
      geom_bar(position = "dodge", alpha = 0.85, color = "#fff") +
      scale_fill_manual(values = pal_accent) +
      labs(x = input$biv_cc_x, y = "Effectif", fill = input$biv_cc_y) +
      theme_cardio()
  }, bg = "#FFFFFF")
  
  # ══════════════════════════════════════════════════════════
  #  ONGLET 4 — MULTIVARIÉ
  # ══════════════════════════════════════════════════════════
  
  # ── Corrélation ─────────────────────────────────────────
  output$plot_corr <- renderPlot({
    d       <- df_filt()[, vars_quanti]
    cor_mat <- cor(d, use = "complete.obs")
    ggcorrplot(cor_mat,
               method        = "square",
               type          = "lower",
               lab           = TRUE,
               lab_size      = 3.5,
               colors        = c("#2980B9", "#f5f5f5", "#C0392B"),
               outline.color = "#ffffff",
               ggtheme       = theme_cardio()
    ) + labs(title = "")
  }, bg = "#FFFFFF")
  
  # ── Nuage de points ─────────────────────────────────────
  output$plot_scatter <- renderPlot({
    d <- df_filt()
    if (input$scatter_col != "Aucune") {
      g <- ggplot(d, aes_string(x = input$scatter_x, y = input$scatter_y,
                                color = input$scatter_col)) +
        scale_color_manual(values = pal_accent)
    } else {
      g <- ggplot(d, aes_string(x = input$scatter_x, y = input$scatter_y)) +
        aes(color = I("#C0392B"))
    }
    g <- g + geom_point(alpha = 0.55, size = 2) +
      labs(x = input$scatter_x, y = input$scatter_y) + theme_cardio()
    if (input$scatter_lm)
      g <- g + geom_smooth(method = "lm", se = TRUE,
                           color = "#E67E22", fill = "#E67E22",
                           alpha = 0.15, linewidth = 1.2)
    g
  }, bg = "#FFFFFF")
  
  # ── ACP ──────────────────────────────────────────────────
  res_acp <- reactive({
    d <- df_filt()[, vars_quanti]
    if (input$acp_quali_sup != "Aucune") {
      d_sup <- df_filt()[[input$acp_quali_sup]]
      d_full <- cbind(d, quali_sup = d_sup)
      PCA(d_full, quali.sup = 6, graph = FALSE)
    } else {
      PCA(d, graph = FALSE)
    }
  })
  
  output$acp_eig_table <- renderTable({
    eig <- as.data.frame(res_acp()$eig)
    eig$Dim <- paste0("Dim.", seq_len(nrow(eig)))
    eig <- eig[, c("Dim", "eigenvalue", "percentage of variance", "cumulative percentage of variance")]
    names(eig) <- c("Dim.", "Val. propre", "% variance", "% cumulé")
    eig$`Val. propre` <- round(eig$`Val. propre`, 3)
    eig$`% variance`  <- paste0(round(eig$`% variance`, 1), " %")
    eig$`% cumulé`    <- paste0(round(eig$`% cumulé`,  1), " %")
    eig
  }, striped = TRUE, hover = TRUE, bordered = FALSE, rownames = FALSE)
  
  output$acp_plot <- renderPlot({
    res  <- res_acp()
    type <- input$acp_plot_type
    col_sup <- if (input$acp_quali_sup != "Aucune") {
      as.factor(df_filt()[[input$acp_quali_sup]])
    } else NULL
    
    if (type == "eig") {
      fviz_eig(res, addlabels = TRUE, ylim = c(0, 65),
               barfill = "#C0392B", barcolor = "#922B21",
               linecolor = "#1A1A2E") +
        theme_cardio() + labs(title = "Éboulis des valeurs propres")
      
    } else if (type == "var") {
      fviz_pca_var(res, col.var = "contrib",
                   gradient.cols = c("#2980B9", "#f5f5f5", "#C0392B"),
                   repel = TRUE, ggtheme = theme_cardio()) +
        labs(title = "Graphe des variables (ACP)")
      
    } else if (type == "ind") {
      if (!is.null(col_sup)) {
        fviz_pca_ind(res, col.ind = col_sup, palette = pal_accent,
                     addEllipses = TRUE, ellipse.type = "confidence",
                     legend.title = input$acp_quali_sup,
                     repel = TRUE, ggtheme = theme_cardio()) +
          labs(title = "Graphe des individus (ACP)")
      } else {
        fviz_pca_ind(res, col.ind = "#C0392B", repel = TRUE,
                     ggtheme = theme_cardio()) +
          labs(title = "Graphe des individus (ACP)")
      }
      
    } else if (type == "biplot") {
      if (!is.null(col_sup)) {
        fviz_pca_biplot(res, col.var = "#1A1A2E", col.ind = col_sup,
                        palette = pal_accent, addEllipses = TRUE,
                        legend.title = input$acp_quali_sup,
                        repel = TRUE, ggtheme = theme_cardio()) +
          labs(title = "Biplot ACP")
      } else {
        fviz_pca_biplot(res, col.var = "#1A1A2E", col.ind = "#C0392B",
                        repel = TRUE, ggtheme = theme_cardio()) +
          labs(title = "Biplot ACP")
      }
    }
  }, bg = "#FFFFFF")
  
  # ── ACM ──────────────────────────────────────────────────
  res_acm <- reactive({
    d <- df_filt()[, vars_quali]
    # Convertir en facteurs
    d <- d %>% mutate(across(everything(), as.factor))
    if (input$acm_quali_sup != "Aucune") {
      idx_sup <- which(vars_quali == input$acm_quali_sup)
      MCA(d, quali.sup = idx_sup, graph = FALSE)
    } else {
      MCA(d, graph = FALSE)
    }
  })
  
  output$acm_eig_table <- renderTable({
    eig <- as.data.frame(res_acm()$eig)
    eig$Dim <- paste0("Dim.", seq_len(nrow(eig)))
    eig <- eig[, c("Dim", "eigenvalue", "percentage of variance", "cumulative percentage of variance")]
    names(eig) <- c("Dim.", "Val. propre", "% variance", "% cumulé")
    eig$`Val. propre` <- round(eig$`Val. propre`, 3)
    eig$`% variance`  <- paste0(round(eig$`% variance`, 1), " %")
    eig$`% cumulé`    <- paste0(round(eig$`% cumulé`,  1), " %")
    head(eig, 8)
  }, striped = TRUE, hover = TRUE, bordered = FALSE, rownames = FALSE)
  
  output$acm_plot <- renderPlot({
    res  <- res_acm()
    type <- input$acm_plot_type
    
    if (type == "eig") {
      fviz_eig(res, addlabels = TRUE,
               barfill = "#C0392B", barcolor = "#922B21",
               linecolor = "#1A1A2E") +
        theme_cardio() + labs(title = "Éboulis des valeurs propres (ACM)")
      
    } else if (type == "eig_kaiser") {
      eig_vals <- res$eig[, 1]
      eig_moy  <- mean(eig_vals)
      df_eig   <- data.frame(Dim = seq_along(eig_vals), VP = eig_vals)
      ggplot(df_eig, aes(x = Dim, y = VP)) +
        geom_line(color = "#C0392B", linewidth = 1) +
        geom_point(color = "#C0392B", size = 3) +
        geom_hline(yintercept = eig_moy, linetype = "dashed",
                   color = "#2980B9", linewidth = 1) +
        annotate("text", x = max(df_eig$Dim) * 0.8, y = eig_moy * 1.08,
                 label = paste0("Moyenne = ", round(eig_moy, 4)),
                 color = "#2980B9", size = 4) +
        labs(title = "Éboulis des valeurs propres + règle de Kaiser",
             x = "Dimension", y = "Valeur propre") +
        theme_cardio()
      
    } else if (type == "biplot") {
      fviz_mca_biplot(res, repel = TRUE,
                      col.var = "#C0392B", col.ind = "#aaa",
                      ggtheme = theme_cardio()) +
        labs(title = "Biplot ACM (individus + modalités)")
      
    } else if (type == "ind_ellipse") {
      hab_var <- if (input$acm_quali_sup != "Aucune") input$acm_quali_sup else vars_quali[1]
      fviz_mca_ind(res,
                   habillage  = as.factor(df_filt()[[hab_var]]),
                   addEllipses = TRUE, ellipse.level = 0.70,
                   repel = TRUE,
                   palette = pal_accent,
                   legend.title = hab_var,
                   ggtheme = theme_cardio()) +
        labs(title = paste0("Individus ACM (ellipses par ", hab_var, ")"))
      
    } else if (type == "contrib1") {
      fviz_contrib(res, choice = "ind", axes = 1, top = 20,
                   fill = "#C0392B", color = "#922B21") +
        theme_cardio() + labs(title = "Contributions des individus — Dim. 1")
      
    } else if (type == "contrib2") {
      fviz_contrib(res, choice = "ind", axes = 2, top = 20,
                   fill = "#2980B9", color = "#1A5276") +
        theme_cardio() + labs(title = "Contributions des individus — Dim. 2")
    }
  }, bg = "#FFFFFF")
  
  # ══════════════════════════════════════════════════════════
  #  ONGLET 5 — TABLEAU
  # ══════════════════════════════════════════════════════════
  
  output$table_data <- renderDT({
    d <- df_filt()
    d$Income <- paste0(format(d$Income, big.mark = " ", scientific = FALSE), " $")
    datatable(
      d,
      options = list(
        pageLength = 15,
        scrollX    = TRUE,
        dom        = "Bfrtip",
        language   = list(
          search     = "Rechercher :",
          lengthMenu = "Afficher _MENU_ lignes",
          info       = "Lignes _START_ à _END_ sur _TOTAL_",
          paginate   = list(previous = "Préc.", `next` = "Suiv.")
        )
      ),
      rownames = FALSE,
      class    = "compact hover"
    ) %>%
      formatStyle("Income",
                  fontWeight = "bold", color = "#C0392B") %>%
      formatStyle("Gender",
                  color = styleEqual(c("Femme", "Homme"), c("#E67E22", "#2980B9"))) %>%
      formatStyle("Product",
                  fontWeight = "bold",
                  color = styleEqual(
                    c("TM195", "TM498", "TM798"),
                    c("#C0392B", "#8E44AD", "#27AE60")
                  ))
  })
  
  output$nb_lignes_affichees <- renderText({
    paste0(nrow(df_filt()), " observations affichées après filtrage")
  })
  
  output$dl_csv <- downloadHandler(
    filename = function() paste0("cardio_filtre_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(df_filt(), file, row.names = FALSE)
  )
}

# ══════════════════════════════════════════════════════════════
shinyApp(ui, server)
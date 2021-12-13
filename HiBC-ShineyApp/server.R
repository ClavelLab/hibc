#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(DBI)
library(RSQLite)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
library(DT)
# Define server logic required to draw a histogram




shinyServer(function(input, output) {db <- dbConnect(RSQLite::SQLite(), dbname = "hiBC.db")


prodtype <- dbGetQuery(
  db,
  statement = 'SELECT i.Clavel_Lab_ID from ISOLATE i
 left join TAXONOMY t on t.pk = i.FK_TAXONOMY
 left join TAX_FAM tf on tf.pk = t.FK_TAX_FAM
 left join TAX_SPE ts on ts.pk = t.FK_TAX_SPE
 left join TAX_GEN tg on tg.pk = t.FK_TAX_GEN
 left join TAX_PHY tp on tp.pk = tf.FK_TAX_PHY
 left join TAX_FAM tf2 on tf2.pk = tg.FK_TAX_FAM
 left join TAX_PHY tp2 on tp2.pk = tf2.FK_TAX_PHY
 left join GENOME g on g.pk = i.FK_GENOME
 left join True_False true on true.pk = g.FK_GENOME_PRESENT
 left join DSMZ d on d.PK = i.FK_DSMZ
 left join ISOLATION iso on iso.pk = i.FK_ISOLATION 
 left join Location loc on loc.pk = iso.FK_LOCATION
 left join Country c on c.pk = loc.FK_COUNTRY
 left join rRNA rna on rna.pk = i.FK_rRNA
 
 left join ISOLATION_MEDIA iso_med on iso_med.FK_ISOLATION = iso.pk
 left join MEDIA m on m.pk = iso_med.FK_MEDIA
 
 left join MALDI mal on mal.pk = i.FK_MALDI')


output$nIsolates <- renderText(
  paste0('<h4>', 
         dbGetQuery(
           conn = db, 
           statement = 
             'SELECT COUNT(i.Clavel_Lab_ID) from ISOLATE i
 left join TAXONOMY t on t.pk = i.FK_TAXONOMY
 left join TAX_FAM tf on tf.pk = t.FK_TAX_FAM
 left join TAX_SPE ts on ts.pk = t.FK_TAX_SPE
 left join TAX_GEN tg on tg.pk = t.FK_TAX_GEN
 left join TAX_PHY tp on tp.pk = tf.FK_TAX_PHY
 left join TAX_FAM tf2 on tf2.pk = tg.FK_TAX_FAM
 left join TAX_PHY tp2 on tp2.pk = tf2.FK_TAX_PHY
 left join GENOME g on g.pk = i.FK_GENOME
 left join True_False true on true.pk = g.FK_GENOME_PRESENT
 left join DSMZ d on d.PK = i.FK_DSMZ
 left join ISOLATION iso on iso.pk = i.FK_ISOLATION 
 left join Location loc on loc.pk = iso.FK_LOCATION
 left join Country c on c.pk = loc.FK_COUNTRY
 left join rRNA rna on rna.pk = i.FK_rRNA
 
 left join ISOLATION_MEDIA iso_med on iso_med.FK_ISOLATION = iso.pk
 left join MEDIA m on m.pk = iso_med.FK_MEDIA
 
 left join MALDI mal on mal.pk = i.FK_MALDI'         ), 
         '</h4>'))


output$nPhyla <- renderText(
  paste0('<h4>', 
         dbGetQuery(
           conn = db, 
           statement = 
             'SELECT COUNT(DISTINCT(tp2.Phylum))
  from ISOLATE i
 left join TAXONOMY t on t.pk = i.FK_TAXONOMY
 left join TAX_FAM tf on tf.pk = t.FK_TAX_FAM
 left join TAX_SPE ts on ts.pk = t.FK_TAX_SPE
 left join TAX_GEN tg on tg.pk = t.FK_TAX_GEN
 left join TAX_PHY tp on tp.pk = tf.FK_TAX_PHY
 left join TAX_FAM tf2 on tf2.pk = tg.FK_TAX_FAM
 left join TAX_PHY tp2 on tp2.pk = tf2.FK_TAX_PHY
 left join GENOME g on g.pk = i.FK_GENOME
 left join True_False true on true.pk = g.FK_GENOME_PRESENT
 left join DSMZ d on d.PK = i.FK_DSMZ
 left join ISOLATION iso on iso.pk = i.FK_ISOLATION 
 left join Location loc on loc.pk = iso.FK_LOCATION
 left join Country c on c.pk = loc.FK_COUNTRY
 left join rRNA rna on rna.pk = i.FK_rRNA
 
 left join ISOLATION_MEDIA iso_med on iso_med.FK_ISOLATION = iso.pk
 left join MEDIA m on m.pk = iso_med.FK_MEDIA
 
 left join MALDI mal on mal.pk = i.FK_MALDI'         ), 
         '</h4>'))


output$nSpecies <- renderText(
  paste0('<h4>', 
         dbGetQuery(
           conn = db, 
           statement = 
             'SELECT COUNT(DISTINCT(i.FK_TAXONOMY))
  from ISOLATE i
 left join TAXONOMY t on t.pk = i.FK_TAXONOMY
 left join TAX_FAM tf on tf.pk = t.FK_TAX_FAM
 left join TAX_SPE ts on ts.pk = t.FK_TAX_SPE
 left join TAX_GEN tg on tg.pk = t.FK_TAX_GEN
 left join TAX_PHY tp on tp.pk = tf.FK_TAX_PHY
 left join TAX_FAM tf2 on tf2.pk = tg.FK_TAX_FAM
 left join TAX_PHY tp2 on tp2.pk = tf2.FK_TAX_PHY
 left join GENOME g on g.pk = i.FK_GENOME
 left join True_False true on true.pk = g.FK_GENOME_PRESENT
 left join DSMZ d on d.PK = i.FK_DSMZ
 left join ISOLATION iso on iso.pk = i.FK_ISOLATION 
 left join Location loc on loc.pk = iso.FK_LOCATION
 left join Country c on c.pk = loc.FK_COUNTRY
 left join rRNA rna on rna.pk = i.FK_rRNA
 
 left join ISOLATION_MEDIA iso_med on iso_med.FK_ISOLATION = iso.pk
 left join MEDIA m on m.pk = iso_med.FK_MEDIA
 
 left join MALDI mal on mal.pk = i.FK_MALDI'         ), 
         '</h4>'))


output$nMedia <- renderText(
  paste0('<h4>', 
         dbGetQuery(
           conn = db, 
           statement = 
             'SELECT COUNT(DISTINCT(m.Medium)) from ISOLATE i
 left join TAXONOMY t on t.pk = i.FK_TAXONOMY
 left join TAX_FAM tf on tf.pk = t.FK_TAX_FAM
 left join TAX_SPE ts on ts.pk = t.FK_TAX_SPE
 left join TAX_GEN tg on tg.pk = t.FK_TAX_GEN
 left join TAX_PHY tp on tp.pk = tf.FK_TAX_PHY
 left join TAX_FAM tf2 on tf2.pk = tg.FK_TAX_FAM
 left join TAX_PHY tp2 on tp2.pk = tf2.FK_TAX_PHY
 left join GENOME g on g.pk = i.FK_GENOME
 left join True_False true on true.pk = g.FK_GENOME_PRESENT
 left join DSMZ d on d.PK = i.FK_DSMZ
 left join ISOLATION iso on iso.pk = i.FK_ISOLATION 
 left join Location loc on loc.pk = iso.FK_LOCATION
 left join Country c on c.pk = loc.FK_COUNTRY
 left join rRNA rna on rna.pk = i.FK_rRNA
 
 left join ISOLATION_MEDIA iso_med on iso_med.FK_ISOLATION = iso.pk
 left join MEDIA m on m.pk = iso_med.FK_MEDIA
 
 left join MALDI mal on mal.pk = i.FK_MALDI'         ), 
         '</h4>'))



output$nAddatives <- renderText(
  paste0('<h4>', 
         dbGetQuery(
           conn = db, 
           statement = 
             'SELECT COUNT(DISTINCT(m.Addition)) from ISOLATE i
 left join TAXONOMY t on t.pk = i.FK_TAXONOMY
 left join TAX_FAM tf on tf.pk = t.FK_TAX_FAM
 left join TAX_SPE ts on ts.pk = t.FK_TAX_SPE
 left join TAX_GEN tg on tg.pk = t.FK_TAX_GEN
 left join TAX_PHY tp on tp.pk = tf.FK_TAX_PHY
 left join TAX_FAM tf2 on tf2.pk = tg.FK_TAX_FAM
 left join TAX_PHY tp2 on tp2.pk = tf2.FK_TAX_PHY
 left join GENOME g on g.pk = i.FK_GENOME
 left join True_False true on true.pk = g.FK_GENOME_PRESENT
 left join DSMZ d on d.PK = i.FK_DSMZ
 left join ISOLATION iso on iso.pk = i.FK_ISOLATION 
 left join Location loc on loc.pk = iso.FK_LOCATION
 left join Country c on c.pk = loc.FK_COUNTRY
 left join rRNA rna on rna.pk = i.FK_rRNA
 
 left join ISOLATION_MEDIA iso_med on iso_med.FK_ISOLATION = iso.pk
 left join MEDIA m on m.pk = iso_med.FK_MEDIA
 
 left join MALDI mal on mal.pk = i.FK_MALDI'         ), 
         '</h4>'))



output$nCountries <- renderText(
  paste0('<h4>', 
         dbGetQuery(
           conn = db, 
           statement = 
             'SELECT COUNT(DISTINCT(c.Country)) from ISOLATE i
 left join TAXONOMY t on t.pk = i.FK_TAXONOMY
 left join TAX_FAM tf on tf.pk = t.FK_TAX_FAM
 left join TAX_SPE ts on ts.pk = t.FK_TAX_SPE
 left join TAX_GEN tg on tg.pk = t.FK_TAX_GEN
 left join TAX_PHY tp on tp.pk = tf.FK_TAX_PHY
 left join TAX_FAM tf2 on tf2.pk = tg.FK_TAX_FAM
 left join TAX_PHY tp2 on tp2.pk = tf2.FK_TAX_PHY
 left join GENOME g on g.pk = i.FK_GENOME
 left join True_False true on true.pk = g.FK_GENOME_PRESENT
 left join DSMZ d on d.PK = i.FK_DSMZ
 left join ISOLATION iso on iso.pk = i.FK_ISOLATION 
 left join Location loc on loc.pk = iso.FK_LOCATION
 left join Country c on c.pk = loc.FK_COUNTRY
 left join rRNA rna on rna.pk = i.FK_rRNA
 
 left join ISOLATION_MEDIA iso_med on iso_med.FK_ISOLATION = iso.pk
 left join MEDIA m on m.pk = iso_med.FK_MEDIA
 
 left join MALDI mal on mal.pk = i.FK_MALDI'         ), 
         '</h4>'))



output$Overview <- renderDataTable(
  data <- dbGetQuery(
    conn = db,
    statement = 
      'SELECT 
 i.Clavel_Lab_ID as "Lab ID",
    i.DSM as "DSMZ ID",
    d.State as "Status at DSMZ",
    
    CASE WHEN  ts.Species  IS NOT NULL
    /* the manually selected family is taken into account */
    THEN  tg.genus  ||" " ||  ts.Species 
    /* the automatically detected family based on the genus is displayed */
    ELSE   tf.family  ||" " ||  tg.genus 
    END AS "Currently assigned name" ,
    
    
    /* Taxonomic information */
    
    CASE WHEN  tp.Phylum IS NOT NULL
    /* the manually selected family is taken into account */
    THEN tp.Phylum
    /* the automatically detected family based on the genus is displayed */
    ELSE  tp2.Phylum
    END AS "Phylum" ,
    
    
    CASE WHEN  tf.family IS NOT NULL
    /* the manually selected family is taken into account */
    THEN tf.family
    /* the automatically detected family based on the genus is displayed */
    ELSE  tf2.Family
    END AS "Family" ,
    
    tg.genus as "Genus",
    ts.Species as "Species", /* Tax species for any reason  */
    
    
    
    
    /* Additional information */
    i.Risk_Group as "Risk group"
    
    
    
    
    
    from ISOLATE i
    left join TAXONOMY t on t.pk = i.FK_TAXONOMY
    left join TAX_FAM tf on tf.pk = t.FK_TAX_FAM
    left join TAX_SPE ts on ts.pk = t.FK_TAX_SPE
    left join TAX_GEN tg on tg.pk = t.FK_TAX_GEN
    left join TAX_PHY tp on tp.pk = tf.FK_TAX_PHY
    left join TAX_FAM tf2 on tf2.pk = tg.FK_TAX_FAM
    left join TAX_PHY tp2 on tp2.pk = tf2.FK_TAX_PHY
    left join GENOME g on g.pk = i.FK_GENOME
    left join True_False true on true.pk = g.FK_GENOME_PRESENT
    left join DSMZ d on d.PK = i.FK_DSMZ
    left join ISOLATION iso on iso.pk = i.FK_ISOLATION 
    left join Location loc on loc.pk = iso.FK_LOCATION
    left join Country c on c.pk = loc.FK_COUNTRY
    left join rRNA rna on rna.pk = i.FK_rRNA
    
    left join ISOLATION_MEDIA iso_med on iso_med.FK_ISOLATION = iso.pk
    left join MEDIA m on m.pk = iso_med.FK_MEDIA
    
    left join MALDI mal on mal.pk = i.FK_MALDI'))








output$CultivationDB <- renderDataTable(
  data <- dbGetQuery(
    conn = db,
    statement = 
      'SELECT 
 i.Clavel_Lab_ID as "Lab ID",
    i.DSM as "DSMZ ID",
    d.State as "Status at DSMZ",
    
    CASE WHEN  ts.Species  IS NOT NULL
    /* the manually selected family is taken into account */
    THEN  tg.genus  ||" " ||  ts.Species 
    /* the automatically detected family based on the genus is displayed */
    ELSE   tf.family  ||" " ||  tg.genus 
    END AS "Currently assigned name" ,
    
    
    /* Taxonomic information */
    
    CASE WHEN  tp.Phylum IS NOT NULL
    /* the manually selected family is taken into account */
    THEN tp.Phylum
    /* the automatically detected family based on the genus is displayed */
    ELSE  tp2.Phylum
    END AS "Phylum" ,
    
    
    CASE WHEN  tf.family IS NOT NULL
    /* the manually selected family is taken into account */
    THEN tf.family
    /* the automatically detected family based on the genus is displayed */
    ELSE  tf2.Family
    END AS "Family" ,
    
    tg.genus as "Genus",
    ts.Species as "Species", /* Tax species for any reason  */
    
    
    /* MALDI information */
    mal.Comment as "MALDI identification",
    i.MALDI_Score as "MALDI score",
    
    /* Additional information */
    i.Risk_Group as "Risk group",
    
    
    /* Media information */
    c.Country as "Country of origin",
    m.Medium as "Media type",
    m.Condition as "Media condition",
    m.Addition as "Media additives"
    
    
    
    
    from ISOLATE i
    left join TAXONOMY t on t.pk = i.FK_TAXONOMY
    left join TAX_FAM tf on tf.pk = t.FK_TAX_FAM
    left join TAX_SPE ts on ts.pk = t.FK_TAX_SPE
    left join TAX_GEN tg on tg.pk = t.FK_TAX_GEN
    left join TAX_PHY tp on tp.pk = tf.FK_TAX_PHY
    left join TAX_FAM tf2 on tf2.pk = tg.FK_TAX_FAM
    left join TAX_PHY tp2 on tp2.pk = tf2.FK_TAX_PHY
    left join GENOME g on g.pk = i.FK_GENOME
    left join True_False true on true.pk = g.FK_GENOME_PRESENT
    left join DSMZ d on d.PK = i.FK_DSMZ
    left join ISOLATION iso on iso.pk = i.FK_ISOLATION 
    left join Location loc on loc.pk = iso.FK_LOCATION
    left join Country c on c.pk = loc.FK_COUNTRY
    left join rRNA rna on rna.pk = i.FK_rRNA
    
    left join ISOLATION_MEDIA iso_med on iso_med.FK_ISOLATION = iso.pk
    left join MEDIA m on m.pk = iso_med.FK_MEDIA
    
    left join MALDI mal on mal.pk = i.FK_MALDI'))















output$GenomeDB <- renderDataTable(
  data <- dbGetQuery(
    conn = db,
    statement = 
      'SELECT 
 i.Clavel_Lab_ID as "Lab ID",

    CASE WHEN  ts.Species  IS NOT NULL
    /* the manually selected family is taken into account */
    THEN  tg.genus  ||" " ||  ts.Species 
    /* the automatically detected family based on the genus is displayed */
    ELSE   tf.family  ||" " ||  tg.genus 
    END AS "Currently assigned name" ,
    
    
    /* Taxonomic information */
    
    CASE WHEN  tp.Phylum IS NOT NULL
    /* the manually selected family is taken into account */
    THEN tp.Phylum
    /* the automatically detected family based on the genus is displayed */
    ELSE  tp2.Phylum
    END AS "Phylum" ,
    
    
    CASE WHEN  tf.family IS NOT NULL
    /* the manually selected family is taken into account */
    THEN tf.family
    /* the automatically detected family based on the genus is displayed */
    ELSE  tf2.Family
    END AS "Family" ,
    
    tg.genus as "Genus",
    ts.Species as "Species", /* Tax species for any reason  */
    
    
    /* 16S rRNA gene information */
    rna.Identity *100 as "Identity to closest match (%)",
    rna.Length as "Sequence length (bp)",
    rna.Comment as "Sequence",
    
    /* Genome information */
    True_False as "Genome available",
    g.Completeness as "Completeness",
    g.Contamination as "Contamination",
    g.Length as "Genome size (Mbp)",
    g.Comment as "Genome accession"
    
    
    from ISOLATE i
    left join TAXONOMY t on t.pk = i.FK_TAXONOMY
    left join TAX_FAM tf on tf.pk = t.FK_TAX_FAM
    left join TAX_SPE ts on ts.pk = t.FK_TAX_SPE
    left join TAX_GEN tg on tg.pk = t.FK_TAX_GEN
    left join TAX_PHY tp on tp.pk = tf.FK_TAX_PHY
    left join TAX_FAM tf2 on tf2.pk = tg.FK_TAX_FAM
    left join TAX_PHY tp2 on tp2.pk = tf2.FK_TAX_PHY
    left join GENOME g on g.pk = i.FK_GENOME
    left join True_False true on true.pk = g.FK_GENOME_PRESENT
    left join DSMZ d on d.PK = i.FK_DSMZ
    left join ISOLATION iso on iso.pk = i.FK_ISOLATION 
    left join Location loc on loc.pk = iso.FK_LOCATION
    left join Country c on c.pk = loc.FK_COUNTRY
    left join rRNA rna on rna.pk = i.FK_rRNA
    
    left join ISOLATION_MEDIA iso_med on iso_med.FK_ISOLATION = iso.pk
    left join MEDIA m on m.pk = iso_med.FK_MEDIA
    
    left join MALDI mal on mal.pk = i.FK_MALDI'))










})

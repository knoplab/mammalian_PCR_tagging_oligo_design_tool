## Konrad Herbst, Ag Knop
## 2018/12/13

library(httr)
library(stringr)
library(dplyr)
library(biomaRt)

## helper functions
###################
## function for fetching relevant information for given transcript numbers
get_transcripts <- function(transcript_ids, mart) {
  if(!any(str_detect(transcript_ids, "^ENS"))) stop("Not all ids start with 'ENS'.")
  transcripts <- getBM(attributes = c("ensembl_gene_id",
                                "ensembl_transcript_id",
                                "rank",
                                "chromosome_name",
                                "strand",
                                "cds_start",
                                "cds_end",
                                "genomic_coding_start"),
                 filters = "ensembl_transcript_id",
                 values = transcript_ids,
                 mart = mart)
  meta_transcripts <- getBM(attributes = c("ensembl_gene_id",
                                     "ensembl_transcript_id",
                                     "entrezgene",
                                     "uniprot_gn",
                                     "transcript_appris",
                                     "external_gene_name"),
                      filters = "ensembl_transcript_id",
                      values = transcript_ids,
                      mart = mart)
  meta_transcripts <- group_by(meta_transcripts, ensembl_transcript_id, entrezgene) %>%
    mutate(uniprot_gn = paste0(uniprot_gn, collapse = ",")) %>%
    ungroup %>% unique
  meta_transcripts <- full_join(meta_transcripts, transcripts, by = c("ensembl_gene_id" = "ensembl_gene_id",
                                                                      "ensembl_transcript_id" = "ensembl_transcript_id")) %>%
    arrange(ensembl_gene_id, ensembl_transcript_id, rank)
  meta_transcripts
}

## function to find last codon before STOP codon and annotate flanking coordinates
## TODO handling cases were STOP is split over three exons is missing
get_prestop_exon <- function(transcripts, flankSize = 200) {
  prestop_exons  <- transcripts %>%
    filter(!is.na(cds_start)) %>%
    mutate(cds_length = cds_end - cds_start + 1) %>%
    group_by(ensembl_transcript_id, entrezgene) %>%
    do({
      stop_exon <- max(.$rank)
      if(.$cds_length[.$rank==stop_exon]<4 & nrow(.)>1) { ## return 2nd-last coding exon in
                                                          ## special cases in which the last
                                                          ## coding exon only contains (part
                                                          ## of) the STOP codon
        ret <- .[.$rank==stop_exon-1,]
        ## modify cds_length to account for the STOP codon site
        ret$cds_length <- ret$cds_length + .$cds_length[.$rank==stop_exon]
        if(ret$strand<0){ ## handle cases when gene is on antisense strand ("-")
          ret$genomic_coding_start <- ret$genomic_coding_start - .$cds_length[.$rank==stop_exon]
        }
      } else {
        ret <- .[.$rank==stop_exon,]
      }
      ret
    }) %>%
    mutate(genomic_coding_end = genomic_coding_start + (cds_length - 1))
  prestop_exons <- group_by(prestop_exons, ensembl_transcript_id, entrezgene) %>%
    do({
      ret <- .
      if(.$strand>0){
        ret$strand <- "+"
        ret$genomic_stop <- ret$genomic_coding_end - 2
        ret$genomic_flank_start <- ret$genomic_stop - flankSize
        ret$genomic_flank_end <- ret$genomic_flank_start + flankSize*2 + 2
      } else {
        ret$strand <- "-"
        ret$genomic_stop <- ret$genomic_coding_start
        ret$genomic_flank_end <- ret$genomic_stop + flankSize + 2
        ret$genomic_flank_start <- ret$genomic_flank_end - flankSize*2 - 2
      }
      ret
    }) %>% ungroup
  prestop_exons
}

## function to format genomic interval for REST query
format_region <- function(row){
  str_c(row$chromosome_name,
        str_c(row$genomic_flank_start, "..", row$genomic_flank_end),
        if_else(row$strand == "+", "1", "-1"), sep=":")
}

## function to retrieve genomic sequence
get_genomic_interval <- function(transcripts_flank_row, server, species = "homo_sapiens"){
  ## if(nrow(transcripts_flank_row)>1) stop("More than one row given.")
  r <- GET(paste(server, "/sequence/region/", species, "/", format_region(transcripts_flank_row), "?",
                 sep = ""), content_type("text/plain"))
  stop_for_status(r)
  content(r)
}

## function to retrieve flanking genomic sequences
get_flanking_genomic <- function(transcript_ids, mart, rest_server, rest_species, flankSize = 200) {
  transcripts <- get_transcripts(transcript_ids, mart = mart)
  transcripts_flank <- get_prestop_exon(transcripts, flankSize)
  sapply(1:nrow(transcripts_flank), function(row) get_genomic_interval(transcripts_flank[row, ], rest_server, rest_species))
}

## usage / workflow
###################
#### used datasets
#ensembl = useMart("ensembl", dataset = "hsapiens_gene_ensembl")
#rest_server <- "https://rest.ensembl.org"
#rest_species <- "homo_sapiens"

#### catch transcript information
#example_ids <- c("ENST00000417874","ENST00000586375", "ENST00000640021", "ENST00000229239", "ENST00000566418", "ENST00000229195")
#get_flanking_genomic(example_ids, ensembl, rest_server, rest_species, flankSize = 200)


### compile list of matching species datasets between biomaRt and REST interface
#library(jsonlite)
#r <- GET(paste(rest_server, "/info/species?", sep = ""), content_type("text/json"))
#stop_for_status(r)
#species <- fromJSON(toJSON(content(r)))[[1]]

## for some reasons ENSEMBL has no matching species ids between the two datasets
## multiple rounds allow to match most of the rows
#gene_datasets <- listDatasets(useMart("ensembl"))
#match_species <- sapply(gene_datasets$version, function(.) str_subset(unlist(species$assembly), .))
#x <- which(sapply(match_species, length)==0)
#match_species[x] <- sapply(sapply(str_split(names(x), "_| |\\."), `[[`, 1), function(.) str_subset(unlist(species$assembly), regex(., ignore_case=TRUE)))
#x <- which(sapply(match_species, length)==0)
#gene_datasets[x,]
### still missing, matching by hand?
##                     dataset                                 description
## 38  eeuropaeus_gene_ensembl                    Hedgehog genes (eriEur1)
## 70  mgallopavo_gene_ensembl                  Turkey genes (Turkey_2.01)
## 103   pformosa_gene_ensembl Amazon molly genes (Poecilia_formosa-5.1.2)
## 120   saraneus_gene_ensembl                       Shrew genes (sorAra1)
## 130 tbelangeri_gene_ensembl                  Tree Shrew genes (tupBel1)
##                    version
## 38                 eriEur1
## 70             Turkey_2.01
## 103 Poecilia_formosa-5.1.2
## 120                sorAra1
## 130                tupBel1

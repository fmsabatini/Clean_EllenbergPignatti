library(tidyverse)
input <- read_lines("Pignatti_Ellenberg.txt")


#delete lines out of place
input <- input[!grepl(pattern="BRAUN-BLANQUETIA", x=input)]
input <- str_squish(input)
input <- input[!grepl(pattern="^$", x=input)] #delete empty lines
input <- input[!grepl(pattern="S. PIGNATTI, Valori di bioindicazione delle piante vascolari", x=input)]
input <- input[!grepl(pattern="Nome scientifico", x=input)]
input <- input[!grepl(pattern="Courtesy", x=input)]
input <- gsub(pattern = "\\.", replacement = "", x = input) #strip away points

## manually correct some errors in original publication
input <- gsub(pattern="An 980", replacement ="P Lian", x = input)
input <- gsub(pattern="Borzí", replacement ="Borzi", x = input)
input <- gsub(pattern="é", replacement ="e", x = input)
input <- gsub(pattern="(Eurasiat)3", replacement ="(Eurasiat)", x = input, fixed=T)
input <- gsub(pattern="(Sv) ", replacement =" ", x = input, fixed=T)


## parse columns
a <- input %>% 
  as_tibble() %>% 
  #parse species names
  separate(value, sep="(?<=[a-zA-Z])\\s*(?=[0-9])", into=c("Species", "Rest"), extra="merge") %>% 
  #parse Pignatti Code
  separate(Rest, sep="(?<=[0-9])\\s*(?=[a-zA-Z])", into=c("Code_Pignatti", "Rest"), extra="merge") %>% 
  #parse indicator values
  mutate(Rest=str_replace(Rest, pattern="\\(", replacement="- ")) %>% 
  mutate(Rest=str_replace(Rest, pattern="\\)", replacement="")) %>% 
  mutate(Rest=str_replace_all(Rest, pattern="\\s*X\\s*", replacement=" 00 ")) %>% 
  separate(Rest, sep="(?<=[a-zA-Z])\\s*(?=[0-9])", into=c("Rest", "All_codes"), extra="merge") %>% 
  mutate(All_codes=str_squish(All_codes)) %>% 
  separate(All_codes, sep="\\s+", into=c("L","T", "C", "U", "R", "N", "S")) %>% 
  mutate_at(.vars=vars(L:S), 
            .funs=list(~replace(., list=.=="00", values=NA))) %>% 
  # fix some inconsistencies
  mutate(Rest=str_replace(string=Rest, pattern="Np", replacement="Np Np")) %>% 
  mutate(Rest=str_replace(string=Rest, pattern="He", replacement="He He")) %>% 
  # Parse chorotypes
  separate(Rest, sep="\\s+|\\s+-\\s+", into=c("GF", "LF", "Corotipo"), extra="merge")


## Standardize chorotypes
main_choro <- c("CircumArtAlp"                            = "CircumArtAlp",
              "Afr"                                       = "Afr$",
              "Appenn-Balcan"                             = "Appenn-Balcan",
              "Art Alp"                                   = "Art Alp|Art:-Alp|ArtAlp",
              "Avv Naturalizz"             = "Avv Naturalizz|Avv NaturaliZZ",
              "Alpico"                                    = "Alpico",
              "Boreoatl"                                  = "Boreoatl",
              "Eurimedit"                                 = "Eurimedit",
              "Centroasiat"                       = "C-Asiat|Centroasiat",
              "Centroeurop"                  = "Centro-Europ|Centroeurop",
              "Circumbor"                                 = "Circumbor",
              "Coltiv"                                    = "Coltiv",
              "Cosmopol"                           = "Cosmop|Cosmopol",
              "Europ"                                     = "Europ",
              "Medit-Mont"           = "Medit-Mont|MeditMont|Medit-MonT",
              "Medit-Turan"                               = "Medit-Turan",
              "Medit-Atl"                                 = "Medit-Atl",
              "Medit-Nesic"                               = "Medit-Nesic",
              "Medit-Macarones"                          = "Medit-Macarones", 
              "Stenomedit"                    = "Stenomedit|StenomedIT", 
              "Endem"                                    = "Endem", 
              "Estalp-Dinar"                              = "Estalp-Dinar",
              "Eurasiat"                            = "Euras|Eurasiat",
              "Euroamer"                                  = "Euroamer",
              "Europ"                                     = "Europ",
              "Eurosib"                           = "Eurosib|EurosIB",
              "Hybrid Cult"                               = "Hybrid Cult",
              "Eurimedit"                                 = "Eurimedit",
              "Neotropic"                                 = "Neotropic",
              "Orof"                                      = "Orof",
              "Paleosubtrop"                              = "Paleosubtrop",
              "Paleotemp"                                 = "Paleotemp",
              "Paleotrop"                                 = "Paleotrop",
              "Pantrop"                                   = "Pantrop",
              "Pontica"                                  = "^Pontica",
              "Saharo-Sind"                               = "Saharo-Sind",
              "Subatlant"                                 = "Subatlant",
              "Subcosmop"                                 = "Subcosmop",
              "Subendem"                                  = "Subendem",
              "Subtrop"                                   = "Subtrop",
              "Termocosmop"                               = "Termocosmop")

collapse_string=paste(paste0(paste0("`", names(main_choro), "` = c('", main_choro, "') "), ""), collapse=", ")
collapse_string <- gsub(collapse_string, pattern="\\|", replacement="', '")

b <- a %>% 
  mutate(Main_Coro=str_extract(a$Corotipo, pattern = paste(main_choro, collapse = "|"))) %>% 
  mutate(Main_Coro=factor(Main_Coro)) %>% 
  mutate(Main_Coro=fct_collapse(Main_Coro,
    `CircumArtAlp` = c('CircumArtAlp') , 
    `Afr` = c('Afr') , 
    `Appenn-Balcan` = c('Appenn-Balcan') , 
    `Art Alp` = c('Art Alp', 'Art:-Alp', 'ArtAlp') , 
    `Avv Naturalizz` = c('Avv Naturalizz', 'Avv NaturaliZZ') , 
    `Alpico` = c('Alpico') , 
    `Boreoatl` = c('Boreoatl') , 
    `Eurimedit` = c('Eurimedit') , 
    `Centroasiat` = c('C-Asiat', 'Centroasiat') ,
    `Centroeurop` = c('Centro-Europ', 'Centroeurop') , 
    `Circumbor` = c('Circumbor') , 
    `Coltiv` = c('Coltiv') , 
    `Cosmopol` = c('Cosmop', 'Cosmopol'), 
    `Europ` = c('Europ') , `Medit-Mont` = c('Medit-Mont', 'MeditMont', 'Medit-MonT') , `Medit-Turan` = c('Medit-Turan') , `Medit-Atl` = c('Medit-Atl') , `Medit-Nesic` = c('Medit-Nesic') , `Medit-Macarones` = c('Medit-Macarones') , `Stenomedit` = c('Stenomedit', 'StenomedIT') , `Endem` = c('Endem') , `Estalp-Dinar` = c('Estalp-Dinar') , `Eurasiat` = c('Euras', 'Eurasiat') , `Euroamer` = c('Euroamer') , `Europ` = c('Europ') , `Eurosib` = c('Eurosib', 'EurosIB') , `Hybrid Cult` = c('Hybrid Cult') , `Eurimedit` = c('Eurimedit') , `Neotropic` = c('Neotropic') , `Orof` = c('Orof') , `Paleosubtrop` = c('Paleosubtrop') , `Paleotemp` = c('Paleotemp') , `Paleotrop` = c('Paleotrop') , `Pantrop` = c('Pantrop') , `Pontica` = c('Pontica') , `Saharo-Sind` = c('Saharo-Sind') , `Subatlant` = c('Subatlant') , `Subcosmop` = c('Subcosmop') , `Subendem` = c('Subendem') , `Subtrop` = c('Subtrop') , `Termocosmop` = c('Termocosmop') 
    )) 


output <- b %>% 
  # separate binomial species names from authors
  separate(Species, sep=" ", into=c("Genus", "Sp", "author"), extra="merge") %>% 
  unite("SpecieS", Genus, Sp, sep=" ")

# export
write_csv(output, file="Ellenberg_Pignatti_out.csv")
      

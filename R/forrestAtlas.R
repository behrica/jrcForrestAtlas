#' @export
#' @importFrom magrittr %>%
getForrestTreesInformation <- function() {
    readr::read_csv(system.file(".","urls.csv",package = "jrcForrestAtlas")) %>%
        dplyr::rename(url=value) %>%
        dplyr::mutate(type=ifelse(grepl("map-data-RPP",url),"RPP","MHS")) %>%
        tidyr::separate(url,into=c("_1","_2","_3","_4","_5","_6","_7","_8","species"),sep="/",remove=F) %>%
        dplyr::mutate(species=stringr::str_replace_all(species,"-"," ")) %>%
        dplyr::select(-dplyr::starts_with("_"))
}

#' @export
getTreeSpeciesDistribution <- function(species_,type_) {
    url  <- getForrestTreesInformation() %>%
         dplyr::filter(species==species_,type==type_) %>%
        dplyr::pull(url)

    zip=tempfile(fileext=".zip")
    response=R.cache::memoizedCall(httr::GET,url,httr::progress())
    raw=httr::content(response,as="raw")
    writeBin(raw,zip)
    tifName=unzip(zip,list=T)  %>% dplyr::pull(Name)
    unzip(zip,exdir=tempdir())
    raster::raster(paste0(tempdir(),"/",tifName))
}


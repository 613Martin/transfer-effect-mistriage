#' Metrocheck
#'
#' Checks by Sjukhuskod if a case is metropolitan or not. Returns the data with
#' a new colum with "Yes" if metropolian, and "No" if not.
#' @param df A dataframe. Must contain a column "Sjukhuskod". No default.
Metrocheck <- function(df) {
    ## Error handling
    if (!is.data.frame(df))
        stop ("df has to be a data.frame")
    if (!("Sjukhuskod" %in% colnames(df)))
        stop ("df has to include the column Sjukhuskod")
    ## Assign metropolitan variable
    df$metropolitan <- ifelse(df$Sjukhuskod == 10011 | # St Görans sjukhus
                              df$Sjukhuskod == 10013 | # SöS
                              df$Sjukhuskod == 11002 | # Karolinska sjukhuset
                              df$Sjukhuskod == 11002 | # Huddinge sjukhus 
                              df$Sjukhuskod == 11003 | # Nya Karolinska
                              df$Sjukhuskod == 11010 | # DS 
                              df$Sjukhuskod == 11011 | # Södertälje sjukhus
                              df$Sjukhuskod == 11012 | # Norrtälje sjukhus
                              
                              df$Sjukhuskod == 51001 | # Sahlgrenska
                              df$Sjukhuskod == 51012 | # Kungälvs sjukhus
                              df$Sjukhuskod == 51013 | # NU sjukvården
                              df$Sjukhuskod == 51014 | # Södra Älvsborgs sjukhus
                              df$Sjukhuskod == 52012 | # Allingsås lasarett
                              
                              df$Sjukhuskod == 41001 | # Skånes universitetssjukhus
                              df$Sjukhuskod == 41011 | # Trelleborgs lasarett
                              df$Sjukhuskod == 11012   # Helsinborgs lasarett
                            , "Yes", "No")
    ## Return data.frame
    return(df)
}

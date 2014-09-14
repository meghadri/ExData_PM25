
fetchRemoteFile <- function(locRemote, dirDest, unzip = FALSE, localFName = NULL) {
    require(tools)
    
    if(!file.exists(dirDest)) 
        dir.create(dirDest)
    
    if(is.null(localFName) || is.na(localFName) || nchar(localFName) == 0 ) {
        fileExt <- file_ext(basename(URLdecode(locRemote)))
        destFile <- file_path_sans_ext(basename(URLdecode(locRemote)))
        destFile <- paste(dirDest, .Platform$file.sep, destFile, "_", as.Date(Sys.time()), ".", fileExt, sep="")
    } else {
        destFile = localFName
    }
    
    codeSucc <- download.file(locRemote, destFile, method = "curl")
    if(codeSucc != 0) 
        stop(paste("Unable to download file from :", locRemote))
    
    retVal = destFile
    
    if(isTRUE(unzip))
        retVal <- unzip(destFile, exdir=dirDest)
    
    retVal
    
}
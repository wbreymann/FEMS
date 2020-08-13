#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

## @import rJava
.onLoad <- function(libname, pkgname) {

    ## load java library in "java"-Path
    # .jpackage(pkgname, lib.loc = libname)
    ## ommit conversion of strings in data.frame to factor variables
    options(stringsAsFactors = FALSE)
    options("getSymbols.warning4.0"=FALSE)
    
}


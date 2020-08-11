#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

ActusDictionary <- NULL
ActusURL <- NULL
#' @import rJava
.onLoad <- function(libname, pkgname) {

    ## load java library in "java"-Path
    # .jpackage(pkgname, lib.loc = libname)
    ## ommit conversion of strings in data.frame to factor variables
    options(stringsAsFactors = FALSE)
    options("getSymbols.warning4.0"=FALSE)
    
    # also load the json containing the ActusDictionary
    load("./data/rflActusDictionary.RData", ActusDictionary <<- new.env(parent = emptyenv()))
    # ActusURL <<- "http://abanaxos.com:8080/"
    ActusURL <<- "http://ractus.ch:8080/"
}


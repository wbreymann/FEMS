library(jsonlite)
library(stringr)


convertActusDefaults <- function(actus_defaults) {

    # create a copy
    converted_defaults <- actus_defaults

    # find index of numercs, backslash's and empty
    idx_numeric <- grepl("[[:digit:]]",actus_defaults) &
                    !grepl("[[:alpha:]]",actus_defaults) &
                    !grepl("\n",actus_defaults)
    idx_slash_n <- grepl("\n",actus_defaults)
    idx_empty <- grepl("^\\s*$", actus_defaults)

    # convert the above
    converted_defaults[idx_slash_n] <-  substr(converted_defaults[idx_slash_n], 1,
                                               nchar(converted_defaults[idx_slash_n]) - 1)
    converted_defaults[idx_numeric] <- as.numeric(converted_defaults[idx_numeric])
    converted_defaults[idx_empty] <- "NULL"

    return(converted_defaults)
}


json_name <- "./data-raw/actus-dictionary.json"
json_body <- read_json(json_name)

# get logicals for necessary values & drop named list entry "contract"
rflActus_required <- lapply(
                    rapply(json_body$applicability,
                           function(x) length(grep("^NN$",x)) > 0, how = "replace"),
                           function(y) y[names(y) != "contract"]
                        )

# get default values for each of the variables...
rflActus_attributes <- lapply( rflActus_required, function(x) {
                                    terms_temp <- lapply( json_body$terms[names(x)] ,
                                                          function(y) unname(unlist(y["default"])))
                                    x <- convertActusDefaults(terms_temp)
                                })

# get allowed values for each attribute
rflActus_allowed_vals <- lapply( rflActus_required, function(x) {
                                        terms_temp <- lapply( json_body$terms[names(x)],
                                                              function(y) {
                                                                tryCatch({
                                                                  unlist(lapply(y[["allowedValues"]], function(z) z$acronym))
                                                                }, error = function(e) {
                                                                  unname(unlist(y["allowedValues"]))
                                                                })
                                                              })
                                        x <- terms_temp
                                    })

# spell all names of list with capitals
names(rflActus_required) <- paste0(toupper(substr(names(rflActus_required), 1, 1)),
                                     substr(names(rflActus_required), 2, nchar(names(rflActus_required))))
rflActus_required <- lapply(rflActus_required, function(x) {
  names(x) <- paste0(toupper(substr(names(x), 1, 1)),substr(names(x), 2, nchar(names(x))))
  x
  })
#
names(rflActus_attributes) <- paste0(toupper(substr(names(rflActus_attributes), 1, 1)),
                                     substr(names(rflActus_attributes), 2, nchar(names(rflActus_attributes))))
rflActus_attributes <- lapply(rflActus_attributes, function(x) {
  names(x) <- paste0(toupper(substr(names(x), 1, 1)),substr(names(x), 2, nchar(names(x))))
  x
})
#
names(rflActus_allowed_vals) <- paste0(toupper(substr(names(rflActus_allowed_vals), 1, 1)),
                                   substr(names(rflActus_allowed_vals), 2, nchar(names(rflActus_allowed_vals))))
rflActus_allowed_vals <- lapply(rflActus_allowed_vals, function(x) {
  names(x) <- paste0(toupper(substr(names(x), 1, 1)),substr(names(x), 2, nchar(names(x))))
  x
})

# save this into the data folder
save(list = c("rflActus_required", "rflActus_attributes","rflActus_allowed_vals"),
                        file = "./data/rflActusDictionary.RData")









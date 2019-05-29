#' Correct erroneous filetypes
#'
#' A Function to correct the file type of each file in the given directory. The function does not return anything - it changes the file endings on the hard drive.
#'
#' @param str The directory in which the files reside. Sub-directories will also be accessed. This folder has to be named \code{files_for_gov2text} on the system.
#' @export
#' @details The specified \code{path} has to be named \code{files_for_gov2text} to ensure that users do not accidentally put the wrong path here. Without this precaution, users might accidentally put a system-relevant path here (i.e. root), and since this function acts recursively, it might unintentionally alter important files. To ensure that this cannot happen, the directory in which files should be corrected HAS TO BE named \code{files_for_gov2text}. Note that it is still possible to use relative paths here. I.e. if \code{files_for_gov2text} is the current working directory, \code{path = "./"} would select it correctly.
correctFiletypes <- function(path){

  if(basename(tools::file_path_as_absolute(path))!="files_for_gov2text"){
    stop("Stopping. The specified directory is not named files_for_gov2text. To ensure that users don't damage their system, this is required. Please rename the folder accordingly. See the documentation for more information.")
  }

  #create a list of all files in all subdirectories
  #f <- list.files(path, recursive = T)
  #ignore files whose names can't be read
  #f <- f[!stringr::str_detect(f, "[^\\x00-\\x7F]")]
  #absolute file paths
  fAbs <- list.files(path, recursive = T, full.names = T)
  fAbs <- fAbs[!stringr::str_detect(fAbs, "[^\\x00-\\x7F]")]

  #determine file type with the wand package (relying on libmagic)
  magicResults <- wand::incant(fAbs)

  #get the file extensions as they currently are
  magicResults$ext <- tools::file_ext(magicResults$file)
  magicResults$extensions <- as.character(magicResults$extensions)

  #changes that need to be made:
  #files that currently have a pdf/txt/html/doc/docx extension and shouldn't have one
  #files that currently have a no extension and should have a pdf/txt/html/doc/docx
  #files that have the wrong pdf/txt/html/doc/docx extension

  magicResults$extensions <- stringr::str_replace_all(magicResults$extensions, "\"", "")
  magicResults$extensions <- stringr::str_replace_all(magicResults$extensions, "c\\(", "")
  magicResults$extensions <- stringr::str_replace_all(magicResults$extensions, "\\)", "")

  magicResults$extensions[magicResults$extensions=="ai, eps, ps"] <- "ps"
  magicResults$extensions[magicResults$extensions=="asf, asx"] <- "asf"
  magicResults$extensions[magicResults$extensions=="asm, s"] <- "asm"
  magicResults$extensions[magicResults$extensions=="bin, bpk, buffer, deb, deploy, dist, distz, dll, dmg, dms, dump, elc, exe, img, iso, lrf, mar, msi, msm, msp, pkg, so"] <- "bin"
  magicResults$extensions[magicResults$extensions=="conf, def, in, ini, list, log, text, txt"] <- "txt"
  magicResults$extensions[magicResults$extensions=="doc, dot"] <- "doc"
  magicResults$extensions[magicResults$extensions=="eml, mime"] <- "eml"
  magicResults$extensions[magicResults$extensions=="f, f77, f90, for"] <- "f"
  magicResults$extensions[magicResults$extensions=="htm, html, shtml"] <- "html"
  magicResults$extensions[magicResults$extensions=="ics, ifb"] <- "ics"
  magicResults$extensions[magicResults$extensions=="jpe, jpeg, jpg"] <- "jpg"
  magicResults$extensions[magicResults$extensions=="m1v, m2v, mpe, mpeg, mpg"] <- "mpeg"
  magicResults$extensions[magicResults$extensions=="m2a, m3a, mp2, mp2a, mp3, mpga"] <- "mp3"
  magicResults$extensions[magicResults$extensions=="mov, qt"] <- "mov"
  magicResults$extensions[magicResults$extensions=="mp4, mp4v, mpg4"] <- "mp4"
  magicResults$extensions[magicResults$extensions=="pot, pps, ppt"] <- "ppt"
  magicResults$extensions[magicResults$extensions=="rng, xml, xsd, xsl"] <- "xml"
  magicResults$extensions[magicResults$extensions=="svg, svgz"] <- "svg"
  magicResults$extensions[magicResults$extensions=="tif, tiff"] <- "tif"
  magicResults$extensions[magicResults$extensions=="ttc, ttf"] <- "ttf"
  magicResults$extensions[magicResults$extensions=="xla, xlc, xlm, xls, xlt, xlw"] <- "xls"

  #one exception: we don't change files to txt, because it would want to change js and css to text, which would be wrong
  magicResults$conflict <- F
  magicResults$conflict[magicResults$ext!=magicResults$extensions & magicResults$extensions!="txt"] <- T

  #file types
  folder <- stringr::str_split(magicResults$file, "\\/(?=[^\\/]+$)", simplify = T)[,1]
  filename <- stringr::str_split(magicResults$file, "\\/(?=[^\\/]+$)", simplify = T)[,2]

  #create the new file names, with the new extensions
  magicResults$newpath <- stringr::str_c(folder, "/",
                                         stringr::str_replace(filename, paste0("\\.", magicResults$ext), ""),
                                         paste0(".", magicResults$extensions))

  #exclude files whose names are too long
  magicResults$nChar <- nchar(magicResults$file)
  magicResults <- magicResults[magicResults$nChar<500,]

  #Print info
  print("Making the following changes:")
  print(paste(magicResults$file[magicResults$conflict==T], "  ----->  ", magicResults$extensions[magicResults$conflict==T]))

  #rename everything
  purrr::map2(magicResults$file[magicResults$conflict==T],
              magicResults$newpath[magicResults$conflict==T], file.rename)

}

##How to use:
# path = "/media/mneumann/ec574740-a4f4-4bd0-b624-6ff2c4ac59e9/testGovWebsitesPackage/cityofboonvilleindiana.com"
# correctFiletypes(path)

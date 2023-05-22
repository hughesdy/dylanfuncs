#' Pull NDA-specified NA value
#'
#' This function looks in NDA def files to figure out what value NDA wants missing data to be coded as
#' @param elementname
#' @keywords pullNAaction ndatools
#' @export
#' @examples
#' pullNAaction()
pullNAaction <- function(elementname) {
  value = def$Notes[which(def$ElementName==elementname)]
  if (value == '') {
    naVal = ''
    return(naVal)
  }
  split = strsplit(value, split = '')[[1]]
  npos=NULL
  i=1
  while (i!='exit') {
    if (i>length(split)) {
      print(paste(elementname, ':', " NA action not found", sep = ''))
      break
    }
    if (split[i] == 'N') {
      i=i+1
      if (split[i] == '/') {
        i=i+1
        if (split[i] == 'A') {
          npos = i-2
          i='exit'
        } else {
          i=i+1
        }
      } else if (split[i] == 'A') {
        npos = i-1
        i='exit'
      } else {
        i=i+1
      }
    } else if (tolower(split[i]) == 'm') {
      i=i+1
      if (i>length(split)) {
        break
      }
      if (split[i] == 'i') {
        i=i+1
        if (split[i]=='s') {
          i=i+1
          if (split[i]=='s') {
            npos=i-3
            i='exit'
          } else {
            i=i+1
          }
        } else {
          i=i+1
        }
      } else {
        i=i+1
      }
    } else {
      i=i+1
    }
  }

  if (!is.null(npos)) {
    if (';'%in%split) {
      semis = which(split==';')
      if (npos < min(semis)) {
        lowerRange = 0
      } else {
        lowerRange = max(semis[which(semis<npos)])
      }

    } else {
      lowerRange = 0
    }

    eqs = which(split=='=')
    upperRange = max(eqs[which(eqs<npos)])

    naString = paste(split[(lowerRange+1):(upperRange-1)])

    naVal = as.numeric(paste(naString, sep = '', collapse = ''))

    return(naVal)
  } else {
    return('NoNA')
  }

}

#' @title Data importer from plate reader
#' @description Function to import the reads of a time serie from the plate reader at the Dr Summers Lab.
#' @param file_list a list with the file paths in format .txt to be imported
#' @param wl vector with all the wavelenght measured
#' @param tp an integer with the total number of time points
#' @param Columns a list containing "n" vectors of lenght 12 with all the conditions across columns in the plate
#' @param Rows a list containing "n" vectors of lenght 8 with all the conditions across rows in the plate
#'
#' @return "D" a data.table containing all the reads, times, and conditions
#' @author Santiago Caño-Muñiz
#' @export

Plate_reader <- function(file_list = file_list, wl = wl, Rows = Rows, Columns = Columns){
  D <- purrr::map(file_list,function(i,wl,Rows,Columns){
    names <- utils::read.delim(file = paste0(path,i), header=F, skip=2,nrows = 1,stringsAsFactors = F)
    names <- names[-99]
    tmp <- utils::read.delim(file = paste0(path,i), header=F, skip=3)
    tmp <- tmp[-nrow(tmp),]
    tmp <- select(tmp,-V99)
    colnames(tmp) <- names[1,]
    tmp$wavelength <- rep(wl, each = nrow(tmp))       # add wavelength
    tmp <- reshape2::melt(select(tmp,-`Temperature(\xa1C)`), id.vars=c('Time(hh:mm:ss)',"wavelength"),var='Cell')
    #add conditions from rows and columns
    tmp$Experiment <- as.character(i)
    tmp <- cbind(tmp,Columns,Rows)
    for(clmns in 1:length(Columns)){
      cols <- as.numeric(substr(tmp$Cell,2,nchar(as.character(tmp$Cell))))
      tmp[,names(Columns[clmns])] <- Columns[[clmns]][cols]
    }
    for(rws in 1:length(Rows)){
      rws_temp <- substr(as.character(tmp$Cell),1,1)
      tmp[,names(Rows[rws])] <- Rows[[rws]][rws_temp]
    }
    tmp
  }
  ,wl=wl,Rows=Rows,Columns=Columns)
  #merge data.frames
  D <- do.call("rbind", D)
  #Transform time from hh:ss to numeric
  #add time
  D$Time <- ifelse(nchar(as.character(D$`Time(hh:mm:ss)`))==4,
                 paste0("00:0",as.character(D$`Time(hh:mm:ss)`)),
                 ifelse(nchar(as.character(D$`Time(hh:mm:ss)`))<7,
                        paste0("00:",as.character(D$`Time(hh:mm:ss)`)),as.character(D$`Time(hh:mm:ss)`)))
  D$Time <- sapply(strsplit(D$Time,":"),
                 function(x) {
                   x <- as.numeric(x)
                   x[1]+x[2]/60+x[3]/3600
                 }
  )
  D <- data.table(D)
  D <- D[,`Time(hh:mm:ss)`:=NULL]
  return(D)
}

#' @title Data importer from plate reader
#' @description Function to import the reads from the plate reader at the Dr Summers Lab excluding the time variable
#' @param file_list a list with the file paths in format .txt to be imported
#' @param wl vector with all the wavelenght measured
#' @param Columns a list containing "n" vectors of lenght 12 with all the conditions across columns in the plate
#' @param Rows a list containing "n" vectors of lenght 8 with all the conditions across rows in the plate
##'
##' @return "D" a data.table containing all the reads, times, and conditions
##' @author Santiago Caño-Muñiz
##' @export


#Static plate reader importer

Plate_readerStatic <- function(file_list=file_list,wl=wl,tp=tp,Rows=Rows,Columns=Columns){
  D <- purrr::map(file_list,function(i,wl,tp,Rows,Columns){
    # Import data
    names <- read.delim(file = paste0(path,i), header=F, skip=2,nrows = 1,stringsAsFactors = F)
    names <- names[c(-1,-99)]
    tmp <- read.delim(file = paste0(path,i), header=F, skip=3)
    tmp <- tmp[-nrow(tmp),]
    tmp <- select(tmp,-V1,-V99)
    colnames(tmp) <- names[1,]
    tmp$wavelength <- rep(wl, each = nrow(tmp))       # add wavelength
    tmp <- reshape2::melt(select(tmp,-`Temperature(\xa1C)`), id.vars=c("wavelength"),var='Cell')
    #add conditions from rows and columns
    tmp$Experiment <- as.character(i)
    tmp <- cbind(tmp,Columns,Rows)
    for(clmns in 1:length(Columns)){
      cols <- as.numeric(substr(tmp$Cell,2,nchar(as.character(tmp$Cell))))
      tmp[,names(Columns[clmns])] <- Columns[[clmns]][cols]
    }
    for(rws in 1:length(Rows)){
      rws_temp <- substr(as.character(tmp$Cell),1,1)
      tmp[,names(Rows[rws])] <- Rows[[rws]][rws_temp]
    }
    tmp
  }
  ,wl=wl,tp=tp,Rows=Rows,Columns=Columns)
  #merge data.frames
  D <- do.call("rbind", D)
  D <- data.table::data.table(D)
  return(D)
}




#' @title Data importer from plate Fluorescent reader in pathology
#' @description Function to import the reads of a time serie from the plate reader at the Dr Summers Lab.
#' @param file_list a list with the file paths in format .txt to be imported
#' @param Columns a list containing "n" vectors of lenght 12 with all the conditions across columns in the plate
#' @param Rows a list containing "n" vectors of lenght 8 with all the conditions across rows in the plate
#'
#' @return "D" a data.table containing all the reads, times, and conditions
#' @author Santiago Caño-Muñiz
#' @export



Fl_PlateReader <- function(file_list=file_list,Rows=Rows,Columns=Columns){
  D <- purrr::map(file_list,function(i){
    sheets <- readxl::excel_sheets(paste0(path,i))
    short_path <- gsub(paste0(wd,"/"), "", path)
    #import sheets individually
    book_tmp <- purrr::map(sheets, function(n,Rows,Columns){
      tmp <- read_excel(paste0(short_path,i),
                        sheet = n, skip = 1)
      tmp <- data.table(tmp)
      tmp <- reshape2::melt(tmp, id.vars=c("Kinetic read"),var='Cell')
      tmp$sheet <- n
      tmp <- data.frame(tmp)
      #add conditions
      tmp <- cbind(tmp,Columns,Rows)
      for(clmns in 1:length(Columns)){
        cols <- as.numeric(substr(tmp$Cell,2,nchar(as.character(tmp$Cell))))
        tmp[,names(Columns[clmns])] <- Columns[[clmns]][cols]
      }
      for(rws in 1:length(Rows)){
        rws_temp <- substr(as.character(tmp$Cell),1,1)
        tmp[,names(Rows[rws])] <- Rows[[rws]][rws_temp]
      }
      tmp
      return(tmp)
    },Rows=Rows,Columns=Columns)
    book_tmp <- do.call("rbind", book_tmp)
    book_tmp$Experiment <- as.character(i)
    return(book_tmp)
  })
  #merge data.frames
  D <- do.call("rbind", D)
  D <- data.table::data.table(D)
  #add time
  D[,
    Time :=.(c(as.numeric(Kinetic.read)-as.numeric(Kinetic.read[1]))/3600), #conver date into time
    by=Cell]
  return(D)
}


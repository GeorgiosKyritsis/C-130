library(xlsx)
library(gWidgets)
options(guiToolkit = 'RGtk2')

#Only for Linux the following library
library(tcltk2)

#Only for Linux
setwd("/home/inamoto21/Desktop")

#Only for Windows
#setwd("C:/Documents and Settings/GEP2/Επιφάνεια εργασίας/ΚΟΙΝΑ ΑΡΧΕΙΑ")

df <- data.frame(row.names = NULL,
                 AIRCRAFT = as.character(0),
                 ITEM = as.character(0),
                 HOURS = as.numeric(0),
                 DATE = as.character(0),
                 RECURRING = as.character(0),
                 NOTES = as.character(0),
                 CLASSIFICATION = as.character(0)
                 ,stringsAsFactors = FALSE)

aircraft1 <- c('741','742','743','744','745')
aircraft2 <- c('746','747','749','751','752')
aircraft3 <- c('296','300','303','723','948')

classification <- c('SMP 515-C-100', 'EDIPES', 'TDEAY', 'DAY', 'SB/TCTO',
                    'TECHNICAL MANUAL', 'TO 1C-130A-36', 'LOL/LOZ')

updateTable <- function(h,...) {
  #Parse the Aircraft/s from checkbox
  aircraft1 = svalue(checkbox.check1)
  #Parse the Aircraft/s from checkbox
  aircraft2 = svalue(checkbox.check2)
  #Parse the Aircraft/s from checkbox
  aircraft3 = svalue(checkbox.check3)
  aircraft = c(aircraft1, aircraft2, aircraft3)
  #Parse the Classification from checkbox
  classif = svalue(checkbox.classification)
  #Parse the Lower Hour
  lower_d = svalue(lower)
  #Parse the Upper Hour
  upper_d = svalue(upper)
  #Parse the Lower Date
  from_d = as.character(svalue(from_date))
  #Parse the Upper Date
  to_d = as.character(svalue(to_date))
  
  #Check if file read
  if(nrow(df) == 1)
    gmessage('ΞΕΧΑΣΕΣ ΝΑ ΔΙΑΒΑΣΕΙΣ ΤΟ ΑΡΧΕΙΟ', title = 'ERROR - FILE READ', icon = 'error')
  
  if(is.na(from_d))
    from_d = as.character(Sys.Date())
  if(is.na(to_d))
    to_d = as.character(Sys.Date())
  
  if((lower_d >= upper_d) && (from_d >= to_d)) {
    gmessage('ΛΟΓΙΚΟ ΛΑΘΟΣ ΣΕ ΗΜΕΡΟΜΗΝΙΕΣ ΚΑΙ ΩΡΕΣ',
             title = 'ERROR - HOURS && DATES SELECTION', icon = 'error')
  } else if (lower_d >= upper_d){
    gmessage('ΛΟΓΙΚΟ ΛΑΘΟΣ ΣΕ ΩΡΕΣ', title = 'ERROR - HOURS SELECTION', icon = 'error')
  } else if (from_d >= to_d) {
    gmessage('ΛΟΓΙΚΟ ΛΑΘΟΣ ΣΕ ΗΜΕΡΟΜΗΝΙΕΣ', title = 'ERROR - DATE SELECTION', icon = 'error')
  } else if (identical(svalue(checkbox.check1), character(0)) & identical(svalue(checkbox.check2), character(0)) & identical(svalue(checkbox.check3), character(0))) {
    gmessage('ΔΙΑΛΕΞΕ ΤΟΥΛΑΧΙΣΤΟΝ ΕΝΑ ΑΕΡΟΠΛΑΝΟ', title = 'ERROR - AIRCRAFT SELECTION', icon = 'error')
  } else if (identical(svalue(checkbox.classification), character(0))) {
    gmessage('ΔΙΑΛΕΞΕ ΤΟΥΛΑΧΙΣΤΟΝ ΜΙΑ ΚΑΤΗΓΟΡΙΑ', title = 'ERROR - CATEGORIES SELECTION', icon = 'error')
  } else {
    temp = subset(df, ((CLASSIFICATION %in% classif) & (AIRCRAFT %in% aircraft) & (HOURS >= lower_d & HOURS <= upper_d)) | ((CLASSIFICATION %in% classif) &(AIRCRAFT %in% aircraft) & (DATE>=from_d & DATE<=to_d )))
    temp$DATE = as.character(format(as.Date(temp$DATE), "%d/%m/%Y"))
    #temp[is.na(temp)] = ""
    temp$HOURS = round(as.numeric(temp$HOURS), digits = 1)
    temp[is.na(temp)] = ""
    t1[] <- temp
  } 
}

saveFile <- function(h,...) {
  #Parse the Aircraft/s from checkbox
  aircraft1 = svalue(checkbox.check1)
  #Parse the Aircraft/s from checkbox
  aircraft2 = svalue(checkbox.check2)
  #Parse the Aircraft/s from checkbox
  aircraft3 = svalue(checkbox.check3)
  aircraft = c(aircraft1, aircraft2, aircraft3)
  #Parse the Classification from checkbox
  classif = svalue(checkbox.classification)
  #Parse the Lower Hour
  lower_d = svalue(lower)
  #Parse the Upper Hour
  upper_d = svalue(upper)
  #Parse the Lower Date
  from_d = as.Date(svalue(from_date))
  #Parse the Upper Date
  to_d = as.Date(svalue(to_date))
  
  if(is.na(from_d))
    from_d = as.Date(Sys.Date(), format = "%Y-%m-%d")
  if(is.na(to_d))
    to_d = as.Date(Sys.Date(), format = "%Y-%m-%d")
  
  if((lower_d >= upper_d) && (from_d >= to_d)) {
    gmessage('ΛΟΓΙΚΟ ΛΑΘΟΣ ΣΕ ΗΜΕΡΟΜΗΝΙΕΣ ΚΑΙ ΩΡΕΣ',
             title = 'ERROR - HOURS && DATES SELECTION', icon = 'error')
  } else if (lower_d >= upper_d){
    gmessage('ΛΟΓΙΚΟ ΛΑΘΟΣ ΣΕ ΩΡΕΣ', title = 'ERROR - HOURS SELECTION', icon = 'error')
  } else if (from_d >= to_d) {
    gmessage('ΛΟΓΙΚΟ ΛΑΘΟΣ ΣΕ ΗΜΕΡΟΜΗΝΙΕΣ', title = 'ERROR - DATE SELECTION', icon = 'error')
  } else if (identical(svalue(checkbox.check1), character(0)) & identical(svalue(checkbox.check2), character(0)) & identical(svalue(checkbox.check3), character(0))) {
    gmessage('ΔΙΑΛΕΞΕ ΤΟΥΛΑΧΙΣΤΟΝ ΕΝΑ ΑΕΡΟΠΛΑΝΟ', title = 'ERROR - AIRCRAFTS', icon = 'error')
  } else if (identical(svalue(checkbox.classification), character(0))) {
    gmessage('ΔΙΑΛΕΞΕ ΤΟΥΛΑΧΙΣΤΟΝ ΜΙΑ ΚΑΤΗΓΟΡΙΑ', title = 'ERROR - CATEGORIES', icon = 'error')
  } else {
    temp = subset(df, ((CLASSIFICATION %in% classif) & (AIRCRAFT %in% aircraft) & (HOURS >= lower_d & HOURS <= upper_d)) | ((CLASSIFICATION %in% classif) &(AIRCRAFT %in% aircraft) & (DATE>=from_d & DATE<=to_d )))
    temp$DATE = as.character(format(as.Date(temp$DATE), "%d/%m/%Y"))
    temp = subset(temp, select = ITEM)
    acft = paste(aircraft, collapse = ',')
    categ = paste(classif, collapse = ',')
    write.xlsx(temp, file = paste0('Aircrafts ',acft, " ", Sys.Date(),'.xls'), row.names = FALSE, showNA = FALSE)
   
    gmessage(paste0('Aircrafts ',acft, " ", Sys.Date(),'.xls created and saved'), title = 'FILE CREATION', icon = 'info')
  }
}
readFile <- function(h,...) {
#   PROGRESS BAR FOR WINDOWS  
#   pb = winProgressBar(title = 'Reading file Aircraft Inspection Program.xls', label = '0% done', min = 0, max = 100, initial = 0)
#   for(i in 1:25) {
#     Sys.sleep(0.01)
#     info = sprintf('%d%% done', round((i/100)*100))
#     setWinProgressBar(pb, i/(100)*100, label = info)
#   }
   
  pb <- tkProgressBar(title = "Reading file Aircraft Inspection Program.xls", label = '0% done', min = 0, max = 100, initial = 0)
  for(i in 1:25) {
      Sys.sleep(0.01)
      info = sprintf('%d%% done', round((i/100)*100))
      setTkProgressBar(pb, i/(100)*100, label= info)
  }
  
  temp <- read.xlsx('Aircraft Inspection Program.xls', sheetIndex = 1,
                  header = TRUE, 
                  colClasses = c('character', 'character', 'numeric', 'POSIXct', 'character', 'character', 'character'), startRow = 1, endRow = 3000,
                  stringsAsFactors = FALSE) # read the first sheet
  temp$DATE = as.POSIXct((as.numeric(temp$DATE) - 25569)*86400, tz = 'GMT', origin = '1970-01-01')
  temp$DATE = as.character(temp$DATE)
  
  assign("df", temp, envir = .GlobalEnv)
  
#   PROGRESS BAR FOR WINDOWS
#   for(i in 25:100) {
#     Sys.sleep(0.01)
#     info = sprintf('%d%% done', round((i/100)*100))
#     setWinProgressBar(pb, i/(100)*100, label = info)
#   }
#   close(pb)

for(i in 25:100) {
   Sys.sleep(0.01)
   info = sprintf('%d%% done', round((i/100)*100))
   setTkProgressBar(pb, i/(100)*100, label = info)
}
  close(pb)
  gmessage('Το Αρχείο Aircraft Inspection Program.xls διαβάστηκε από την εφαρμογή', title = 'FILE READ', icon = 'info')
}

file_select = glabel('Πριν πατήσεις το κουμπί <<Read File>> ενημέρωσε το αρχείο Aircraft Inspection Program.xls\nΔεν είναι υποχρεωτικό να κλείσεις την εφαρμογή .')
file_update = gbutton("Read File", handler = readFile)
font(file_update) <- c(color="red", style="bold")
lower_label = glabel('From:')
lower <- gspinbutton(from=-100, to = 2000, by =1, value=-100)
upper_label = glabel('To:')
upper <- gspinbutton(from=-100, to = 2000, by =1, value=150)
from_date = gcalendar(text = "From:", format = "%Y-%m-%d") 
to_date = gcalendar(text = "To:", format = "%Y-%m-%d") 
checkbox.check1 = gcheckboxgroup(aircraft1, horizontal = TRUE)
checkbox.check2 = gcheckboxgroup(aircraft2, horizontal = TRUE)
checkbox.check3 = gcheckboxgroup(aircraft3, horizontal = TRUE)
checkbox.classification = gcheckboxgroup(classification, horizontal = FALSE, checked = TRUE)
button_go = gbutton('Update Table', handler = updateTable)
font(button_go) <- c(color="red", style="bold")
save = gbutton("Save to file", handler = saveFile)

##layout
window <- gwindow("Aircraft Inspection Program")
BigGroup <- ggroup(cont=window, horizontal = TRUE)
group <- ggroup(horizontal=FALSE, container=BigGroup)
tmp <- gframe("Readind the File Aircraft Inspection Program.xls", container=group, horizontal = FALSE)
add(tmp, file_select)
add(tmp, file_update)
tmp <- gframe("Hours Selection", container=group, horizontal = FALSE)
add(tmp,lower_label)
add(tmp,lower)
add(tmp,upper_label)
add(tmp,upper)
tmp <- gframe("Date Selection", container=group, horizontal = FALSE)
add(tmp,from_date)
add(tmp,to_date)
tmp <- gframe("C-130H", container=group)
add(tmp,checkbox.check1,expand=TRUE, fill="x")
tmp <- gframe("C-130H", container=group)
add(tmp,checkbox.check2,expand=TRUE, fill="x")
tmp <- gframe("C-130B", container=group)
add(tmp,checkbox.check3,expand=TRUE, fill="x")
tmp <- gframe("Categories", container=group)
add(tmp,checkbox.classification)
tmp <- gframe("Update Table", container=group, horizontal = FALSE)
add(tmp,button_go)
tmp <- gframe("Save the table to a file", container=group, horizontal = FALSE)
add(tmp,save)
t1 <- gtable(df, container = BigGroup, expand = TRUE)
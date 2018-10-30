# JS form script Builder ---------------------------------------------------
title_= function(a){
  paste(a, collapse = "_")
} 

question_namer = function(a, i) {
  x=paste(
    title_(a),
    'question',
    i,
    sep="_"
  )
  return(x)
}

choice_formatter = function(choices) {
  choices = paste("[",
                  paste(paste0("'",
                               choices,
                               "'"),
                        collapse = ", "),
                  "]",
                  sep = "")
  return(choices)
}

beginning_stuff = function(x){
  form_var = paste(
    "var form = FormApp.create('", x$meta$title, "');", sep = "")
  spreadsheet_var = paste(
    "var ss = SpreadsheetApp.create('", x$meta$title, " responses');", sep = "")
  return(paste(form_var, spreadsheet_var, "form.setDestination(FormApp.DestinationType.SPREADSHEET, ss.getId());", sep = "\n"))
}


inputs_stuff = function(x) {
  latlong = paste(
    "form.addTextItem()
      .setTitle('latlong');"
  )
  inputs = lapply(1:length(x$questions), function(i) {
    if (x$questions[[i]]$InputType == 'input') {
      paste(
        "form.addTextItem()
        .setTitle('", question_namer(strsplit(x$meta$title, " ")[[1]], i),"');", sep = "")
    } else if (x$questions[[i]]$InputType == 'select') {
      paste(
        "form.addListItem()
        .setTitle('", question_namer(strsplit(x$meta$title, " ")[[1]], i),"')
        .setChoiceValues(", choice_formatter(1:length(x$questions[[i]]$Choices)),
        ");",
        sep = ""
        )
    }
  })
  
  return(paste(latlong,paste(do.call("c",inputs), collapse = "\n"), sep = "\n"))
}


log_stuff = function() {
  paste(
    "Logger.log('Published URL: ' + form.getPublishedUrl());
    Logger.log('Editor URL: ' + form.getEditUrl());"
  )
}


saveToFolder_function = function(){
  "function saveItemInFolder(item,folder) {
  var id = item.getId();  // Will throw error if getId() not supported.
  folder.addFile(DriveApp.getFileById(id));
}"
}

script_builder = function(x) {
  beg = beginning_stuff(x)
  input = inputs_stuff(x)
  log = log_stuff()
  function_name = paste(title_(strsplit(x$meta$title," ")[[1]]), "function()", sep = "_")
  
  script = paste(
    # saveToFolder_function(),
    # "\n",
    "function ", function_name, " { \n",
    beg,
    "\n",
    input,
    "\n",
    log,
    "\n",
    "var folder = DriveApp.getFoldersByName('",paste(strsplit(x$meta$title," " )[[1]],collapse ="_"),"').next();",
    "\n",
    "saveItemInFolder(form, folder)",
    "\n",
    "saveItemInFolder(ss, folder)",
    "\n",
    "}",
    sep = ""
  )
  # files=list(list(
  #   name = "Code",
  #   type = "server_js",
  #   source = script
  # ), 
  # list(name = "index",
  #      type = "html",
  #      source = ""))
  return(script)
}



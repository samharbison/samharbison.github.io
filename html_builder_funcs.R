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

a=paste(strsplit(x$meta$title, " ")[[1]])
  
  
  

x = jsonlite::read_json("~/samharbison.github.io/something_here.json")
redirect_url = 'https://media.giphy.com/media/9uoYC7cjcU6w8/giphy.gif'
google_form_url = "https://docs.google.com/forms/d/e/1FAIpQLSc2WW01-yh5cOx2tzGEisIShU42-aIyzBYhwbPalY0pAdGvbg/viewform"




# Had builder function ----------------------------------------------------

head_html = function(x,
                     locate = FALSE,
                     font_api = "https://fonts.googleapis.com/css?family=Poppins:800") {
  if (locate == FALSE) {
    head = paste(
      '<head>
      <meta charset="UTF-8">
      <title>',
      paste('MFS R2R:', x$meta$title, sep = " "),
      '</title>
      <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/normalize/5.0.0/normalize.min.css">
      <link rel="stylesheet" href="',
      paste(paste(
        'style', paste(strsplit(x$meta$title, " ")[[1]], collapse = "_"), sep = "_"
      ), "css", sep = '.'),
      '">
      <link href="',
      font_api,
      '" rel="stylesheet">
      <!-- Latest compiled and minified CSS -->
      <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
      <!-- jQuery library -->
      <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js"></script>
      <!-- Latest compiled JavaScript -->
      <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
      </head>',
      sep = ""
    )
  } else if (locate == TRUE) {
    head = paste(
      '<head>
      <meta charset="UTF-8">
      <title>',
      paste('MFS R2R:', x$meta$title, sep = " "),
      '</title>
      <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/normalize/5.0.0/normalize.min.css">
      <link rel="stylesheet" href="',
      paste(paste('style', title_(
        strsplit(x$meta$title, " ")[[1]]
      ), sep = "_"), "css", sep = '.'),
      '">
      <link href="',
      font_api,
      '" rel="stylesheet">      <!-- Latest compiled and minified CSS -->
      <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
      <!-- jQuery library -->
      <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js"></script>
      <!-- Latest compiled JavaScript -->
      <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
      </head>'
      ,
      sep = ""
    )
  }
  return(head)
}

  
  
  
  

# Title, Description, and Redirect div funtions ---------------------------

header_html = function(x) {
  header = paste(
    '<header>
    <div class="wrapper">
    <h1>',
    x$meta$title,
    '</h1>
    </div>
    </header>',
    collapse = "\n"
  )
  return(header)
}
  
survey_description_html = function(x) {
  p = paste(
    '<p class="form-group">',
    x$meta$description,
    '</p>',
    sep = "",
    collapse = "\n"
  )
  return(p)
}

survey_redirect_html = function(redirect_url) {
  redirect = paste(
  '<script type="text/javascript">
    var submitted=false;
  </script>
  <iframe name="hidden_iframe" id="hidden_iframe" style="display:none;" onload="if(submitted) {window.location=',
  paste("'",redirect_url,"'", sep = ""),
  ';}"></iframe>',
  sep = "")
  return(redirect)
}
  


# how to make inputs ------------------------------------------------------
  
input_html = function(x, event_ids) {
    event_ids= event_ids[-1]
    inputs = lapply(1:length(x$questions), function(i) {
      if (x$questions[[i]]$InputType == 'input') {
        paste(
          '<div class="form-group">
          <label for="',
          question_namer(strsplit(x$meta$title, " ")[[1]], i),
          '">',
          x$questions[[i]]$InputLabel,
          '</label>
          <input type="text" class="form-control" name="',event_ids[i],'" id=',
          #paste(paste(strsplit(x$meta$title, " ")[[1]], collapse = "_"), 'question', i, sep = "_"),
          question_namer(strsplit(x$meta$title, " ")[[1]], i),
          '>
          </div>
          <br>',
          sep = ""
        )
      } else if (x$questions[[i]]$InputType == 'select') {
        options = lapply(1:length(x$questions[[i]]$Choices), function(j) {
          paste('<option value="',
                j,
                '">',
                x$questions[[i]]$Choices[[j]][1],
                '</option>',
                sep = "")
        })
        
        paste(
          '<div class="form-group">
          <label for="',
          question_namer(strsplit(x$meta$title, " ")[[1]], i),
          '">',
          x$questions[[i]]$InputLabel,
          '</label>
          <br>
          <select class="form-control" name="',event_ids[i],'" id="',
          question_namer(strsplit(x$meta$title, " ")[[1]], i),
          '">
          <option style="display:none;"></option>\n',
          paste(do.call("c", options), collapse = "\n"),
          '\n</select>
          </div>
          <br>',
          sep = ""
        )
      }
    })
    return(inputs)
  }
  

# building whole form  ----------------------------------------------------

library(rvest)




form_html = function(x, google_form_url, locate = TRUE)  {
  require(rvest)
  url = read_html(google_form_url)
  fn = names(html_form(url)[[1]][["fields"]])
  event_ids =  unique(fn[grepl("entry.", fn)])
  formResponse_url = html_form(url)[[1]][["url"]]
  
  form_tag_1 = paste('<form action="',
                   formResponse_url,
                   '" method="POST" id="ss-form" target="hidden_iframe" onsubmit="submitted=true;">',
                   sep = "")
  form_tag_2 = '</form>'
  hidden_location_input = paste('<input type="hidden" name="',event_ids[1],'" id="latlong">', sep = "")
  inputs =input_html(x, event_ids)
  button = '<button class="btn btn-secondary btn-lg btn-block" type="submit" value="Submit">Submit your Survey</button>'
  divs = paste(
    paste(
      do.call("c", inputs),
      collapse = "\n"),
    button,
    sep = "\n")
  
  if (locate == TRUE) {
    out = paste(form_tag_1, hidden_location_input, divs, form_tag_2, sep = "\n")
  } else if (locate == FALSE) {
    out = paste(form_tag_1, divs, form_tag_2, sep = "\n")
  }
  return(out)
}


# section builder ---------------------------------------------------------

section_html = function(x, google_form_url, redirect_url, locate = TRUE) {
  section = paste(
    '<section>',
    survey_description_html(x),
    survey_redirect_html(redirect_url),
    form_html(x, google_form_url, locate),
    '<script>
      
    var x = document.getElementById("latlong");
    
    function getLocation() {
    if (navigator.geolocation) {
    navigator.geolocation.getCurrentPosition(showPosition);
    } else {
    x.value = "Geolocation is not supported by this browser.";
    }
    }
    
    function showPosition(position) {
    x.value = position.coords.latitude.toFixed(3) + "," + position.coords.longitude.toFixed(3);
    }
    </script>',
    '</section>',
    sep = "\n"
  )
}


# Html file builder -------------------------------------------------------

html_html = function(x, google_form_url, redirect_url, locate = TRUE, font_api = "https://fonts.googleapis.com/css?family=Poppins:800") {
  if (locate == TRUE) {
    file = paste(
      '<!DOCTYPE html>
      <html>',
      head_html(x, locate, font_api),
      '<body onload="getLocation()">',
      header_html(x),
      section_html(x, google_form_url, redirect_url, locate),
      '</body>
      </html>',
      sep = "\n"
    )
  } else {
  file = paste(
    '<!DOCTYPE html>
    <html lang="en" >',
    head_html(x, locate, font_api),
    '<body>',
    header_html(x),
    section_html(x, google_form_url, redirect_url, locate),
    '</body>
    </html>',
    sep = "\n"
  )
  }
  return(file)
}



cat(html_html(x, google_form_url, redirect_url, TRUE), file = "~/samharbison.github.io/map/tester.html")

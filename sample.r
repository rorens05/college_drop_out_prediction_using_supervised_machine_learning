#install.packages("base64enc")  
library(shiny)
library(caret)
library(e1071)
library(base64enc)
  
  # load the dataset
  Filename <- "~/Desktop/ulstudents.csv"
  dataset<-read.csv(Filename, header=TRUE)
  starting <- 0
  b64 <- base64enc::dataURI(file="sample.jpg", mime="image/png")
  # create training dataset, I didnt follow it though, I just 
  # used the 100% in the dataset instead of 90%
  # since were not going to test it anyway.
  # this 3 lines of code can be remove
  train_index <- createDataPartition(y=dataset$Action, p=0.90, list=FALSE)
  training <- dataset[train_index]
  testing <- dataset[-train_index]
  
  # train the model, NBclassfier will be use later to predict the result
  NBclassfier <- naiveBayes(Action ~., dataset)
  
  # print to see more details about the model, check the console
  print(NBclassfier)
  summary(NBclassfier)
  
  # create UI
  ui <- fluidPage(
    
    # custom styles
    tags$head(
      tags$style(HTML("
        @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
        
        h1 {
          font-family: 'Lobster', cursive;
          font-weight: 500;
          line-height: 1.1;
          color: #48ca3b;
        }
        
        h4 {
          width: 100%;
          border: 1px solid #cecece;
          display: inline-block;
          height: 40px;
          padding: 10px;
          text-align: center;
          border-radius: 5px;
          font-weight: bold;
          color: red
        }
        img{
          width: 50%;
          display: block;
          margin: 0 auto;
        }
        .gradeInput, .UnitInput{
          margin-left: 4px;
          margin-right: 4px;
          width: 50px;
          
          height: 34px;
          padding: 6px 2px;
          font-size: 14px;
          color: #555;
          background-color: #fff;
          border: 1px solid #ccc;
          border-radius: 4px;
          margin-bottom: 5px;
        }
        .UnitInputDrop{
          width: 100px;
          margin-left: 10px
          
          height: 34px;
          padding: 6px 10px;
          font-size: 14px;
          color: #555;
          background-color: #fff;
          border: 1px solid #ccc;
          border-radius: 4px;
          margin-bottom: 5px;
        }
        
        #agbtn, #adbtn, #calculate, #reset{
          margin-top: 5px;
          margin-bottom: 10px;
          width: 100%;
          border-radius: 5px;
          padding: 5px;
          background-color: #337ab7;
          color: white;
          border: 1px solid #2e6da4;
        }
        p{
          text-align: center;
        }
        #run{
         background-color: #0dc400;
         color: white
        }
  
      "))
    ),
    
    titlePanel( 
      h2("College Dropout Prediction", align = "center")
    ),
    titlePanel( 
      h3("Using Supervised Machine Learning", align = "center")
    ),
    # main form
    fluidRow(
      column(3,
             h3("Grade Calculator"),
             h5("Please input grades of the student"),
             tags$div(
               HTML(
                 "
                    <div id='studentGradeContainer'>
                      
                    </div>
                    <button id='agbtn'>Add Subject</button>
                    <!-- <h5>Please input drop subject units of the student</h5> -->
                    <div id='studentDroppedContainer'>
                    
                    </div>
                    <button id='adbtn'>Add Dropped Subject</button><br/>
                    <button id='calculate'>Calculate</button> <hr>
                    <button id='reset'>Reset</button>
                 "
               )
             )
             
      ),
      column(4, offset = 1,br(),
             
             numericInput(inputId = "gw_average",
                          label = "General Weighted average",
                          value = 0, width = "100%"),
             
             numericInput(inputId = "percent_drop",
                          label = "PercentDrop",
                          value = 0, width = "100%"),
             
             selectInput(inputId = "location",
                         label = "Choose Location:",
                         choices = c(
                           "Dagupan City", "Mangaldan", "Calasiao", "Binmaley",
                           "Bugallon", "Lingayen", "San Carlos City", "Sta. Barbara", "San Jacinto", "Labrador", "Mapandan", "Malasiqui", "Manaoag", "San Fabian",
                           "Others"
                           ), width = "100%"),
             
             selectInput(inputId = "gender",
                         label = "Choose Gender:",
                         choices = c("Male", "Female"), width = "100%"),
             
             actionButton(inputId="run", label="Predict",text = "Predict", icon = NULL, width = "100%"),br(),
             tags$div(
               HTML("<label class='control-label' >Output:</label>")
             ),
             h4(tableOutput("view")),
             tags$div(
               HTML(
                "<script>
                    var gradeLimit = 10;
                    var gradeCounter = 0;
                    var droppedCounter = 0;
                    var gpercentDropped = 0;
                    var ggwa = 0;
                    
                    document.getElementById('view').style.opacity = '0';
                
                    document.getElementById('gw_average').value = '0';

                    
                    document.getElementsByTagName('BUTTON')[document.getElementsByTagName('BUTTON').length - 1].onclick = function(){
                      document.getElementById('view').style.opacity = '1';
                    }
                    
                    document.getElementById('agbtn').onclick = function(){
                      if(gradeLimit > gradeCounter){
                        gradeCounter = gradeCounter + 1;
                        var inputContainer = document.createElement('DIV');
                        inputContainer.setAttribute('class', 'inputContainer');
                        
                        inputContainer.innerHTML = '<label>Grade: </label><input type=\"number\" class=\"gradeInput\" value=0><label>Unit: </label><input type=\"number\" class=\"UnitInput\" value=0>' 
                        document.getElementById('studentGradeContainer').appendChild(inputContainer);
                      }
                    }
                    
                    document.getElementById('adbtn').onclick = function(){
                      if(gradeLimit > droppedCounter){
                        droppedCounter = droppedCounter + 1;
                        var inputContainer = document.createElement('DIV');
                        inputContainer.setAttribute('class', 'dropInputContainer');

                        inputContainer.innerHTML = '<label>Unit: </label><input type=\"number\" class=\"UnitInputDrop\">' 
                        document.getElementById('studentDroppedContainer').appendChild(inputContainer);
                      }
                    }
                    
                    document.getElementById('reset').onclick = function() {
                      
                      document.getElementById('studentGradeContainer').innerHTML = '';
                      document.getElementById('studentDroppedContainer').innerHTML = '';
                      document.getElementById('view').style.opacity = '0';
                      document.getElementById('gw_average').value = '0';
                      document.getElementById('percent_drop').value = '0';
                    }
                    
                </script>"
               )
             ),
             tags$script('
                document.getElementById("gw_average").onkeydown = function(){
                  document.getElementById("view").style.opacity = "0";
                  Shiny.onInputChange("calcpd", null);
                  Shiny.onInputChange("calcgwa", null);
                }
                
                document.getElementById("percent_drop").onkeydown = function(){
                  document.getElementById("view").style.opacity = "0";
                  Shiny.onInputChange("calcpd", null);
                  Shiny.onInputChange("calcgwa", null);
                }
             
                document.getElementById("calculate").onclick = function() {
                  var containers = document.getElementsByClassName("inputContainer");
                  var totalGrade = 0;
                  var totalUnits = 0;
                  for(var i = 0; i != containers.length; i++){
                    var grade = 0;
                    var unit = 0;
                    for(var j = 0; j != containers[i].childNodes.length; j++){
                      if(containers[i].childNodes[j].className == "gradeInput"){
                        grade = parseInt(containers[i].childNodes[j].value)
                      }
                      if(containers[i].childNodes[j].className == "UnitInput"){
                        unit = parseInt(containers[i].childNodes[j].value)
                      }
                    }
                    totalGrade += grade * unit
                    totalUnits += unit;
                  }
                  
                  var gwa = (totalGrade * 1.0 )/ (totalUnits * 1.0)
                  document.getElementById("gw_average").value = gwa;
                  ggwa = gwa;
                  
                  if(document.getElementById("gw_average").value == ""){
                    document.getElementById("gw_average").value = 0;
                    gwa = 0;
                  }
                  
                  var dropContainers = document.getElementsByClassName("dropInputContainer");
                  var totalDroppedUnits = 0;

                  for(var i = 0; i != dropContainers.length; i++){
                    var unit = 0;
                    for(var j = 0; j != dropContainers[i].childNodes.length; j++){
                      if(dropContainers[i].childNodes[j].className == "UnitInputDrop"){
                        unit = parseInt(dropContainers[i].childNodes[j].value)
                        totalDroppedUnits += unit;
                      }
                    }
                  }
                  var percentdrop = (totalDroppedUnits * 1.0) / ((totalDroppedUnits + totalUnits) * 1.0)
                  document.getElementById("percent_drop").value = percentdrop * 100;
                  
                  
                  if(document.getElementById("percent_drop").value == ""){
                    document.getElementById("percent_drop").value = 0;
                    percentdrop = 0;
                  }
                  
                  Shiny.onInputChange("calcpd", percentdrop * 100);
                  Shiny.onInputChange("calcgwa", gwa);
                };
                
                
                document.getElementById("reset").onclick = function() {
                  Shiny.onInputChange("calcpd", null);
                  Shiny.onInputChange("calcgwa", null);
                  
                  
                  document.getElementById("studentGradeContainer").innerHTML = "";
                  document.getElementById("studentDroppedContainer").innerHTML = "";
                  document.getElementById("view").style.opacity = "0";
                  document.getElementById("gw_average").value = "0";
                  document.getElementById("percent_drop").value = "0";
                  
                  gradeCounter = 0;
                  droppedCounter = 0;
                }
                
                
              ')
      ),
      column(4,
         img(src=b64),
         p("College Dropout \n Prediction Version 2")
      )
    )
  )
  
  server <- function(input, output) {
    
    
    observeEvent(input$run, {
      
      output$view <- renderText({
        
        print("printing calcpd")
        print(input$calcpd)
        print("printing calcgwa")
        print(input$calcgwa)
        
        # if the value of the inputs are default, dont print output
        # model variables
        
        gwa <- ""
        p_drop <- ""
        loc <- ""
        gender <- input$gender
        
        # logic here 
        # convert user input to categorized data
        # gw_average
        
        average <- input$calcgwa
        if(is.null(average)){
          average <- input$gw_average
        }
        
        if(average < 75){
          gwa <- "Fail"
        } else if(average < 80){
          gwa <- "Low"
        } else if(average < 85){
          gwa <- "Medium"
        } else if(average < 90){
          gwa <- "High"
        } else {
          gwa <- "Very High"
        }
        
        pd <- input$calcpd
        if(is.null(pd)){
          pd <- input$percent_drop
        }
        print(pd)
        # percent drop
        if(pd <= 15){
          p_drop <- "Very Low"
        } else if(pd <= 30){
          p_drop <- "Low"
        } else if(pd <= 60){
          p_drop <- "High"
        } else {
          p_drop <- "Very High"
        }
        
        #location "Dagupan City", "Mangaldan", "Calasiao", "Binmaley"
        i_location <- input$location
        if("Others" == i_location){
          loc <- "Very Far"
        }else if(
          "Dagupan City" == i_location ||
          "Mangaldan" == i_location ||
          "Calasiao" == i_location ||
          "Binmaley" == i_location
        ){
          loc <- "Near"
        }else{
          loc <- "Far"
        }
        
        # create dataframe from input of user 
        Location <- c(loc)
        X.Drop <- c(p_drop)
        GWA <- c(gwa)
        Gender <- c(gender)
        Action <- c('NA')
        data_to_predict <- data.frame(Location, X.Drop, GWA, Gender, Action)
        # check the console to verify the values
        print(data_to_predict)
        
        #predicting from the dataframe created from user input
        testPred=predict(NBclassfier, newdata=data_to_predict, type="class")
        print(testPred)
        
        # print output here
        # paste(toString(input$gw_average), toString(input$percent_drop),toString(input$location),toString(input$gender))
        # get the main value of the predicted output and print it in the form
        
        # created a javascript solution so this line of code is not needed anymore
        # if(starting == 0){
        #  starting <<- 1
        #  print("not started yet")
        #  ""
        #}else{
        #  toString(testPred)
        #}
        toString(testPred)
        
      })
    })
  }
  
  shinyApp(ui = ui, server = server)
  

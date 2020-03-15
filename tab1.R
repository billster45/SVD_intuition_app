# add chapter on PCS https://bookdown.org/rdpeng/exdata/dimension-reduction.html
TAB1_SERVER <- function(input, output, session) {
  


  output$TAB1_CONTROLS <- shiny::renderUI({
    
    controls <- base::list(
      


      
      
    )
    
    return(controls)
  })
  
  
  
  output$TAB1_BODY <- renderUI({
    
#https://shiny.rstudio.com/articles/tag-glossary.html
# svd from scratch https://rpubs.com/aaronsc32/singular-value-decomposition-r and this https://web.mit.edu/be.400/www/SVD/Singular_Value_Decomposition.htm
    #http://www.ams.org/publicoutreach/feature-column/fcarc-svd
    #http://www.ams.org/publicoutreach/feature-column/fcarc-svd
    #https://stats.idre.ucla.edu/r/codefragments/svd_demos/
    #https://www.youtube.com/watch?v=4DI68P4hicQ
    ui <- shiny::fluidPage(

      doc <- tags$html(
        tags$body(
          
          shiny::br(),
          shiny::img(src="https://live.staticflickr.com/4313/36003144451_eb1dd9db12_c.jpg", 
                     height="40%", width="40%", align="right")  ,
          
          tags$h2("Why is learning SVD useful?"), 
          
          tags$ol(
            
          tags$li(p("Singular Value Decomposition (SVD) is a powerful data analysis technique with many uses including data dimension reduction 
          prior to Machine Learning through",
          shiny::a("Principal Components Analysis",
          href="https://towardsdatascience.com/dimensionality-reduction-does-pca-really-improve-classification-outcome-6e9ba21f0a32",
          target="_blank"),
          "solving linear equations, as well as image compression and improving our ability to search text through ",
          shiny::a("Latent Semantic Analysis (LSA)",
          href="https://github.com/billster45/NLP-Intuition#beyond-tf-idf---latent-semantic-analysis-lsa",
          target="_blank"), " both of which you can experiment with using this app.")
          ),
          
          tags$li(p("By playing around with image compression and LSA with text in this app you might build a greater 
                     and more intuitive understanding of SVD. And perhaps a readiness to use it more confidently and appropriately in your 
                     data analysis.")
                  )
          ),
          
          tags$h2("What does SVD do to a matrix of numbers?"), 
          
          tags$ol(
            
            tags$li("SVD is a matrix factorisation technique where the image matrix (or the counts of words
              in each document) are de-composed into three matricies, U, D and V. When multiplied together, the three matricies will exactly re-create the original
              matrix."),
            
            tags$li("The SVD magic happens when we truncate the three matricies before multipying them back together. This
              creates a new matrix with less information. For an image this will compress the information leading to an image that is fuzzier. While 
              for text, SVD wiil give weight to words that are not in a document but are in documents similar to them (i.e. they shaare other words), 
                    so improving text search.")
          ),
          
          tags$h2("How is SVD calcualted in R practically and intuitively?"), 
          
          tags$ol(
          
            tags$li("Practically speaking, the original matrix A is multiplied
                     by the same matrix after it is transposed (i.e. swapping columns with rows) which we call AT."),
            
            tags$li("Multiplying a matrix by its transpose creates an orthogonal square matrix with the same number of rows as columns.
                     The multiplication is done twice in different orders then the eigenvectors are found for each one. 
                     So the V matrix comes from the transposed matrix multiplied by the orignal matrix, ATA.
                     And the U matrix comes from the original matrix multiplied by the transposed matrix, AAT."),
            
            tags$li("The SVD method next finds the eigenvectors of ATA and AAT to create the V and U matricies respectively."),
            
            tags$li(p("For the D (or diagonal) matrix of singular values, this is calculated by taking the square roots of the eigenvalues from either 
                     of the V or U matricies. This method is well explained in R code in this excellent",
                     shiny::a("example",
                              href="https://rpubs.com/aaronsc32/singular-value-decomposition-r",
                              target="_blank"), ". And we can intuitively understand how eigen decomposition works through
                     the amazing 3BlueBrown1 tutorial,", 
                     shiny::a("Eigenvectors and eigenvalues",
                              href="https://youtu.be/PFDu9oVAE-g",
                              target="_blank")))
            ),
                    
          tags$h2("Why when we select all the singular vectors is the non-compressed image not identical to the original?"), 
          
          tags$ol(
            
            tags$li(p("Selecting all the singular values in the slider does not exactly re-create the image and it can look darker. 
            Before applying 
                      SVD the app has first scaled the matrix by subtracting the column mean and dividing each column by its standard deviation 
                      using base::scale(). Therefore, if you select the
                      maximum number of vectors in the image slider this creates the uncompressed image only after scaling. Scaling is a ",
                      shiny::a("reccomended step in Principal Components analysis",
                   href="https://www.theanalysisfactor.com/tips-principal-component-analysis/",
                   target="_blank"),"so that the inputs are on a similar scale.")
                   )
            ),
          
          tags$h2("For the 'Memos' matrix after SVD some zeros have a negative sign that is different to the original TDM"), 
          
          tags$ol(
            
            tags$li(p("The occurence of a negative sign that differs from the TDM is explained in",
                      shiny::a("'Resolving sign ambiguity in SVD'",
                               href="https://prod-ng.sandia.gov/techlib-noauth/access-control.cgi/2007/076422.pdf",
                               target="_blank"), "."))),
          
          tags$h2("Acknowledgements"), 
          
          tags$ol(
            
            tags$li(p("There are many great image compressions tutorials using R. Two I found useful and inspired this app
                    are, ", 
            shiny::a("Reconstructing Images Using PCA",
                     href="https://kieranhealy.org/blog/archives/2019/10/27/reconstructing-images-using-pca/",
                     target="_blank"), "by Kieran Healy, and the, ",
            shiny::a("Dimension reduction",
                              href="https://bookdown.org/rdpeng/exdata/dimension-reduction.html",
                     target="_blank"), "chapter in Roger Peng's Exploratory Data Analysis book.")),
            
            tags$li(p("The Apollo 11 image is from the amazing, ", 
            shiny::a("NASA commons Flickr account",
                     href="https://www.flickr.com/photos/nasacommons/36003144451/in/photolist-WRtwvR-d75YXb-fpXYto-d74CaC-fpXZaQ-d1zNAw-d1sZx1-d1zLvY-fpY11m-cZYRgd-cZWtD1-cZZp3b-d771sN-2i9pigF-2i9mJ75-cyfeoq-27UNwZz-cZWtvU-fpHH3V-2eL8TqU-cZZr1U-fpHK44-29dw4DL-fpXXLj-p9RdCr-X9RVAg-fpXWrY-J8ANj4-28coWRY-fpXYFS-cZYSNo-cZYRqJ-fpXYfU-fpHJ7g-fpHJW8-cAyhXA-d1sWNs-cZZm59-VRHpVL-23wHkUN-cAyisu-8wyRYe-cAyh9m-8pkqLQ-fpHHWM-d753qj-cZWtJ3-cZZp9h-d74F2s-fpHJ1T",
                     target="_blank"), ".")),
            
            tags$li(p("The book image is by, ",
            shiny::a("Suzy Hazelwookd on Pexels.com",
                              href="https://www.pexels.com/photo/low-light-photography-of-books-1301585/",
                     target="_blank"), ".")),
            
            tags$li(p("Finding the 'knee' of the scree plot uses this method in the ",
            shiny::a("inflection R package",
                     href="https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3043076",
                     target="_blank"), ".")),
              
            tags$li(p("The 'memos' text LSA example is from the ",
            shiny::a("Introduction to Latent Semantic Analysis",
                     href="http://lsa.colorado.edu/papers/dp1.LSAintro.pdf",
                     target="_blank"), " which itself takes the text example from the canonical",
            shiny::a("Deerwater (1990) paper",
                              href="https://www.cs.bham.ac.uk/~pxt/IDA/lsa_ind.pdf",
                     target="_blank"), ".")),
            
            tags$li(p("The 'Gold Silver Truck' text example is from the ",
            shiny::a("Information Retrieval book",
                     href="https://www.springer.com/gp/book/9781402030031",
                     target="_blank"), ".")),
            
            tags$li(p("The shinydashboardplus structure of this app began by using this well set out template by ",
            shiny::a("Dan Rogers",
                     href="https://github.com/bodhi-root/shiny-dashboard-template",
                     target="_blank"), ".")),
            
            tags$li(p("When you upload an image the app uses a combination of ",  
                        shiny::a("imager",
                              href="https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html",
                      target="_blank"), " and",
            shiny::a("magick",
                     href="https://cran.r-project.org/web/packages/magick/vignettes/intro.html",
                     target="_blank"), " R packages. Magick reduces the size of the uploaded image (to stop the app crashing)
                     and converts colour images to grayscale to simplify the compression process applied.")),
            
            tags$li(p("Using ",
            shiny::a("facet_zoom()",
                              href="https://rdrr.io/cran/ggforce/man/facet_zoom.html",
                      target="_blank"), "from the ggforce package clearly reveals the point at which the inflection R package 
            finds the 'knee' of the scree plot."))
            
          ),
          
    shiny::br(),
    # <a data-flickr-embed="true" href="https://www.flickr.com/photos/nasacommons/36003144451/in/photolist-WRtwvR-d75YXb-fpXYto-d74CaC-fpXZaQ-d1zNAw-d1sZx1-d1zLvY-fpY11m-cZYRgd-cZWtD1-cZZp3b-d771sN-2i9pigF-2i9mJ75-cyfeoq-27UNwZz-cZWtvU-fpHH3V-2eL8TqU-cZZr1U-fpHK44-29dw4DL-fpXXLj-p9RdCr-X9RVAg-fpXWrY-J8ANj4-28coWRY-fpXYFS-cZYSNo-cZYRqJ-fpXYfU-fpHJ7g-fpHJW8-cAyhXA-d1sWNs-cZZm59-VRHpVL-23wHkUN-cAyisu-8wyRYe-cAyh9m-8pkqLQ-fpHHWM-d753qj-cZWtJ3-cZZp9h-d74F2s-fpHJ1T" title="Candid Apollo"><img src="https://live.staticflickr.com/4313/36003144451_eb1dd9db12_c.jpg" width="795" height="533" alt="Candid Apollo"></a><script async src="//embedr.flickr.com/assets/client-code.js" charset="utf-8"></script>
    
    shiny::img(src='https://images.pexels.com/photos/1301585/pexels-photo-1301585.jpeg?cs=srgb&dl=low-light-photography-of-books-1301585.jpg&fm=jpg', height="50%", width="50%", align="left")  

          )
      )
    )
    
    return(ui)
  })
  

}
# good for reactivity understanding
# https://riptutorial.com/shiny/topic/10787/reactive--reactivevalue-and-eventreactive--observe-and-observeevent-in-shiny
TAB2_SERVER <- function(input, output, session) {

  ## ---- demo images ----

  # https://www.pexels.com/photo/low-light-photography-of-books-1301585/
  books <-
    imager::load.image("apollo11.jpg") %>%
    imager::grayscale()

  grey_boat <-
    imager::load.image("booksmall.png")

  ## ---- server logic ----

  # user uploads image

  img <- shiny::reactive({
    shiny::req(input$filedata)

    # read in image
    image <- magick::image_read(input$filedata$datapath)

    # get info on image
    info_image <- image_info(image)

    # Check if not too big, if so, make small
    if (info_image[6] > 400000) {
      image <- image %>% magick::image_resize("300x")
    } else {
    }

    # First check if grey. If not, make grey
    if (info_image[4] == "sRGB") {
      image <- image %>% magick::image_quantize(colorspace = "gray")
    } else {
    }

    # write small grey image as png
    magick::image_write(image, path = "image.png", format = "png")

    # re-load
    t <- imager::load.image("image.png")

    return(t)
  })

  shiny::observeEvent(input$filedata, {

    # when the file is uploaded change the drop down to be the user's image
    shiny::updateSelectInput(session, "select_image", selected = 1)

    # output the uploaded image
    output$imgPlot <- shiny::renderPlot({
      graphics::plot(img(), axes = FALSE, main = "Orginal image")
    })
  })


  # Runs based on the image selected
  shiny::observeEvent(c(input$select_image, input$radio_image), {
    if (input$select_image == 1) {
      image <- img()
    } else if (input$select_image == 2) {
      image <- books
    } else if (input$select_image == 3) {
      image <- grey_boat
    }

    # Output original image
    output$imgPlot <- shiny::renderPlot({
      graphics::plot(image, axes = FALSE, main = "Original image")
    })

    # scale image matrix if chosen by radio button
    if (input$radio_image == 1) {
      image_scaled <- image
    } else if (input$radio_image == 2) {
      image_scaled <- base::scale(image)
      image_scaled[is.nan(image_scaled)] <- 0
    }

    # Decompose orginal image with SVD
    SVD <- base::svd(image_scaled)

    # Find the maximum number of columns in the matrix for the slider and update it
    # max_dim <- max_dim_fun(SVD)
    shiny::updateSliderInput(session, "values", max = ncol(SVD$v))

    # Output variance explained by different numbers of singular values
    var_expl <- SVD$d^2 / sum(SVD$d^2)
    var_expl_df <- as.data.frame(var_expl) %>%
      dplyr::mutate(singular_values = row_number())

    # find knee
    knee <- inflection::uik(
      x = var_expl_df$singular_values,
      y = var_expl_df$var_expl
    )

    # output the knee value as info box
    output$kneeBox <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        knee, "...the number of singular values at the 'knee' of this variance explained plot",
        # icon = icon("list"),
        color = "light-blue",
        # width = NULL,
        href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3043076"
      )
    })

    # set the slider to be the knee value
    shiny::updateSliderInput(session, "values", value = knee)

    # output the variance explained plot
    output$var_explained <- shiny::renderPlot({
      shiny::isolate(var_expl_df %>%
        ggplot2::ggplot() +
        ggplot2::aes(
          x = singular_values,
          y = var_expl,
        ) +
        ggplot2::geom_point() +
        ggplot2::theme(
          legend.position = "none",
          plot.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 14, face = "bold")
        ) +
        ggforce::facet_zoom(
          xlim = c(1, knee),
          horizontal = FALSE,
          zoom.size = 0.8
        ) +
        ggplot2::labs(
          title = "Variance explained by singular values",
          subtitle = paste("The 'knee' is at", knee, "singular values"),
          x = "Number of singular values",
          y = "% of variance explained"
        ) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        theme(zoom.y = element_blank(), validate = FALSE))
    })

    # Convert the original image to a matrix and output as a table in blue
    image_mt <- image %>%
      as.matrix()

    output$image_mt_tbl <- DT::renderDataTable({
      data_table_fun(
        df = image_mt,
        table_title = paste("Matrix of original image: ", nrow(image_mt), " rows and ", ncol(image_mt), " columns"),
        colours = "Blues",
        font_perc = "80%",
        dp = 2
      )
    })
  })

  # Update the following with any user changes to these settings
  shiny::observeEvent(c(
    input$values,
    input$select_image,
    input$filedata,
    input$radio_image
  ), {
    if (input$select_image == 1) {
      image <- img()
    } else if (input$select_image == 2) {
      image <- books
    } else if (input$select_image == 3) {
      image <- grey_boat
    }

    # sacle image matrix if chosen by radio button
    if (input$radio_image == 1) {
      image_scaled <- image
    } else if (input$radio_image == 2) {
      image_scaled <- base::scale(image)
      image_scaled[is.nan(image_scaled)] <- 0
    }

    # Decompose orginal image with SVD
    SVD <- base::svd(image_scaled)

    # Find the maximum number of columns in the matrix for the slider and update it
    shiny::updateSliderInput(session, "values", max = ncol(SVD$u))

    # this truncates the decomposed matrices U, D and V by the value in the slider
    u <- SVD$u[, 1:input$values]
    d <- base::diag(SVD$d[1:input$values]) # placing values on the diagonal
    v <- base::t(SVD$v[, 1:input$values]) # transpose the matrix (swap rows with columns)

    # multiply the truncated matrices into the compressed image
    reconstruct_mt <- u %*% d %*% v

    # Output the compressed matrix as yellows?
    output$reconstruct_mt_tbl <- DT::renderDataTable({
      data_table_fun(
        df = reconstruct_mt,
        table_title = paste("Matrix of compressed image (U x D x V): ", nrow(reconstruct_mt), " rows and ", ncol(reconstruct_mt), " columns"),

        colours = "Oranges",
        font_perc = "80%",
        dp = 2
      )
    })

    reconstruct <- imager::as.cimg(reconstruct_mt)

    # Output the compressed image
    output$imgPlot_red <- shiny::renderPlot({
      graphics::plot(reconstruct, axes = FALSE, main = paste0("Compressed with first ", as.character(input$values), " values"))
    })


    # U matrix table
    output$u_tbl <- DT::renderDataTable({
      data_table_fun(
        df = u,
        table_title = paste("Truncated U matrix: ", nrow(u), " rows and ", ncol(u), " columns"),
        colours = "Greens",
        font_perc = "80%",
        dp = 2
      )
    })

    # V matrix table
    output$v_tbl <- DT::renderDataTable({
      data_table_fun(
        df = v,
        table_title = paste("Truncated V matrix: ", nrow(v), " rows and ", ncol(v), " columns"),
        colours = "Greens",
        font_perc = "80%",
        dp = 2
      )
    })

    # D matrix table
    output$d_tbl <- DT::renderDataTable({
      data_table_fun(
        df = d,
        table_title = paste("Truncated D matrix: ", nrow(d), " rows and ", ncol(d), " columns"),
        colours = "Greens",
        font_perc = "80%",
        dp = 2
      )
    })
  })


  ## ---- user interface sidebar ----

  output$TAB2_CONTROLS <- shiny::renderUI({
    controls <- base::list()

    return(controls)
  })

  ## ---- user interface main body ----

  output$TAB2_BODY <- shiny::renderUI({
    img_plot <- shiny::plotOutput(outputId = "imgPlot", width = "500px", height = "300px")
    red_img_plot <- shiny::plotOutput(outputId = "imgPlot_red", width = "500px", height = "300px")

    var_expl <- shiny::plotOutput(outputId = "var_explained", width = "500px", height = "300px")

    table_u <- DT::dataTableOutput(outputId = "u_tbl")
    table_v <- DT::dataTableOutput(outputId = "v_tbl")
    table_d <- DT::dataTableOutput(outputId = "d_tbl")

    table_reconstruct_mt <- DT::dataTableOutput(outputId = "reconstruct_mt_tbl")
    table_image_mt <- DT::dataTableOutput(outputId = "image_mt_tbl")

    ui <- shiny::fluidPage(
      width = 12,

      tags$h3("1 Select an image and compress with the slider"),

      shiny::wellPanel(
        shiny::fluidRow(
          column(
            6,

            img_plot,

            # select your image
            shiny::selectInput("select_image",
              label = "Select image to compress",
              choices = list(
                "Your upload" = 1,
                "Apollo 11" = 2,
                "Books" = 3
              ),
              selected = 2
            ),

            # upload your own image
            shiny::fileInput(
              inputId = "filedata",
              label = "Or upload your own image (optional)"
              # ,
              # accept = c(".png",".jpg")
            )
          ),

          column(
            6,

            red_img_plot,

            # select how many singular values to compress
            shiny::sliderInput(
              inputId = "values",
              label = "Select number of values to compress image",
              min = 2,
              max = 20,
              value = 2,
              step = 1
            ),

            shiny::radioButtons("radio_image",
              label = "Centre and scale image matrix?",
              choices = list("Do not centre or scale" = 1, "Centre & scale" = 2),
              inline = TRUE,
              selected = 1
            )
          )
        )
      ),

      shiny::wellPanel(
        shiny::fluidRow(
          column(
            6,

            table_image_mt
          ),

          column(
            6,


            shinydashboard::valueBoxOutput("kneeBox", width = NULL),

            var_expl
          )
        ),

        shiny::fluidRow(
          column(
            6,
            boxPlus(
              title = "Original image matrix",
              closable = TRUE,
              width = 12,
              enable_label = TRUE,
              label_text = 1,
              label_status = "info",
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              p("The blue table is the matrix of numbers used to plot the original image 
                above. Using the slider, we manipulate this matrix with Singular Value Decomposition (SVD)
                to create three new matrices in green. If we truncate and multiply together the three green 
                matrices, this creates the orange matrix below that has less information. However, the most important 
                information is kept.  The new orange matrix is plotted as the compressed image on the top right.")
            )
          ),

          column(
            6,
            boxPlus(
              title = "Variance explained",
              closable = TRUE,
              width = 12,
              enable_label = TRUE,
              label_text = 2,
              label_status = "info",
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              p(" 
                          The 'scree' plot above tells us what percentage of variance in the original blue matrix is 
                          explained by the number of singular values selected from the green Diagonal matrix. The 'knee' is the 
                          number of values from where the amount of additional 
                          variance explained tails off rapidly if more values are selected.")
            )
          )
        )
      ),

      tags$h3("2 The original matrix de-composed by SVD into U, D and V and then truncated"),

      shiny::wellPanel(
        fluidRow(
          column(4, table_u),
          column(4, table_d),
          column(4, table_v)
        ),

        fluidRow(
          column(
            12,
            boxPlus(
              title = "Original image matrix is decomposed by SVD into U, D, and V matrices that are truncated by the slider",
              closable = TRUE,
              width = 12,
              enable_label = TRUE,
              label_text = 3,
              label_status = "info",
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              p("The R function base::svd() decomposes the blue matrix of the original image into these 
               three green matrices: U, D, and V. The number selected in the slider truncates each matrix. 
               The truncated matrices are multiplied together to create the orange matrix below of the compressed image. 
               If you select the maximum number
                in the slider there will be no truncation, the original image will be re-created exactly. 
                This demonstrates that SVD factors the original blue matrix
                into the green U, D and V with no loss of information. The slider truncates the right-most columns of the 
                U matrix (called the left singular vectors), the bottom rows of the V matrix, and the right-most values of the 
                D matrix (called singular values).")
            )
          )
        )
      ),

      tags$h3("3 The compressed matrix (U x V x D)."),

      shiny::wellPanel(
        fluidRow(
          column(
            12,

            table_reconstruct_mt,

            boxPlus(
              title = "Truncated U, D and V matrices when multiplied together to create the compressed image matrix",
              closable = TRUE,
              width = 12,
              enable_label = TRUE,
              label_text = 4,
              label_status = "info",
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              p("The green U, D and V matrices above truncated by the slider are multiplied together to 
                 create this orange matrix. This matrix is used to plot the compressed image above. Note that when selecting the
                maximum number in the slider, and the option to 'Centre and Scale' is chosen,
                the image will look darker than the original because of the change to the original image matrix from this transformation.")
            )
          )
        )
      )
    )
    return(ui)
  })
}

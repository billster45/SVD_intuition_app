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
  shiny::observeEvent(input$select_image, {

    # if ( !is.null(input$filedata) & input$select_image == 1) {
    if (input$select_image == 1) {
      image <- img()
      # } else if ( is.null(input$filedata) & input$select_image == 1) {
      #     return()
    } else if (input$select_image == 2) {
      image <- books
    } else if (input$select_image == 3) {
      image <- grey_boat
    }

    # Output original image
    output$imgPlot <- shiny::renderPlot({
      graphics::plot(image, axes = FALSE, main = "Original image")
    })

    image_scaled <- base::scale(image)

    # replace any NaN with 0
    image_scaled[is.nan(image_scaled)] <- 0

    # Decompose orginal image with SVD
    SVD <- base::svd(image_scaled)

    # Find the maximum number of columns in the matrix for the slider and update it
    # max_dim <- max_dim_fun(SVD)
    shiny::updateSliderInput(session, "vectors", max = ncol(SVD$v))

    # Output variance explained by different numbers of singular vectors
    var_expl <- SVD$d^2 / sum(SVD$d^2)
    var_expl_df <- as.data.frame(var_expl) %>%
      dplyr::mutate(singular_vectors = row_number())

    # find knee
    knee <- inflection::uik(
      x = var_expl_df$singular_vectors,
      y = var_expl_df$var_expl
    )

    output$kneeBox <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        knee, "singular vectors at the 'knee' of the variance explained plot",
        # icon = icon("list"),
        color = "light-blue",
        # width = NULL,
        href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3043076"
      )
    })

    shiny::updateSliderInput(session, "vectors", value = knee)

    output$var_explained <- shiny::renderPlot({
      shiny::isolate(var_expl_df %>%
        ggplot2::ggplot() +
        ggplot2::aes(
          x = singular_vectors,
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
          title = "Variance explained by singular vectors",
          subtitle = paste("The 'knee' is at", knee, "singular vectors"),
          x = "Number of singular vectors",
          y = "% of variance explained"
        ) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        theme(zoom.y = element_blank(), validate = FALSE))
    })

    # Convert the original image to a matrix and output as a table
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

  # vectors change
  shiny::observeEvent(c(
    input$vectors,
    input$select_image,
    input$filedata
  ), {
    if (input$select_image == 1) {
      image <- img()
    } else if (input$select_image == 2) {
      image <- books
    } else if (input$select_image == 3) {
      image <- grey_boat
    }

    image_scaled <- base::scale(image)

    # replace any NaN with 0
    image_scaled[is.nan(image_scaled)] <- 0

    # Decompose orginal image with SVD
    SVD <- base::svd(image_scaled)

    # Find the maximum number of columns in the matrix for the slider and update it
    shiny::updateSliderInput(session, "vectors", max = ncol(SVD$u))

    u <- SVD$u[, 1:input$vectors]
    d <- base::diag(SVD$d[1:input$vectors]) # placing values on the diagonal
    v <- base::t(SVD$v[, 1:input$vectors]) # transpose the matrix (swap rows with columns)

    reconstruct_mt <- u %*% d %*% v

    # Output the compressed matrix
    output$reconstruct_mt_tbl <- DT::renderDataTable({
      data_table_fun(
        df = reconstruct_mt,
        table_title = paste("Matrix of compressed image (U x D x V): ", nrow(reconstruct_mt), " rows and ", ncol(reconstruct_mt), " columns"),

        colours = "Blues",
        font_perc = "80%",
        dp = 2
      )
    })

    reconstruct <- imager::as.cimg(reconstruct_mt)

    # Output the compressed image
    output$imgPlot_red <- shiny::renderPlot({
      graphics::plot(reconstruct, axes = FALSE, main = paste0("Compressed with first ", as.character(input$vectors), " vectors"))
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

      shiny::wellPanel(
        shiny::fluidRow(
          column(
            6,

            img_plot,

            # select your image
            shiny::selectInput("select_image",
              label = "1. Select image to compress",
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
              label = "2. Upload an image (optional)"
              # ,
              # accept = c(".png",".jpg")
            )
          ),

          column(
            6,

            red_img_plot,

            # select how many singular vectors to compress
            shiny::sliderInput(
              inputId = "vectors",
              label = "3. Select number of singular vectors to compress image",
              min = 2,
              max = 20,
              value = 2,
              step = 1
            )
          )
        ),
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
              title = "Original image and matrix",
              closable = TRUE,
              width = 12,
              enable_label = TRUE,
              label_text = 1,
              label_status = "info",
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              p("This matrix of numbers contains all the information needed to plot the original image 
                   above ready for compressing with the Singular Value Decomposition method.")
            )
          ),

          column(
            6,
            boxPlus(
              title = "Compressed image and variance explained",
              closable = TRUE,
              width = 12,
              enable_label = TRUE,
              label_text = 2,
              label_status = "info",
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              p("The image above has been compressed using the number of singular vectors selected in the slider. 
                          The 'scree' plot above shows what percentage of variance in the original matrix is 
                          explained by the number of singular vectors selected. The 'knee' is the 
                          number of vectors where the percentage of additional 
                          variance explained will tail off rapidly if more vectors are selected.")
            )
          )
        )
      ),

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
              title = "Original image matrix is decomposed by SVD into U, D, and V matricies that are truncated by the slider",
              closable = TRUE,
              width = 12,
              enable_label = TRUE,
              label_text = 3,
              label_status = "info",
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              p("The R function base::svd() decomposes the matrix of the original image into these 
               three matricies: U, D, and V. If they are multiplied together without truncating they would exactly re-create the image.
               However, the number selected in the slider truncates each matrix. Then, when multiplied together, 
               they create the matrix below of the compressed image. Selecting the maximum number
                of vectors in the slider will recreate the original image with no compression.")
            )
          )
        )
      ),

      shiny::wellPanel(
        fluidRow(
          column(
            12,

            table_reconstruct_mt,

            boxPlus(
              title = "Truncated U, D and V matricies multiplied together to create the compressed image matrix",
              closable = TRUE,
              width = 12,
              enable_label = TRUE,
              label_text = 4,
              label_status = "info",
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              p("The three truncated U, D and V matricies above have been trunctad by the slider then multiplied together to 
                 create this matrix. This is the matrix that is used to plot the compressed image above.")
            )
          )
        )
      )
    )
    return(ui)
  })
}

#
TAB3_SERVER <- function(input, output, session) {


  # https://tutorials.quanteda.io/basic-operations/tokens/tokens_select/
  txt1 <- c(
    c1 = "Human machine interface for ABC computer applications",
    c2 = "A survey of user opinion of computer system response time",
    c3 = "The EPS user interface management system",
    c4 = "System and human system engineering testing of EPS",
    c5 = "Relation of user perceived response time to error measurement",
    m1 = "The generation of random, binary, ordered trees",
    m2 = "The intersection graph of paths in trees",
    m3 = "Graph minors IV: Widths of trees and well-quasi-ordering",
    m4 = "Graph minors: A survey"
  )

  txt2 <- c(
    d1 = "Shipment of gold damaged in a fire",
    d2 = "Delivery of silver arrived in a silver truck",
    d3 = "Shipment of gold arrived in a truck"
  )

  ## ---- server logic ----


  # user selects and image to compress
  shiny::observeEvent(input$select_text, {
    if (input$select_text == 1) {
      txt <- txt1
    } else if (input$select_text == 2) {
      txt <- txt2
    }


    # print the raw text
    txt_df <- txt %>% as.data.frame()

    output$txt_tbl <- DT::renderDataTable({
      txt_df %>%
        DT::datatable(
          caption = "The text to be...",
          extensions = c("Buttons"),
          options = list(autoWidth = TRUE)
        )
    })

    # convert text to matrix
    toks <- quanteda::tokens(txt)
    toks_nostop <- quanteda::tokens_select(toks, c("human", "interface", "computer", "user", "system", "response", "time", "EPS", "survey", "trees", "graph", "minors"),
      selection = "keep", padding = FALSE
    )

    if (input$select_text == 1) {
      mydfm <- quanteda::dfm(toks_nostop)
    } else if (input$select_text == 2) {
      mydfm <- quanteda::dfm(toks, remove = stopwords())
    }

    # Convert to a term document feature matrix to match the example
    tdm <<- base::t(as.matrix(mydfm))

    # output the tdm
    output$tdm_tbl <- DT::renderDataTable({
      data_table_fun(
        df = tdm,
        table_title = paste("Term Document Matrix (TDM): ", nrow(tdm), " rows and ", ncol(tdm), " columns"),
        colours = "Blues",
        font_perc = "100%",
        dp = 2
      )
    })

    # decompose the matrix
    SVD_text <<- base::svd(tdm)

    # Find the maximum number of columns in the matrix for the slider and update it
    shiny::updateSliderInput(session, "vectors_text", max = ncol(SVD_text$u))
  })

  # The User SVD the text
  shiny::observeEvent(c(
    input$select_text,
    input$vectors_text
  ), {
    u_text <- SVD_text$u[, 1:input$vectors_text]
    d_text <- base::diag(SVD_text$d[1:input$vectors_text]) # placing values on the diagonal
    v_text <- base::t(SVD_text$v[, 1:input$vectors_text]) # transpose the matrix (swap rows with columns)

    # output the decomposed matricies
    # U matrix table
    output$u_text_tbl <- DT::renderDataTable({
      data_table_fun(
        df = u_text,
        table_title = paste("U matrix: ", nrow(u_text), " rows and ", ncol(u_text), " columns"),
        colours = "Greens",
        font_perc = "100%",
        dp = 2
      )
    })

    # V matrix table
    output$v_text_tbl <- DT::renderDataTable({
      data_table_fun(
        df = v_text,
        table_title = paste("V matrix: ", nrow(v_text), " rows and ", ncol(v_text), " columns"),
        colours = "Greens",
        font_perc = "100%",
        dp = 2
      )
    })

    # D matrix table
    output$d_text_tbl <- DT::renderDataTable({
      data_table_fun(
        df = d_text,
        table_title = paste("D matrix: ", nrow(d_text), " rows and ", ncol(d_text), " columns"),
        colours = "Greens",
        font_perc = "100%",
        dp = 2
      )
    })

    # Reconstruct
    reconstruct_mt_text <- u_text %*% d_text %*% v_text

    colnames(reconstruct_mt_text) <- colnames(tdm)
    rownames(reconstruct_mt_text) <- rownames(tdm)

    output$reconstruct_mt_text_tbl <- DT::renderDataTable({
      data_table_fun(
        df = reconstruct_mt_text,
        table_title = paste("Matrix of compressed image (U x D x V): ", nrow(reconstruct_mt_text), " rows and ", ncol(reconstruct_mt_text), " columns"),
        colours = "Blues",
        font_perc = "100%",
        dp = 2
      )
    })
  })

  ## ---- user interface sidebar ----


  output$TAB3_CONTROLS <- shiny::renderUI({
    controls <- base::list()

    return(controls)
  })

  ## ---- user interface main body ----


  output$TAB3_BODY <- renderUI({
    table_tdm <- DT::dataTableOutput(outputId = "tdm_tbl")
    table_txt <- DT::dataTableOutput(outputId = "txt_tbl")

    table_u_text <- DT::dataTableOutput(outputId = "u_text_tbl")
    table_v_text <- DT::dataTableOutput(outputId = "v_text_tbl")
    table_d_text <- DT::dataTableOutput(outputId = "d_text_tbl")
    table_reconstruct_mt_text_tbl <- DT::dataTableOutput(outputId = "reconstruct_mt_text_tbl")


    ui <- shiny::fluidPage(
      width = 12,

      shiny::wellPanel(
        shiny::fluidRow(
          column(
            6,

            # select your text
            shiny::selectInput("select_text",
              label = "1. Select text",
              choices = list(
                "Memos" = 1,
                "Gold Silver Truck" = 2
              ),
              selected = 1
            ),

            table_txt
          ),

          column(6, boxPlus(
            title = "SVD can be applied to text too",
            width = 12,
            closable = TRUE,
            enable_label = TRUE,
            label_text = 1,
            label_status = "info",
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            p("Text documents can be turned into matrix of numbers and SVD applied too. If we truncate the
          three matricies and multiply (like we just have for an image) this is now called Latent Semantic Analysis.
            'Latent' means hidden, 'Semantic' is meaning (i.e. hidden meaning analysis). In LSA, a document is given 
            some of the information value from words not inside the document if those words are found 
            inside documents that are similar.")
          )),
        ),

        shiny::fluidRow(
          column(6, table_tdm),

          column(
            6,
            table_reconstruct_mt_text_tbl,

            # select how many singular vectors to compress
            shiny::sliderInput(
              inputId = "vectors_text",
              label = "2. Select number of singular vectors:",
              min = 2,
              max = 15,
              value = 2,
              step = 1
            )
          )
        ),




        shiny::fluidRow(
          column(6, boxPlus(
            title = "Term Document Matrix TDM",
            width = 12,
            closable = TRUE,
            enable_label = TRUE,
            label_text = 2,
            label_status = "info",
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            p("This table of text documents is turned into the Term Document Matrix (TDM) next to it. The TDM simply counts
             the number of times each word appears in each document. Stop words that nearly always appear in 
              most documents are excluded from the TDM (e.g. 'and' or 'the').")
          )),



          column(6, boxPlus(
            title = "Latent Semantic Analysis Matrix",
            width = 12,
            closable = TRUE,
            enable_label = TRUE,
            label_text = 3,
            label_status = "info",
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            p("SVD has decompose the matrix above into the U, D and V matricies below.
           The de-composed matrices are then truncated by the slider before being multiplied together to create the matrix above.
           Notice how words (or terms) that don't appear in some documents get a value after a truncated SVD is applied. 
           For example, in the 'Memos' documents, look at the word 'trees' in document m4. When
             2 singular vectors are chosen in the slider this has a value of 0.66 in the truncated SVD
             matrix. Whereas in the TDM the value is zero. So SVD can make the term 'trees' available for text search or 
             text classification even though is does not appear in the document.")
          ))
        )
      ),
      shiny::wellPanel(
        shiny::fluidRow(
          column(4, table_u_text),
          column(4, table_d_text),
          column(4, table_v_text)
        ),


        shiny::fluidRow(
          column(12, boxPlus(
            title = "TDM decomposed by SVD into U, D, and V matricies and truncated by slider",
            width = 12,
            closable = TRUE,
            enable_label = TRUE,
            label_text = 4,
            label_status = "info",
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            p("These are the U, D and V matricies created from the Singular Value Decompostion of the TDM.
         Each matrix is truncated by the number of singular vectors selected in the slider. Try setting the slider
      to the maximum number of vectors. When the full non-truncated matricies are
        multiplied, the matrix values above are identical to the orginal TDM. This demonstrates that the U, D and V
        matricies contain all the information in the original TDM.")
          ))
        )
      )
    )

    return(ui)
  })
}

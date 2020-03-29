TAB3_SERVER <- function(input, output, session) {

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

  shiny::observeEvent(c(
    input$select_text,
    input$values_text,
    input$radio
  ), {
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

    
    # Tokensie the text
    toks <- quanteda::tokens(txt)
    
    # Depending on docs chosen, tokenise the text to single words and remove stop words
    if (input$select_text == 1) {
      toks_nostop <- quanteda::tokens_select(toks, c("human", "interface", "computer", "user", "system", "response", "time", "EPS", "survey", "trees", "graph", "minors"),
                                             selection = "keep", padding = FALSE)
        mydfm <- quanteda::dfm(toks_nostop)
    } else if (input$select_text == 2) {
      mydfm <- quanteda::dfm(toks, remove = stopwords())
    }

    # Transpose Document Term Matrix to a Term Document Matrix to match the orientation in the literature
    tdm <<- base::t(as.matrix(mydfm))

    # output the tdm as a table
    output$tdm_tbl <- DT::renderDataTable({
      data_table_fun(
        df = tdm,
        table_title = paste("Term Document Matrix (TDM): ", nrow(tdm), " rows and ", ncol(tdm), " columns"),
        colours = "Blues",
        font_perc = "100%",
        dp = 2
      )
    })

    # decompose the matrix with SVD
    SVD_text <<- base::svd(tdm)

    # Find the maximum number of columns in the matrix for the slider and update it
    shiny::updateSliderInput(session, "values_text", max = ncol(SVD_text$u))

    # Truncate the matrices from SVD according to the slider value
    u_text <- SVD_text$u[, 1:input$values_text]
    rownames(u_text) <- rownames(tdm)
    d_text <- base::diag(SVD_text$d[1:input$values_text]) # placing values on the diagonal
    v_text <- base::t(SVD_text$v[, 1:input$values_text]) # transpose the matrix (swap rows with columns)
    colnames(v_text) <- colnames(tdm)

    # output the decomposed matrices
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

    # Multiply the truncated matrices together to perform the LSA
    reconstruct_mt_text <- u_text %*% d_text %*% v_text


    # Add the document and word names to the reconstructed matrix
    colnames(reconstruct_mt_text) <- colnames(tdm)
    rownames(reconstruct_mt_text) <- rownames(tdm)

    output$reconstruct_mt_text_tbl <- DT::renderDataTable({
      data_table_fun(
        df = reconstruct_mt_text,
        table_title = paste("Latent Semantic Analysis of the TDM (U x D x V): ", nrow(reconstruct_mt_text), " rows and ", ncol(reconstruct_mt_text), " columns"),
        colours = "Oranges",
        font_perc = "100%",
        dp = 2
      )
    })


    ## ---- PCA Section SVD----

    # Option for user to first scale and centre the tdm for both the SVD and prcomp 
    if (input$radio == 2) {
      tdm_scale <- tdm
      myPCA <- stats::prcomp(tdm, scale. = FALSE, center = FALSE)
    } else if (input$radio == 1) {
      tdm_scale <- base::scale(tdm, center = TRUE, scale = TRUE)
      myPCA <- stats::prcomp(tdm, scale. = TRUE, center = TRUE)
    }

    # output the tdm (whether raw or centred and scaled)
    output$tdm_scale_tbl <- DT::renderDataTable({
      data_table_fun(
        df = tdm_scale,
        table_title = "Term Document Matrix (TDM)",
        colours = "Blues",
        font_perc = "100%",
        dp = 2
      )
    })

    # SVD of the scaled (or not) tdm
    SVD_text_scaled <- base::svd(tdm_scale)

    u_text_scaled <- SVD_text_scaled$u
    rownames(u_text_scaled) <- rownames(tdm)
    d_text_scaled <- base::diag(SVD_text_scaled$d) # placing values on the diagonal
    v_text_scaled <- SVD_text_scaled$v
    rownames(v_text_scaled) <- colnames(tdm)

    # output the decomposed matrices
    # U matrix table
    output$u_text_scaled_tbl <- DT::renderDataTable({
      data_table_fun(
        df = u_text_scaled,
        table_title = "U matrix",
        colours = "Greens",
        font_perc = "100%",
        dp = 2
      )
    })

    # V matrix table
    output$v_text_scaled_tbl <- DT::renderDataTable({
      data_table_fun(
        df = v_text_scaled,
        table_title = "V matrix: equivalent to rotation from prcomp",
        colours = "Greens",
        font_perc = "100%",
        dp = 2
      )
    })

    # D matrix table
    output$d_text_scaled_tbl <- DT::renderDataTable({
      data_table_fun(
        df = d_text_scaled,
        table_title = "D matrix",
        colours = "Greens",
        font_perc = "100%",
        dp = 2
      )
    })

    # convert D matrix to the eigenvalues calcualted by PCA
    # using this guide https://genomicsclass.github.io/book/pages/pca_svd.html
    eigenvalues_SVD <- SVD_text_scaled$d^2 / (nrow(tdm) - 1)

    eigenvalues_svd_df <- as.data.frame(eigenvalues_SVD)
    eigenvalues_svd_df <- base::t(eigenvalues_svd_df)

    output$SVD_eigen_tbl <- DT::renderDataTable({
      data_table_fun(
        df = eigenvalues_svd_df,
        table_title = "Eigenvalues: equivalent to sdev squared from prcomp()",
        colours = "Greens",
        font_perc = "100%",
        dp = 2
      )
    })

    # U x V to create SVD PCA Scores equivalent
    SVD_PCAscores <- SVD_text_scaled$u %*% base::diag(SVD_text_scaled$d)
    rownames(SVD_PCAscores) <- rownames(tdm)

    output$SVD_PCAscores_tbl <- DT::renderDataTable({
      data_table_fun(
        df = SVD_PCAscores,
        table_title = "U x D matrix: equivalent to x from prcomp()",
        colours = "Greens",
        font_perc = "100%",
        dp = 2
      )
    })

    # scores ggplot
    output$SVD_PCAscores_plot <- shiny::renderPlot({
      shiny::isolate(
        SVD_PCAscores[, 1:2] %>%
          as.data.frame() %>%
          dplyr::mutate(terms = rownames(SVD_PCAscores)) %>%
          ggplot2::ggplot() +
          ggplot2::aes(
            x = V1,
            y = V2,
            label = terms
          ) +
          ggplot2::geom_point() +
          ggrepel::geom_text_repel(aes(label = terms)) +
          ggplot2::geom_hline(
            yintercept = 0,
            linetype = 2,
            linetype = 2
          ) +
          ggplot2::geom_vline(xintercept = 0) +
          ggplot2::theme_minimal() +
          ggplot2::labs(
            title = "PCA Scores for each Term",
            subtitle = "The scores in the first and second columns of the U x D matrix"
          )
      )
    })

    # principal axes,directions or eigenvectors plot
    # https://stats.stackexchange.com/a/141531
    SVD_PCAloadings <- SVD_text_scaled$v[, 1:2]
    rownames(SVD_PCAloadings) <- colnames(tdm)
    SVD_PCAloadings <- SVD_PCAloadings %>%
      as.data.frame() %>%
      dplyr::mutate(docs = rownames(SVD_PCAloadings)
                    #V1 = scales::rescale(V1, to=c(-1,1)), #https://stats.stackexchange.com/questions/104306/what-is-the-difference-between-loadings-and-correlation-loadings-in-pca-and
                    #V2 = scales::rescale(V2, to=c(-1,1))
                    #V1 = scale(V1),
                    #V2 = scale(V2)
              )
    
    

    output$SVD_PCAloadings_plot <- shiny::renderPlot({
      shiny::isolate(
        SVD_PCAloadings %>%
          ggplot2::ggplot() +
          ggplot2::aes(
            x = V1,
            y = V2,
            label = docs
          ) +
          ggplot2::geom_point(colour = "white") +
          ggrepel::geom_text_repel(aes(label = docs), # https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html
            colour = "#3F9DAB",
            force = 10
          ) +
          ggplot2::geom_hline(
            yintercept = 0,
            linetype = 2
          ) +
          ggplot2::geom_vline(
            xintercept = 0,
            linetype = 2
          ) +
          # ggplot2::coord_fixed(ratio=1) +
          # ggplot2::ylim(-1,1) +
          # ggplot2::xlim(-1,1) +
          ggplot2::theme_minimal() +
          ggplot2::geom_segment(aes(x = 0, y = 0, xend = V1, yend = V2),
            arrow = arrow(
              length = unit(0.2, "cm"),
              type = "closed",
            ),
            colour = "#3F9DAB"
          ) +
          ggplot2::labs(
            title = "PCA Eigenvectors (or principle axes/directions) for each Document",
            subtitle = "The scores in the first and second columns of the V matrix"
          )
      )
    })

    # SVD bi-plot
    # SVD_PCAloadings

    # Do the scree plot by hand with SVD first
    var_expl_text <- SVD_text_scaled$d^2 / sum(SVD_text_scaled$d^2)
    var_expl_text_df <- as.data.frame(var_expl_text) %>%
      dplyr::mutate(singular_values = row_number())

    output$var_explained_text <- shiny::renderPlot({
      shiny::isolate(var_expl_text_df %>%
        ggplot2::ggplot() +
        ggplot2::aes(
          x = singular_values,
          y = var_expl_text,
          label = var_expl_text
        ) +
        ggplot2::geom_point() +
        ggplot2::geom_line(linetype = 2) +
        ggplot2::theme(
          legend.position = "none",
          plot.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 14, face = "bold")
        ) +
        ggrepel::geom_text_repel(aes(label = scales::percent(var_expl_text,
          accuracy = 0.1
        ))) +
        ggplot2::labs(
          title = "Scree plot from base::svd() results",
          subtitle = "Each squared singular value divided by the sum total of all squared values",
          x = "Number of singular values",
          y = "% of variance explained"
        ) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::scale_x_continuous(breaks = seq(
          min(var_expl_text_df$singular_values),
          max(var_expl_text_df),
          1
        )) +
        ggplot2::theme_minimal())
    })


    ## ---- PCA Section using prcomp----

    # Output PCA Scores
    PCAscores <- myPCA$x

    output$PCAscores_tbl <- DT::renderDataTable({
      data_table_fun(
        df = PCAscores,
        table_title = "PCA Scores: x from stats::prcomp()",
        colours = "Greens",
        font_perc = "100%",
        dp = 2
      )
    })

    # Output PCA Scores (that match the matrix abobe)
    PCAloadings <- myPCA$rotation

    output$PCAloadings_tbl <- DT::renderDataTable({
      data_table_fun(
        df = PCAloadings,
        table_title = "PCA Loadings: rotation from stats::prcomp()",
        colours = "Greens",
        font_perc = "100%",
        dp = 2
      )
    })


    # PCA scree plot
    output$factoextra_scree <- shiny::renderPlot({
      shiny::isolate(
        factoextra::fviz_eig(myPCA,
          addlabels = TRUE,
          geom = "line",
          ylim = c(0, 50)
        ) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title = "factoextra Scree plot")
      )
    })

    # output the PCA biplot
    output$factoextra_biplot <- shiny::renderPlot({
      shiny::isolate(
        factoextra::fviz_pca_biplot(myPCA,
          repel = TRUE,
          col.var = "#2E9FDF", # Document colour
          col.ind = "#696969" # Terms colour
        ) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title = "factoextra Biplot of first two Principal Components")
      )
    })

    # output the eigen values
    eigenvalues_PCA <- myPCA$sdev^2
    eigenvalues_pca_df <- as.data.frame(eigenvalues_PCA)
    eigenvalues_pca_df <- base::t(eigenvalues_pca_df)

    output$PCAeigen_tbl <- DT::renderDataTable({
      data_table_fun(
        df = eigenvalues_pca_df,
        table_title = "Eigenvalues from sdev squared from stats::prcomp()",
        colours = "Greens",
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

    # PCA
    table_tdm_scale <- DT::dataTableOutput(outputId = "tdm_scale_tbl")
    var_expl_text <- shiny::plotOutput(outputId = "var_explained_text", width = "500px", height = "300px")
    table_tdm1 <- DT::dataTableOutput(outputId = "tdm_tbl")
    factoextra_scree_plt <- shiny::plotOutput(outputId = "factoextra_scree", width = "500px", height = "300px")
    factoextra_biplot_plt <- shiny::plotOutput(outputId = "factoextra_biplot", width = "500px", height = "300px")

    table_u_text_scaled <- DT::dataTableOutput(outputId = "u_text_scaled_tbl")
    table_v_text_scaled <- DT::dataTableOutput(outputId = "v_text_scaled_tbl")
    table_d_text_scaled <- DT::dataTableOutput(outputId = "d_text_scaled_tbl")
    table_SVD_PCAscores <- DT::dataTableOutput(outputId = "SVD_PCAscores_tbl")
    table_SVD_eigen <- DT::dataTableOutput(outputId = "SVD_eigen_tbl")


    table_PCAloadings <- DT::dataTableOutput(outputId = "PCAloadings_tbl")
    table_PCAscores <- DT::dataTableOutput(outputId = "PCAscores_tbl")
    table_PCAeigen <- DT::dataTableOutput(outputId = "PCAeigen_tbl")


    SVD_PCAscores_plt <- shiny::plotOutput(outputId = "SVD_PCAscores_plot", width = "500px", height = "300px")
    SVD_PCAloadings_plt <- shiny::plotOutput(outputId = "SVD_PCAloadings_plot", width = "500px", height = "300px")





    ui <- shiny::fluidPage(
      width = 12,

      tags$h3("1 Select a set of documents then use the slider to control truncation of matrices"),

      shiny::wellPanel(
        shiny::fluidRow(
          column(
            6, boxPlus(
              title = "SVD on text",
              width = 12,
              closable = TRUE,
              enable_label = TRUE,
              label_text = 1,
              label_status = "info",
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              p("We can also apply SVD to documents if we convert them into a matrix that counts the words. 
              If we apply SVD to a matrix of word counts in documents, then truncate and multiply the
          three matrices SVD creates, this is known as Latent Semantic Analysis.
            'Latent' means hidden, 'Semantic' is meaning (i.e. hidden meaning analysis). In LSA, a word not in a document is given 
            some information value if that word occurs in other document that are similar (i.e. it shares other words with other documents).")
            ),
            # select your text
            shiny::selectInput("select_text",
              label = "Select text",
              choices = list(
                "Memos" = 1,
                "Gold Silver Truck" = 2
              ),
              selected = 1
            )
          ),

          column(6, table_txt)
        ),

        shiny::fluidRow(
          column(6, table_tdm),

          column(
            6,
            table_reconstruct_mt_text_tbl,

            #
            shiny::sliderInput(
              inputId = "values_text",
              label = "Select level of matrix truncation:",
              min = 2,
              max = 15,
              value = 2,
              step = 1
            )
          )
        ),




        shiny::fluidRow(
          column(6, boxPlus(
            title = "Term Document Matrix (TDM)",
            width = 12,
            closable = TRUE,
            enable_label = TRUE,
            label_text = 2,
            label_status = "info",
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            p("We convert the documents into the blue Term Document Matrix (TDM) above. The TDM simply counts
             the number of times each word appears in each document. We say term instead of word as a matrix
             could also be created for every word pair (called bigrams), or any number of word sequences (known as ngrams). 'Stop' words that occur in most 
              documents are excluded before the TDM is created (e.g. 'and' or 'the') as they offer little discriminantory 
              value for tasks such as text search or document clustering.")
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
            p("SVD has decomposed the blue TDM into the green U, D and V matrices below.
           The de-composed matrices are truncated by the slider before being multiplied together to create the orange matrix above.
           Notice how some words (or terms) that don't appear in documents do have a value after the truncated SVD is applied. 
           For example, in the 'Memos' documents, look at the value for the word 'trees' in document m4 in the blue TDM. When the value
             2 is chosen in the slider, 'trees' is given the value 0.66 in the orange matrix above from the truncated SVD. 
             But when you look at 'trees' in the original blue TDM, the value is zero. So SVD has made the term 'trees' 
             available for text search or 
             text classification, even though 'trees' does not appear in document m4. However, note that 'trees' does  
             appear in similar documents that share other words (i.e. documents m1, m2 and m3).")
          ))
        )
      ),

      tags$h3("2 The three matrices from SVD that when truncated and multiplied perform LSA"),

      shiny::wellPanel(
        shiny::fluidRow(
          column(4, table_u_text),
          column(4, table_d_text),
          column(4, table_v_text)
        ),

        shiny::br(),


        shiny::fluidRow(
          column(12, boxPlus(
            title = "TDM decomposed by SVD into U, D, and V matrices and truncated by slider",
            width = 12,
            closable = TRUE,
            enable_label = TRUE,
            label_text = 4,
            label_status = "info",
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            p("These are the green U, D and V matrices created from the Singular Value Decomposition of the blue TDM.
         Each matrix is truncated by the number selected in the slider. 
         The U matrix (of left singular vectors) has the right most columns truncated.
         The V matrix (of right singular vectors) has the bottom most rows truncated.
         The square D matrix (diagonal of singular values) has the right most columns truncated and remains 
         a square as the rows with only zeros in are also removed. 
         Try setting the slider
      to the maximum number of values. When the full non-truncated matrices are
        multiplied, the matrix values in orange above are identical to the original blue TDM. This demonstrates that the U, D and V
        matrices contain all the information in the original TDM, just like in the image SVD example.")
          ))
        )
      ),

      tags$h3("3 SVD is also underlies Principal Components Analysis"),

      shiny::wellPanel(
        shiny::fluidRow(
          column(
            6,
            table_tdm_scale
          ),

          column(
            6, boxPlus(
              title = "Using SVD to peform PCA",
              width = 12,
              closable = TRUE,
              enable_label = TRUE,
              label_text = 5,
              label_status = "info",
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              p("SVD is the method underlying Principal Components Analysis.
            A common first step in PCA is to centre and scale the matrix. 
            It is typically carried out if the columns contain 
            units on different scales and we don't want some columns to dominate others in the analysis. 
            Use the options below to run PCA with or without this adjustment  and see the impact.
            We use base R function base::scale(). For each term count in each column, the column mean is subtracted then divided by the column standard deviation.
              Each column will now have a mean of zero and a standard deviation of one.")
            ),

            shiny::radioButtons("radio",
              label = "Centre and Scale Term Document Matrix?",
              choices = list("Centre & scale" = 1, "Do not centre or scale" = 2),
              inline = TRUE,
              selected = 1
            )
          )
        )
      ),

      tags$h3("4 Create both a PCA Scree plot and PCA eigenvalues from the SVD Diagonal matrix of singular values"),

      shiny::wellPanel(
        shiny::fluidRow(
          column(6, table_d_text_scaled),
          column(6, var_expl_text)
        ),

        shiny::br(),

        shiny::fluidRow(
          column(12, boxPlus(
            title = "Variance explained",
            width = 12,
            closable = TRUE,
            enable_label = TRUE,
            label_text = 6,
            label_status = "info",
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            p("After applying the R function base::svd() to the TDM, the resulting diagonal matrix
              of singular values can describe the variance explained. 
              The values in the Scree plot 
              are each singular value in the diagonal matrix from left to right, squared, 
              and divided by the sum total of all the squared values
              in the diagonal matrix.")
          ))
        ),

        shiny::fluidRow(
          column(6, table_SVD_eigen),



          column(6, boxPlus(
            title = "Eigenvalues from SVD",
            width = 12,
            closable = TRUE,
            enable_label = TRUE,
            label_text = 7,
            label_status = "info",
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            p("We can also convert the singular values from the diagonal matrix of the SVD into the
          eigenvalues reported in PCA. Eigenvalues measure the amount of variation 
            retained by each principal component. The first Principal Component
            finds the direction with the maximum amount of variation in the data set.
            So in the language of a truncated SVD, the eigenvalues can be used to show what number of 
            singular vectors can be kept to explain most of the variance in the matrix.
            
            When using the R function stats::prcomp() for PCA, the eigenvalues are the standard deviations it returns squared.
            To convert the D diagonal matrix singular values from SVD to PCA eigenvalues, square each value and divide
            by the sample variance (the number of terms minus one).
            ")
          ))
        )
      ),


      tags$h3("5 Visualise PCA Eigenvectors using the SVD V matrix"),

      shiny::wellPanel(
        shiny::fluidRow(
          column(6, table_v_text_scaled),
          column(6, SVD_PCAloadings_plt)
        ),

        shiny::br(),

        shiny::fluidRow(
          column(12, boxPlus(
            title = "PCA Eigenvectors",
            width = 12,
            closable = TRUE,
            enable_label = TRUE,
            label_text = 8,
            label_status = "info",
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            p("The PCA Eigenvectors are taken from the V matrix. We can plot the first 
            two columns of the V matrix on the x-axis and y-axis respectively. 
            The arrows point in the direction of increasing values for each original document. 
            Arrows that are close show that two documents are highly correlated. 
            Negatively correlated documents are on opposite sides of the plot.
            For the 'Memos' text, note how effective these two columns
            are at clustering documents with the same words in this 2-dimensional space. For example,
            documents C2 and C5 are the only documents that include the words 'response' and 'time' and so 
            are close to each other in the plot.")
          ))
        )
      ),

      tags$h3("6 Create PCA Scores from SVD matrices U x D"),

      shiny::wellPanel(
        shiny::fluidRow(
          column(3, table_u_text_scaled),
          column(3, table_SVD_PCAscores),
          column(6, SVD_PCAscores_plt),

          shiny::br(),
          shiny::br(),


          shiny::fluidRow(
            column(12, boxPlus(
              title = "PCA Scores",
              width = 12,
              closable = TRUE,
              enable_label = TRUE,
              label_text = 9,
              label_status = "info",
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              p("PCA scores come from the multiplication of the U and D matrices. A typical PCA plot is the first of the values
            in the first two columns. These are the first two principal components. So this is the values of the
            original TDM but re-oriented or rotated onto our two new axes.")
            ))
          )
        )
      ),


      tags$h3("7 Create the identical PCA output to SVD using the R PCA function stats::prcomp()"),

      shiny::wellPanel(
        shiny::fluidRow(
          column(3, table_PCAscores),
          column(3, table_PCAloadings),
          column(6, factoextra_biplot_plt)
        ),


        shiny::fluidRow(
          column(6, factoextra_scree_plt),

          column(6, boxPlus(
            title = "PCA using stats::prcomp",
            width = 12,
            closable = TRUE,
            enable_label = TRUE,
            label_text = 10,
            label_status = "info",
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            p("To demonstrate that we have entirely re-created a Principal Components Analysis
            with SVD, here we simply use the R function stats::prcomp() on the TDM.
            From the list of objects prcomp creates we can view here both the PCA scores and 
            PCA loadings in tables. And use the excellent factoextra::fviz_pca_biplot function to create
            the combined plot of both the PCA scores and loadings, factoextra::fviz_eig() to create
            the scree plot, and squaring sdev to create the eigenvalues (or use the function 
            factoextra::get_eigenvalue()).
            ")
          ))
        ),

        shiny::fluidRow(
          column(6, table_PCAeigen)
        )
      )
    )

    return(ui)
  })
}

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(umap)
library(metap)
library(packcircles)
library(RMySQL)

#connect to SQL database, unfortunately you will need to hard code your login details
DCDM_project = dbConnect(RMySQL::MySQL(), dbname = 'DCDM_project', username = 'your_username', password = 'your_password')

# Load the data
df = dbGetQuery(DCDM_project, "SELECT Analysis.analysis_id,Analysis.pvalue,Genes.gene_accession_id,Genes.gene_symbol,Parameter.parameter_name,Parameter.parameter_group,Parameter.parameter_id,impcParameterOrigId FROM Analysis INNER JOIN Genes ON Genes.gene_accession_id=Analysis.gene_accession_id INNER JOIN Parameter ON Parameter.parameter_id=Analysis.parameter_id") 
#rename the parameter_group column to group so it works with rest of the code

# Create combined variables for gene and parameter, containing both IDs and names/symbols
# These are used when selecting genes and parameters
df = df%>%rename_with(~c('group'),c(parameter_group))
df$gene_combined = paste(df$gene_symbol, ', ', df$gene_accession_id, sep = '')
df$parameter_combined = paste(df$parameter_name, ', ', df$parameter_id, sep = '')

# Define function for combining p-values using Fisher's method

combine_pvalue = function(pvals) {
  # Replace zero p-values with a small value to avoid log(0)
  
  pvals[pvals == 0] = 1e-10
  
  # If there are fewer than two valid p-values, return the first p-value as is
  
  if (length(pvals) < 2) {
    return(pvals[1]) # Return the first valid p-value
  } else {
    combined_pvalue = sumlog(pvals)$p
    return(combined_pvalue) # Otherwise, apply Fisher's method using sumlog
  }
}

#UI setup
ui = dashboardPage(
  dashboardHeader(title = "IMPC Dashboard Group 1"),
  dashboardSidebar(
    #Create a sidebar where the user can hop between the three Figures
    sidebarMenu(
      id = "sidebarItemExpanded",
      menuItem("Figure 1: Bubble Chart", tabName = "figure1", icon = icon("circle")),
      menuItem("Figure 2: Scatter Plot", tabName = "figure2", icon = icon("line-chart")),
      menuItem("Figure 3: Clustering Plot", tabName = "figure3", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems( #For each figure, create a layout with the plot and the customizable inputs
      tabItem(tabName = "figure1", 
              fluidRow(
                column(
                  width = 4,
                  selectInput("gene_id", "Select the Knockout Mouse by Gene", choices = unique(df$gene_combined))
                ),
                column(
                  width = 4,
                  sliderInput("p_value_threshold1", "p-value Threshold", min = 0, max = 1, value = 0.05, step = 0.01)
                ),
                column(
                  width = 4,
                  radioButtons("sig_visualise", "Significance Visualisation", choices = 
                    c('Colour','Border'))
                )
              ),
              fluidRow(
                box(title = "Bubble Chart", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("bubble_chart"))
              )
      ),
      tabItem(tabName = "figure2",
              fluidRow(
                column(
                  width = 6,
                  selectInput("parameter_id", "Select Parameter", choices = unique(df$parameter_combined))
                ),
                column(
                  width = 6,
                  sliderInput("p_value_threshold2", "p-value Threshold", min = 0, max = 1, value = 0.05, step = 0.01)
                )
              ),
              fluidRow(
                box(title = "Scatter Plot", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("scatter_plot"))
              )
      ),
      tabItem(tabName = "figure3",
              fluidRow(
                column(
                  width = 3,
                  radioButtons("selected_grouping", "Group values by ", choiceNames = c("Group", "Parameter"),
                               choiceValues = c("group", "parameter_id"))
                ),
                column(
                  width = 3,
                  radioButtons("color_by", "Colour values by", choiceNames = c('Most significant parameter group', 'K-means clusters'), 
                               choiceValues = c("Min_Column", "cluster"))
                ),
                column(
                  width = 3,
                  sliderInput("p_value_threshold3", "p-value Threshold", min = 0, max = 1, value = 0.05, step = 0.01)
                ),
                column(
                  width = 3,
                  sliderInput("k_number", "K-number (number of clusters)", min = 1, max = 10, value = 6, step = 1)
                )
              ),
              fluidRow(
                box(title = "Clustering Plot", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("clustering_plot"))
              )
      )
    )
  )
)

#Backend server setup
server = function(input, output, session) {
  
  # There are multiple p-values per gene and parameter, these must be combined to one value each
  # This is done using the Fisher's Method function defined earlier, leaving out unused columns
  df_grouped = df %>%
    group_by(parameter_id, gene_combined, group, parameter_name, parameter_combined, gene_accession_id) %>%
    summarise(
      p_value = combine_pvalue(pvalue),
      across(everything(), ~first(.x), .names = "{.col}"),
      .groups = 'drop'
    ) %>%
    select(-pvalue)
  
  # Defining the set of colors used in figures 1 and 3, and assigning names based on group (fig1+3)
  # and k-means numbers (fig3)
  myColors = 
    c('#ff1493','#dda0dd','#f0e68c','#ff00ff','#00bfff',
      '#00ffff','#e9967a','#4169e1','#00fa9a','#7fff00',
      '#0000cd','#ffff00','#ffa500','#ff0000','#800080',
      '#8b0000','#2e8b57','#696969','gray86','#006400',
      '#00008b','#b03060','#ff4500','#ffff00','#deb887',
      '#00ff00','#00ffff','#ff00ff','#6495ed')
  names(myColors) = c('Abnormal_behaviour', 'Activity', 'Bone', 'LDT', 'Brain', 'Eye', 
                       'Coat', 'Face', 'Embryo','Image', 'Hearing', 'Chemical', 'Limb', 
                       'Blood', 'PPI', 'Heart', 'Weight', 'Other','Non-significant','1',
                       '2','3','4','5','6','7','8','9','10')
  
  # Figure 1 Data prep
  # Gene ID, parameter combined, and gene symbol are dropped from the data frame as they are not
  # used in this plot.
  data_fig1 = df_grouped %>% select(-c(gene_accession_id, parameter_combined, gene_symbol))
  
  # P-values are transformed for plotting, by taking the inverse (so that smaller p-values
  # will be bigger on the plot) and power transforming (to emphasise the extreme/significant
  # values)
  data_fig1$ptrans = (1-data_fig1$p_value)^8
  
  
  # Figure 2 Data prep
  # Gene combined is dropped as it is not used in figure 2
  data_fig2 = df_grouped %>% select(-c(gene_combined))
  
  # Reactive expression for Figure 1 (Bubble Chart)
  output$bubble_chart = renderPlotly({
    # Subsetting data based on the input from the selected gene_combined value
    subset_fig1 = subset(data_fig1, gene_combined == input$gene_id)
    
    # Sorting by transformed pvalue, highest to lowest, so that when it's plotted the high values
    # (significant) are plotted in the centre, and the low values on the outside
    subset_fig1 = arrange(subset_fig1, desc(ptrans))
    
    # Creating layout for circle packing based on transformed p-value
    # Progressive layout means plotting is ordered, starting in the centre
    # Radius chosen over area to emphasise more extreme values
    packing = circleProgressiveLayout(subset_fig1$ptrans, sizetype = 'radius')
    
    # Radius reduced by 2% to allow space between circles, for aesthetics
    packing$radius = 0.98 * packing$radius  # Scale radius
    
    # Assign labels for group if p-transformed is above a certain threshold (92nd percentile)
    # These are used for the text label on the circle, plotting on smaller circles is too cluttered
    # Standard label is group, replaced with blank string
    subset_fig1$label = ifelse(subset_fig1$ptrans < quantile(subset_fig1$ptrans, 0.92), '', subset_fig1$group)
    
    # Creating labelling dataset, requires both plotting and group data
    # na.omit used due to very small values introducing NAs in plotting data (removes the row entirely)
    # na.omit used after binding, to ensure that removed rows do not interfere with cbind
    text_data = na.omit(cbind(subset_fig1, packing))
    
    # Creating final plotting dataset using packing data
    # na.omit used again to remove NAs caused by small values, npoints is number of vertices 
    # plotted per circle (i.e. resolution)
    plot_fig1 = circleLayoutVertices(na.omit(packing), npoints = 50)
    
    # id column created in data subset to allow the left join (plotting dataset contains id)
    subset_fig1$id = c(1:length(subset_fig1$gene_combined))
    
    # Joining plotting dataset and gene dataset by id, to allow group, parameter name, and pvalues to
    # be accessed and indexed within the plotting dataset
    plot_fig1 = left_join(plot_fig1, subset_fig1, by = "id")
    
    # Setting default border type at 0.05, this will passed into linewidth in the plot
    # If border option is selected, this is changed to 1 for significant values
    plot_fig1$border_type = 0.05
    
    # Group colour column created to as colour grouping for plot (by parameter group)
    # Separating this from parameter group allows non-significant values to be greyed-out
    # while still allowing the correct group to be shown on hover info
    plot_fig1$group_colour = plot_fig1$group
    
    # Applying optional significance visualise settings
    if (input$sig_visualise=='Colour'){ # By colour, change non-significant values group colour (light grey)
    plot_fig1$group_colour = ifelse(plot_fig1$p_value < input$p_value_threshold1, plot_fig1$group, 'Non-significant') 
    } else { # By border, change significant values border type (line weight) to 1
    plot_fig1$border_type = ifelse(plot_fig1$p_value < input$p_value_threshold1, 1, 0.05)
    }
    
    # Create ggplo2 object for circle-packing data, using geom_polygon
    ggplot_obj = ggplot() + 
      geom_polygon(data = plot_fig1, aes(x, y, group = id, fill=group_colour,
      # group = id refers to the plotting points for the circle vertices grouped by each individual data point
      # fill = group_colour refers to the colour filling being split by parameter group
        text = paste("Paramter name:", parameter_name,"<br>Parameter group:", group,
                     "<br>Parameter ID:", parameter_id, "<br>p-value:", signif(p_value,3))),
      # text refers the hover-info text for the circles, showing parameter name, group, ID, and original p-value
      linewidth = plot_fig1$border_type,
      # linewidth set to border type variable, default is 0.05 (thin), with sig. values set to 1 (thicker)
      colour = 'black', alpha = 1) + # colour is for border, alpha is hue brightness (1 = max)
      scale_fill_manual(name = 'Parameter Group', values = myColors) + # filling points by group, first 19 color values
      # Name = Parameter Group for title of legend
      scale_size_continuous(range = c(1,4)) + 
      # continuous scaling ensures font and plot sizes are adjusted post-plotting based on size
      geom_text(data = text_data, aes(x, y, size=ptrans, label = label)) +
      # labelling the points based on label value (group for top 8%, blank for rest),
      # size of text scaled by the pvalue
      theme_void() +
      guides(size = guide_legend(title = "")) + # size title removed from legend, leaving just parameter group/colour
      coord_equal()
    
    # Convert ggplot to plotly for final plotting
    ggplotly(ggplot_obj,tooltip="text") %>% # tooltip text calls the hoverinfo defined in text earlier
      style(hoverinfo = "skip", traces = c((dim(text_data)[1]+1):(2*dim(text_data)[1])))
    # Skipping hover-info for all bubble label text, only bubble fill showing relevant info
    # A hover-info 'trace' is created for each circle fill and each text label (including blanks)
    # This skips the second half of these traces, the ones assigned to the text
    # Uses dimensions of the labeling data for number of traces, as there may be values removed
  })
  
  # The rest of your server code for Figures 2 and 3 remains unchanged
  
  # Reactive expression for Figure 2 (Scatter Plot)
  output$scatter_plot = renderPlotly({
    # Filter the data for the selected parameter and adjust p-values
    plot_data = data_fig2 %>%
        filter(parameter_combined == input$parameter_id) %>%
        mutate(
          p_value_adjusted = ifelse(p_value < 0.01, 0.01, p_value),
          significance = ifelse(p_value_adjusted < input$p_value_threshold2, "Significant", "Not Significant"),
          neg_log10_pvalue = -log10(p_value_adjusted))
    # Create the output plot 
    plot_ly(data = plot_data, 
            x = ~seq_along(gene_accession_id), #Gene IDs on the x-axis
            y = ~neg_log10_pvalue, #-log10(p_value)
            type = 'scatter', 
            mode = 'markers+lines',
            marker = list( #Define how the markers look
              size = 8, 
              color = ~p_value,  
              colorscale = 'plasma',
              colorbar = list(title = "p-value"), #colour based on p_value
              line = list(color = ~ifelse(significance == "Significant", "black", "white"), width = 2) # create an outline for the genes below the significance threshold
            ),
            line = list(color = "gray", width = 1), 
            hoverinfo = 'text', #Define the text that pops up when the user hovers over a data point
            text = ~paste("Gene: ", gene_symbol, 
                          "<br>p-value: ", signif(p_value, 3),
                          "<br>-log10(p-value): ", signif(neg_log10_pvalue, 3),
                          "<br>Significance: ", significance)) %>%
      layout( #Title and axis labels
        title = "Gene Knockout Effects on Parameter by p-value", 
        xaxis = list(title = "Genes", showticklabels = FALSE),
        yaxis = list(title = "-log10(p-value)"),
        shapes = list(
          list(type = "line", y0 = -log10(input$p_value_threshold2), y1 = -log10(input$p_value_threshold2),
               x0 = 0, x1 = length(plot_data$gene_accession_id),
               line = list(color = "green", dash = "dash", width = 2) #Create a line to show the p_value threshold
          )
        ),
        annotations = list(
          list(x = length(plot_data$gene_accession_id), y = -log10(input$p_value_threshold2),
               xref = "x", yref = "y", text = paste("p-value =", input$p_value_threshold2),
               showarrow = TRUE, arrowhead = 7, ax = 20, ay = 40) #Add an annotation to write out the selected p_value threshold
        )
      )
  })
  
  # Creating widget updates for fig.3 options, so you can't select parameter for grouping and
  # group for colour-by at the same time
  observe({
    if (input$selected_grouping == 'parameter_id'){
      updateRadioButtons(session, 'color_by', choiceNames = c('K-means clusters'), 
                          choiceValues = c("cluster"))} # Removes choice of group for colour-by
    if (input$selected_grouping == 'group'){
      updateRadioButtons(session, 'color_by', choiceNames = c('Most significant parameter group', 'K-means clusters'), 
                         choiceValues = c("Min_Column", "cluster"))} # Resets choices back to normal
  })
  
  # Reactive expression for Figure 3 (Clustering Plot)
  output$clustering_plot = renderPlotly({
    
    # Grouping data using Fisher's Method, retaining only gene_combined information and
    # chosen grouping input (parameter or parameter group)
    data_fig3 = df %>%
      group_by(across(all_of(c(input$selected_grouping, "gene_combined")))) %>%
      summarise(p_value = combine_pvalue(pvalue),across(everything(), ~first(.x), 
                  .names = "{.col}"), .groups = 'drop') %>%
      select(-pvalue)
    
    # Mutating the results to convert any non-signifcant p-values to 1, therefore only
    # clustering using the significant results (significance threshold taken from input)
    # This emphasises the lower values much more, emphasising significance and reducing noise
    data_fig3 = mutate(data_fig3, p_value = if_else(p_value > input$p_value_threshold3, 1.00, p_value))
    
    # Pivoting the data to a matrix-like dataframe of p-values
    mat_fig3 = data_fig3 %>%
      select(gene_combined, !!sym(input$selected_grouping), p_value) %>%
      pivot_wider(names_from = !!sym(input$selected_grouping), values_from = p_value) %>%
      as.data.frame() # re-converted to data frame as pivot_wider outputs tibble
    
    # Setting the row names as the gene info and removing it from the dataset
    row.names(mat_fig3) = mat_fig3$gene_combined
    mat_fig3 = mat_fig3[-1]
    
    # Perform UMAP calculations
    umap_res = umap(mat_fig3, random_state = 123)
    
    # Perform k-means clustering on matrix data using k-number input, for colouring points
    kmeans_results = kmeans(mat_fig3, centers = input$k_number, nstart = 25)
    
    # Finding the most significant group for each value, for colouring points
    # This is just group with the lowest (most significant) pvalue for each gene
    mat_fig3$Min_Column = apply(mat_fig3, 1, function(row) names(mat_fig3)[which.min(row)])
    # If all values are non-significant (i.e. 1), sets to non-significant instead
    mat_fig3 = mat_fig3 %>% mutate( Min_Column = if_else(apply(select(., -Min_Column), 1, min) == 1,
                                                         "Non-significant", Min_Column))
  
    # Create the final plotting data frame, taking the UMAP x and y values for point locations
    # Includes gene_names for hoverinfo, and clusters and min_column for colouring by
    umap_plot = data.frame(
      x = umap_res$layout[,1],
      y = umap_res$layout[,2],
      gene_name = row.names(mat_fig3),
      cluster = as.factor(kmeans_results$cluster),
      Min_Column = mat_fig3$Min_Column)
    
    # Creating label variables for hover-info, based on colour-by selection
    if (input$color_by == 'cluster'){
      label1 = '<br>Cluster:'
      label2 = 'cluster'
    } else {
      label1 = '<br>Group:'
      label2 = 'Min_Column'
    }
    
    # Plotting using plotly
    plot_ly(umap_plot, x = ~x, y = ~y, color = ~.data[[input$color_by]],
            # deciding the coloring by using input from colour-by
            colors = myColors, type = "scatter", mode = "markers",
                text = ~paste("Gene:", gene_name, label1, get(label2)),
                # text argument is for hover-info, showing gene name and colour-by info
                hoverinfo = "text"
                ) %>%
      layout(
        title = "UMAP Plot",
        xaxis = list(title = "UMAP1"),
        yaxis = list(title = "UMAP2"))
  })
}


# Run the application
shinyApp(ui = ui, server = server)


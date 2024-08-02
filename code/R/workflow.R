# install.packages("DiagrammeR")
library(DiagrammeR)
library(rsvg)
library(DiagrammeRsvg)
library(magrittr)




grViz("
digraph boxes_and_circles {

  # a 'graph' statement
  graph [overlap = true, fontsize = 12]

  # several 'node' statements
  node [shape = hexagon, height = 1.2,
        fontname = Helvetica]
  A [label = 'Tishreen Dataset']; B [label = 'Google Earth Engine/RGEE']

  node [shape = parallelogram, height = 0.8, 
        fontname = Helvetica]
  C [label = 'Time-Series NDWI (2000-2023)'] D [label = 'Archaeological Data: Polygons']

  node [shape = diamond, height = 1.5]  
  E [label = 'Treshold = 0'];

  node [shape = box, height = 0.8]
  F [label = 'Reclassification']; 
  G [label = 'Zonal Histogram'];
  node [shape = rectangle, height = 0.8, style = rounded]  
  H [label = 'Emerged Extent']; 

  # several 'edge' statements
  A->D B->C C->E E->F F->G
  G->H D->G
}
")


a <- "
digraph boxes_and_circles {

  # a 'graph' statement
  graph [overlap = false, fontsize = 26.0, rankdir = TB, concentrate=FALSE, center=TRUE]

  # several 'node' statements
  node [shape = hexagon, height = 1]
  F [label = 'Google Earth Engine/RGEE'];
  G [label = 'Tishreen Dataset']
  
  node [shape = parallelogram, height = 0.8, 
        fontname = Helvetica]
  A [label = 'Archaeologica Data: Polygons']; 
  B [label = 'Time-Series NDWI (2000-2023)']; 
node [shape = diamond, height = 1.5]  
  C [label = 'Treshold = 0.05'];

  node [shape = box, height = 0.8]
  1 [label = 'Reclassification']; 
  2 [label = 'Zonal Histogram'];
  node [shape = rectangle, height = 0.8, style = rounded]  
  3 [label = 'Emerged Extent']; 


  # several 'edge' statements
  
  
A->2 B->C 2->3 C->1 1->2 F->B G->A [weight=12]
}"


grViz(a)
grViz(a) %>%
  export_svg %>% charToRaw %>% rsvg_png("workflow_LR.png")


export_graph(a,
             file_name = "workflow.png",
             file_type = "png")
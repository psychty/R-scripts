
#install.packages('DiagrammeR')

library(DiagrammeR)

data_1 <- data.frame(Outcome = c("Died", "Did not die", "Fell off", "Survived"), Number = c(41,80,91,102))

label_1 <- paste0("Chocoloca - ", format(1000, big.mark = ","))

grViz("digraph {
      
      graph[layout = dot]
      
      # default style (can be overridden by individual cells later)
      node [shape = rectangle, fontsize = 8, fontname = Verdana]
      
      # Create your labels
      node1 [label = '@@1'] 
      node2 [label = '@@2']
      process [label =  'Process \n Data']
      statistical [label = 'Statistical \n Analysis']
      results [label= 'Results']
      
      # edge definitions with the node IDs
      # Using {} shows two origins going into process
      {node1 node2}  -> process -> statistical -> results
      node2 -> results
      
      }
    [1]: paste0(data_1$Outcome[1], ' - ', data_1$Number[1])
    [2]: paste0(data_1$Outcome[2], ' - ', data_1$Number[2])
    ")



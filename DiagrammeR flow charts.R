
#install.packages('DiagrammeR')
library(readr)
library(DiagrammeR)
library(tidyverse)

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

Flow_df <- read_csv("C:/Users/Rich/Downloads/Flow_df.csv") %>% 
  mutate(Flow_label = gsub("Didn't", "Did not", Flow_label)) %>% 
  filter(!(Node %in% c(18, 21, 25, 27))) %>% 
  mutate(Node = row_number())

grViz("digraph {
      
 graph[layout = dot]
      
 # default style (can be overridden by individual cells later)
node [shape = rectangle, fontsize = 8, fontname = Verdana]
 
1 [label = '@@1']
2 [label = '@@2']
3 [label = '@@3']
4 [label = '@@4']
5 [label = '@@5']
6 [label = '@@6']
7 [label = '@@7']
8 [label = '@@8']
9 [label = '@@9']
10 [label = '@@10']
11 [label = '@@11']
12 [label = '@@12']
13 [label = '@@13']
14 [label = '@@14']
15 [label = '@@15']
16 [label = '@@16']
17 [label = '@@17']
18 [label = '@@18']
19 [label = '@@19']
20 [label = '@@20']
21 [label = '@@21']
22 [label = '@@22']
23 [label = '@@23']

1 -> {2 3}
2 -> {4 5 6 7}
3 -> {8 9 10 11}
4 -> {12 13 14 15 16 17}
6 -> {18 19}
8 -> {20 21 22}
10 -> 23
    }
[1]: paste0(Flow_df$Flow_label[1])
[2]: paste0(Flow_df$Flow_label[2])
[3]: paste0(Flow_df$Flow_label[3])
[4]: paste0(Flow_df$Flow_label[4])
[5]: paste0(Flow_df$Flow_label[5])
[6]: paste0(Flow_df$Flow_label[6])
[7]: paste0(Flow_df$Flow_label[7])
[8]: paste0(Flow_df$Flow_label[8])
[9]: paste0(Flow_df$Flow_label[9])
[10]: paste0(Flow_df$Flow_label[10])
[11]: paste0(Flow_df$Flow_label[11])
[12]: paste0(Flow_df$Flow_label[12])
[13]: paste0(Flow_df$Flow_label[13])
[14]: paste0(Flow_df$Flow_label[14])
[15]: paste0(Flow_df$Flow_label[15])
[16]: paste0(Flow_df$Flow_label[16])
[17]: paste0(Flow_df$Flow_label[17])
[18]: paste0(Flow_df$Flow_label[18])
[19]: paste0(Flow_df$Flow_label[19])
[20]: paste0(Flow_df$Flow_label[20])
[21]: paste0(Flow_df$Flow_label[21])
[22]: paste0(Flow_df$Flow_label[22])
[23]: paste0(Flow_df$Flow_label[23])
      ")

# perhaps put the closed and finished earlier than expected into a subclass

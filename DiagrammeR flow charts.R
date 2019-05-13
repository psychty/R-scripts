
#install.packages('DiagrammeR')
library(readr)
library(DiagrammeR)
library(tidyverse)

Flow_df <- read_csv("C:/Users/Rich/Downloads/Flow_df.csv") %>% 
  mutate(Flow_label = gsub("Didn't", "Did not", Flow_label)) %>% 
  filter(!(Node %in% c(18, 21, 25, 27))) %>% 
  mutate(Node = row_number()) %>% 
  mutate(Flow_label = ifelse(Node_level == 4, gsub("\r\n", " ", Flow_label), Flow_label)) %>% 
  mutate(Flow_label = gsub("   ", " ", Flow_label))

Flow_df$Flow_label

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
12 [label = '@@12', style=filled, fillcolor = '#e7e7e7', height = 0.25, width = 1.8]
13 [label = '@@13', style=filled, fillcolor = '#e7e7e7', height = 0.25, width = 1.8]
14 [label = '@@14', style=filled, fillcolor = '#e7e7e7', height = 0.25, width = 1.8]
15 [label = '@@15', style=filled, fillcolor = '#e7e7e7', height = 0.25, width = 1.8]
16 [label = '@@16', style=filled, fillcolor = '#e7e7e7', height = 0.25, width = 1.8]
17 [label = '@@17', style=filled, fillcolor = '#e7e7e7', height = 0.25, width = 1.8]
18 [label = '@@18', style=filled, fillcolor = '#91cf60', height = 0.25, width = 1.8]
19 [label = '@@19', style=filled, fillcolor = '#91cf60', height = 0.25, width = 2.4]
20 [label = '@@20', style=filled, fillcolor = '#e7e7e7', height = 0.25, width = 1.8]
21 [label = '@@21', style=filled, fillcolor = '#e7e7e7', height = 0.25, width = 1.8]
22 [label = '@@22', style=filled, fillcolor = '#e7e7e7', height = 0.25, width = 1.8]
23 [label = '@@23', style=filled, fillcolor = '#91cf60', height = 0.25, width = 2.4]

1 -> {2 3}
2 -> {4 5 6 7}
3 -> {8 9 10 11}
4 -> 12

subgraph cluster_early_stillopen {
graph [ranksep = 0.1, style = dotted]
12 -> 13 -> 14 -> 15 -> 16 -> 17 [style = invis, length = 0.5]
}

6 -> 18
subgraph cluster_resolved_stillopen {
graph [ranksep = 0.1, style = dotted]
18 -> 19 [style = invis]
}

8 -> 20
subgraph cluster_early_closed {
graph [ranksep = 0.1, style = dotted]
20 -> 21 -> 22 [style = invis]
}

10 -> 23
subgraph cluster_resolved_closed{
graph [ranksep = 0.1, style = dotted]
23
}

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

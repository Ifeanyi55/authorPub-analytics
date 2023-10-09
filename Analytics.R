library(tidyverse)
library(highcharter)
library(openalexR)

# set options
# options(openalexR.mailto = "youremail@email.com")
# 
# 
# # fetch data from open alex api
# works_search <- oa_fetch(
#   entity = "works",
#   title.search = c("simulation"),
#   cited_by_count = ">50",
#   from_publication_date = "2020-01-01",
#   to_publication_date = "2023-08-31",
#   options = list(sort = "cited_by_count:desc"),
#   verbose = TRUE
# )
# 
# write.csv(works_search,file = "authorsPublications.csv")


# load authorPublications.csv
data <- read.csv("authorsPublications.csv")
view(data)

# publication with the most collaborators
pubs_with_most_collabs <- data |> 
  group_by(Publication) |> 
  summarize(num_of_collabs = n()) |> 
  arrange(desc(num_of_collabs))

pubs_with_most_collabs |> view()

# top 20 pie
pie_hc <- pubs_with_most_collabs[1:20,] |> 
  hchart(
    type = "pie",
    hcaes(x = Publication,y = num_of_collabs),
    name = "Number of Authors",
    shadow = T) |> 
  hc_title(text = "<b>Top 20 Publications With The Most Authors</b>")

pie_hc

# gauge plot of the highest number of authors
gauge <- highchart() |> 
  hc_pane(startAngle = -90,
          endAngle = 90,
          background = list(
            shape = "arc",
            innerRadius = "60%",
            outerRadius = "100%",
            borderWidth = 5
            )) |> 
  hc_yAxis(
    labels = list(style = list(fontSize = "15px",color = "black"))
  ) |> 
  hc_colors(colors = "green") |> 
  hc_add_series(data = 54, 
                type = "solidgauge",
                name = "Number of Authors",
                dataLabels = list(
                  useHTML = T,
                  borderWidth = 0,
                  borderColor = "black",
                  style = list(fontSize = "20px",color = "green")
                )) |> 
  hc_tooltip(enabled = T) |> 
  hc_title(text = "<b>Highest Number of Authors Collaborating on One Publication</b>")

gauge


# affiliations with most authors
aff_with_most_authors <- data |> 
  group_by(Affiliation) |> 
  summarize(num_of_auhtors = n()) |> 
  arrange(desc(num_of_auhtors))

aff_with_most_authors

# top 20 bar
hc <- aff_with_most_authors[1:20,] |> 
  hchart(type = "bar",
         hcaes(x = Affiliation, 
               y = num_of_auhtors),
         name = "Number of Authors",
         color = "#0174c3",
         shadow = T) |> 
  hc_title(text = "<b>Top 20 Affiliations With The Most Authors</b>")
  
hc

# institutions with most authors
inst_with_most_authors <- data |> 
  group_by(Institution) |> 
  summarize(num_of_authors = n()) |> 
  arrange(desc(num_of_authors))

inst_with_most_authors |> view()


  
pie_hc2 <-  inst_with_most_authors[1:20,] |>
  hchart(
    hcaes(x = Institution, y = num_of_authors),
    type = "pie",
    name = "Number of Authors",
    shadow = T) |>     
  hc_title(text = "<b>Top 20 Institution Categories With The Most Authors</b>")

pie_hc2 


# top 20 authors involved in most collaboration
author_most_appearing <- data |> 
  group_by(Authors) |> 
  summarize(Frequency = n()) |> 
  arrange(desc(Frequency))

author_most_appearing |> view()  

hc_bar <- author_most_appearing[1:20,] |> 
  hchart(
    type = "bar",
    hcaes(x = Authors,
          y = Frequency),
    shadow = T,
    name =  "Number of Collaborations",
    color = "#ff3b01"
  ) |> 
  hc_title(text = "<b>Top 20 Authors Involved in Most Collaborations</b>")

hc_bar

# most author positions
most_author_position <- data |> 
  group_by(Position) |> 
  summarize(Count = n()) |> 
  arrange(desc(Count))

most_author_position |> view()

most_pie <- most_author_position |> 
  hchart(
    hcaes(x = Position, y = Count),
    type = "pie",
    name = "Position Count",
    shadow = T) |>
  hc_colors(c("#2874A6","#3498DB","#5DADE2")) |> 
  hc_title(text = "<b>Authors Positions Count</b>")

most_pie

  
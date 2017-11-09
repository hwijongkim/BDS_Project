library(shiny)
library(leaflet)
# ui코드는 User에게 보여지는 지도 상의 모든 interface들을 관리합니다.

# UI 상에서 우측에 보여지는 legend의 다양한 선택 사항들을 의미합니다.
vars1 <- c(
  "전체" = "All",
  "가평" = "Gapyeong",
  "김포" = "Gimpo",
  "고양" = "Goyang",
  "광주" = "Gwangju",
  "남양주" = "Namyangju"
)

vars2 <- c(
  "Site Area" = "radius",
  "Avg. price" = "avg_price_size"
)

vars3 <- c(
  "All Types" = "",
  "Cemetery" = "cemetery",
  "Dae" = "dae",
  "Dap" = "dap",
  "Factory" = "factory",
  "Fishery" = "fishery",
  "Forest" = "forest",
  "Googeo" = "googeo",
  "Jeon" = "jeon",
  "Levee" = "levee",
  "Mixed" = "mixed",
  "Oil" = "oil",
  "Orchard" = "orchard",
  "Park" = "park",
  "Parking" = "parking",
  "Pasture" = "pasture",
  "Rail" = "rail",
  "Random" = "random",
  "Religion" = "religion",
  "River" = "river",
  "Road" = "road",
  "School" = "school",
  "Sports" = "sports",
  "Unknown" = "unknown",
  "Water" = "water",
  "Garage" = "garage"
)

vars4 <- c("시군구"="",
           "가평" = "Gapyeong",
           "김포" = "Gimpo",
           "고양" = "Goyang",
           "광주" = "Gwangju",
           "남양주" = "Namyangju")

shinyUI(navbarPage("경기도 공유 재산 지도", id="nav",
                   
                   tabPanel("지도",
                            div(class="outer",
                                
                                tags$head(
                                
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")
                                ),
                                
                                leafletOutput("map", width="100%", height="100%"),
                                
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                              
                                              h2("Public Assets"),
                                              
                                              selectInput("city", "City", vars1, selected = "All"),
                                              selectInput("size", "Size", vars2, selected = "radius"),
                                              checkboxInput("cluster", "Add Cluster"),
                                              helpText("Cluster numbers show total Public assets for each area",
                                                       "",
                                                       "(applies to 'All Types' only)"),
                                              radioButtons("type", "Show Just One Type", vars3, selected = '')
                                )
                                )
                              ),
                   tabPanel("공유 재산 현황표",
                            fluidRow(
                              column(3,
                                     selectInput("cities", "시군구", vars4, multiple=TRUE)
                              ),
                              column(3,
                                     conditionalPanel("input.cities",
                                                      selectInput("assets", "재산 형태", c("공유 재산"=""), multiple=TRUE)
                                     )
                              )
                            ),
                            helpText("지도 상에 표시되는 공유 재산의 데이터를 보려면 액션 버튼을 클릭해야 합니다"),
                            helpText("현제의 탭에서 제대로 된 정보를 보기 위해서는 지도 탭에서 '전체'를 선택해야 합니다"),
                            hr(),
                            DT::dataTableOutput("P.A.table")
                   )
                            ))

library(shiny)
library(leaflet)
library(DT)
# ui코드는 User에게 보여지는 지도 상의 모든 interface들을 관리합니다.


# UI 상에서 우측에 보여지는 legend의 다양한 선택 사항들을 의미합니다.
vars1 <- c(
  "모두" = "All",
  "가평" = "가평",
  "김포" = "김포",
  "고양" = "고양",
  "광주" = "광주",
  "남양주" = "남양주"
)

vars2 <- c(
  "크기" = "radius",
  "면적 당 평균가" = "면적당평균가"
)

vars3 <- c(
  "모두" = "",
  "공원용지"= "공원용지",
  "공장용지"= "공장용지",
  "과수원"= "과수원",
  "구거"= "구거",
  "답"= "답",
  "대"= "대",
  "도로"= "도로",
  "목장용지"= "목장용지",
  "묘지"= "묘지",
  "미등록"= "미등록",
  "수도용지"= "수도용지",
  "양식장"= "양식장",
  "유지"= "유지",
  "임야"= "임야",
  "잡종지"= "잡종지",
  "전"= "전",
  "제방"= "제방",
  "종교용지"= "종교용지",
  "주차장"= "주차장",
  "창고용지"= "창고용지",
  "철도용지"= "철도용지",
  "체육용지"= "체육용지",
  "하천"= "하천",
  "학교용지"= "학교용지"
)
vars4 <- c("시군구"="",
           "가평" = "가평",
           "김포" = "김포",
           "고양" = "고양",
           "광주" = "광주",
           "남양주" = "남양주")

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
                                              
                                              h2("공유 재산"),
                                              
                                              selectInput("도시", "도시", vars1, selected = "All"),
                                              selectInput("size", "크기", vars2, selected = "radius"),
                                              checkboxInput("cluster", "클러스터링"),
                                              helpText("각 지역이 보유하고 있는 모든 공유재산의 수를 클러스터링 합니다",
                                                       "",
                                                       "('모두' 유형에만 적용 가능합니다)"),
                                              radioButtons("type", "유형 선택", vars3, selected = '')
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
                                                      selectInput("assets", "공유재산", c("재산명칭"=""), multiple=TRUE)
                                     )
                              )
                            ),
                            helpText("지도 상에 표시되는 공유 재산의 데이터를 보려면 액션 버튼을 클릭해야 합니다"),
                            helpText("현재의 탭에서 제대로 된 정보를 보기 위해서는 지도 탭에서 '전체'를 선택해야 합니다"),
                            hr(),
                            DT::dataTableOutput("P.A.table")
                   )
))
# biblioteki
library(shiny)
library(ggplot2)
library(dplyr)
library(rgdal)
library(sf)
library(rmapshaper)
library(tmap)
library(RColorBrewer)
library(leaflet)
library(ggridges)
library(viridis)
library(corrplot)
library(lubridate)

# ustawienie języka na polski
Sys.setlocale("LC_ALL", "Polish")
# wgranie danych
load("przejazdy.RData")
load("punkty_pomiarowe.RData")

# dodanie nowych kolumn określających dni tygodnia i miesiące
przejazdy$dni_tygodnia <- as.factor(wday(przejazdy$Data, label = TRUE, abbr = FALSE))
przejazdy$miesiące <- as.factor(format(przejazdy$Data, "%m"))

ui <- fluidPage(

    # Tytuł strony
    titlePanel(h1("Ruch pomiarowy w Gdańsku", align = "center", style="color: #C46A62")),
    # Podtytuł sekcji
    h3("Gdzie znajdują się punkty pomiaru ruchu rowerowego?"),
    fluidRow(
        # Zadanie 8
        column(width = 9, # strona ma maks 12
            leafletOutput(outputId = "mapa")),
        # Krótki opis badanego tematu
        column(width = 3, style = "background-color:#F5F7F5",
            h3("Trochę informacji:"),
            h4("W Gdańsku zainstalowanych zostało 30 punktów automatycznego pomiaru ruchu rowerowego. Zliczenie roweru odbywa się poprzez przejazd nad pętlą indukcyjną, zatopioną w nawierzchni trasy rowerowej. Impulsy przesyłane są do sterownika pętli, a następnie przez sieć GSM do serwerów internetowych. Poniżej przedstawiono podstawowe statystyki 27 z tych punktów."),
        )),
    
    h3("Poniżej zestawione zostały statystyki dla wszystkich punktów przejazdowych."),
    sidebarLayout(
          mainPanel(
            # Pytanie przewodzące dla poniższego wykresu
            h4("Czy we wszystkich punktach w określonym okresie czasu pobrano dane z tej samej ilości dni?"),
            # Zadanie 1
            plotOutput("rozkladDni"),
            h4("Czy natężenie ruchu różniło się między punktami?"),
            # Zadanie 3
            plotOutput("natezenie"),
            plotOutput("rozklad_przejazdow")),
        # Do wybrania zakres dat, interakcja z większością wykresów (oprócz korelogramu)
        sidebarPanel(
            dateRangeInput(inputId = "dateRange1", 
                           label = h3("Zakres dat"),
                           start = "2020-01-01", end = "2020-12-31", min = min(przejazdy$Data),
                           max = max(przejazdy$Data), format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                           language = "en", separator = " to ", width = NULL),
    )),
    
    h3("W tej sekcji można się przyjrzeć statystykom dla konkretnego punktu."),
    sidebarLayout(
        mainPanel(
            h4("Podstawowe statystyki dla wybranego punkty w wybranym okresie czasu."),
            # Dodatkowe statystyki
            tableOutput(outputId = "tabela"),
            h4("Rozkład natężenia przejazdów w wybranym czasie dla wybranego punktu."),
            # Zadanie 2
            plotOutput("l_przejazdow_dla_pkt"),
            plotOutput("natezenie_w_czasie"),
            
        ),
        # Do wybrania punkt pomiarowy, interakcja z większością poniższych wykresów
        sidebarPanel(
            selectInput(inputId = "points",
                        label = h3("Punkty przejazdowe"),
                        choices = punkty$stacja)
        )
    ),
    
    h3("Czy w podziale na miesiące i dni tygodnia można zauważyć jakieś zależności?"),
    sidebarLayout(
        mainPanel(
            h4("Czy we wszystkich punktach w wybranym okresie czasu pobrano dane z tej samej ilości dni?"),
            # Zadanie 4
            plotOutput('agregacja_czasowa'),
            # Zadanie 5
            plotOutput('agregacja_wszystko')),
        sidebarPanel(
            # Do wybrania agregacja czasowa, interakcja tylko z wykresami w tej sekcji
            selectInput(inputId = "agregacja",
                        label = h3("Agregacja czasowa"),
                        choices = colnames(przejazdy)[13:14]),
    )),
    
    h3("Czy pogoda miała wpływ na liczbe przejazdów?"),
    sidebarLayout(
        mainPanel(
            h4("Wpływ wybranego warunku pogodowego na liczbę przejazdów."),
            # Zadanie 7
            plotOutput("przejazdy_a_pogoda_wszystko"),
            # Zadanie 6
            plotOutput("przejazdy_a_pogoda")
        ),
        sidebarPanel(
            # Do wyboru warunki pogodowe, interakcja tylko z wykresami poniższych sekcji
            selectInput(inputId = "warunki_pogodowe",
                         label = h3("Warunki pogodowe"),
                         choices = colnames(przejazdy)[4:12])            
        )
    ),
    
    h3("Czy można zaobserwować jakieś zależności między warunkami pogodowymi?"),
    sidebarLayout(
        mainPanel(
            # Zadanie 9, na korelogram nie mają wpływu filtry znajdujące się na stronie
            h5("Korelacje między zmiennymi opisującymi warunki pogody"),
            plotOutput("korelogram"),
            plotOutput("warunek_a_warunek")
        ),
        sidebarPanel(
            # Do wyboru warunek pogodowy, interakcja z wykresem warunek_a_warunek
            selectInput(inputId = "warunki_pogodowe2",
                        label = h3("Drugi warunek pogodowy"),
                        choices = colnames(przejazdy)[4:12])
        )
    ),
    
)

server <- function(input, output, session) {

    # Zadanie 1
    output$rozkladDni <- renderPlot({
        zliczone_przejazdy <- count(przejazdy %>% 
                                        filter(Data >= input$dateRange1[1] & Data <= input$dateRange1[2]), Stacja)
        ggplot(zliczone_przejazdy, aes(x=reorder(Stacja,n), y=n))+ 
            geom_bar(stat="identity", width=0.65, fill="coral3") +
            geom_text(aes(label=n), position=position_dodge(width=0.9), hjust = -0.1, size=3.5) +
            ylab("Liczba dni pomiarowych") +
            xlab("Punkty pomiaru")+
            ggtitle("Rozkład liczby dni pomiarowych") +
            coord_flip() +
            theme_minimal()
    })

    # Zadanie 2
    output$natezenie_w_czasie <- renderPlot({
        ggplot(przejazdy[przejazdy$Stacja == input$points & 
                             przejazdy$Data >= input$dateRange1[1] &
                             przejazdy$Data <= input$dateRange1[2],], aes(x=Data, y=Licznik))+ 
            geom_bar(stat="identity", width=0.6, fill="coral3") +
            xlab("Data oserwacji")+
            ylab("Liczba przejazdów")+
            ggtitle("Rozkład liczby przejazdów dla wybranej stacji w wybranym okresie czasu") +
            theme_minimal()
    })
    
    output$l_przejazdow_dla_pkt <- renderPlot({
        ggplot(przejazdy[przejazdy$Stacja == input$points & 
                             przejazdy$Data >= input$dateRange1[1] &
                             przejazdy$Data <= input$dateRange1[2],], aes(Licznik)) +
            theme_bw() +
            geom_histogram(bins = 18, fill = viridis(18), color = "white") +  # alternatywne sposoby ustawienia przedziałów poprzez argumenty: `binwidth`, `breaks`
            labs(title = "Rozkład liczby przejazdów dla wybranego punktu",
                 x = "Liczba przejazdów",
                 y = "Liczebność") +
            theme_minimal()
    })

    # Zadanie 3
    output$natezenie <- renderPlot({
        dane_natezenie <- przejazdy %>% 
                                filter(Data >= input$dateRange1[1] & Data <= input$dateRange1[2])
        Aggreg <- aggregate(dane_natezenie$Licznik, by=list(dane_natezenie$Stacja), FUN=mean)
        x = Aggreg$x
        ggplot(Aggreg, aes(x=reorder(Group.1,x), y=x))+
            geom_bar(stat="identity", width=0.65, fill="coral3") +
            geom_text(aes(label=round(x,1)), position=position_dodge(width=0.9), hjust = -0.05, size=3.5) +
            ylab("Liczba przejazdów") +
            xlab("Punkty pomiaru") +
            ggtitle("Średnie natężenie ruchu w określonym przedziale czasowym") +
            coord_flip() +
            theme_minimal()
    })
    
    output$rozklad_przejazdow <- renderPlot({
            dane_natezenie <- przejazdy %>% 
                filter(Data >= input$dateRange1[1] & Data <= input$dateRange1[2])
            ggplot(dane_natezenie, aes(x = Licznik, y = Stacja, fill = factor(Stacja))) +
                geom_density_ridges(scale = 8, show.legend = F) +
                xlab("Liczba przejazdów") +
                ylab(NULL) +
                ggtitle("Rozkłady liczb przejazdów w wybranym okresie czasu") +
                theme_minimal()
        })
    
    # Zadanie 4
    output$agregacja_czasowa <- renderPlot({
        ggplot(data = przejazdy[przejazdy$Stacja == input$points & 
                                    przejazdy$Data >= input$dateRange1[1] &
                                    przejazdy$Data <= input$dateRange1[2],], aes(x = Licznik, y = .data[[input$agregacja]], fill = .data[[input$agregacja]])) +
            geom_density_ridges(scale = 2, show.legend = F) +
            xlab("Liczba przejazdów") +
            ylab(NULL) +
            ggtitle("Rozkład przejazdów dla wybranego punktu w wybranej agregacji czasowej") +
            theme_minimal()
    })
    
    # Zadanie 5
    output$agregacja_wszystko <- renderPlot({
        ggplot(data = przejazdy[przejazdy$Data >= input$dateRange1[1] &
                                    przejazdy$Data <= input$dateRange1[2],], aes(x = Licznik, y = .data[[input$agregacja]], fill = Stacja)) +
            geom_density_ridges(scale = 2, show.legend = T, alpha = 0.3, na.rm = T) +
            xlab("Liczba przejazdów") +
            ylab(NULL) +
            ggtitle("Rozkłady przejazdów dla wybranej agregacji czasowej") +
            theme_minimal()
    })

    # Zadanie 6
    output$przejazdy_a_pogoda <- renderPlot({
        ggplot(data = przejazdy[przejazdy$Stacja == input$points & 
                                    przejazdy$Data >= input$dateRange1[1] &
                                    przejazdy$Data <= input$dateRange1[2],], mapping = aes(x = .data[[input$warunki_pogodowe]], y = Licznik)) +
            theme_bw() +
            ylab("Liczba przejazdów") +
            geom_point(color = 'darkgreen', alpha = 0.5) +
            geom_smooth(method = "lm", color = "coral3") +
            ylim(0, NA) +
            ggtitle('Zależność między liczbą przejazdów a wybranym warunkiem pogodowym w wybranej stacji') +
            theme_minimal()
        
    })
    
    # Zadanie 7
    output$przejazdy_a_pogoda_wszystko <- renderPlot({
        ggplot(data = przejazdy[przejazdy$Data >= input$dateRange1[1] &
                                    przejazdy$Data <= input$dateRange1[2],], mapping = aes(x = .data[[input$warunki_pogodowe]], y = Licznik)) +
            theme_bw() +
            ylab("Liczba przejazdów") +
            geom_point(alpha = 0.5, aes(color = Stacja)) +
            geom_smooth(method = "lm", color = "coral3") +
            ylim(0, NA) +
            ggtitle('Zależność między liczbą przejazdów a wybranym warunkiem pogodowym') +
            theme_minimal()
    })
    
    # Zadanie 8
    punkty$label <- with(punkty, paste(
        "<p> <b>", punkty$stacja, "</b> </br>",
        "Suma przejazdów: ", (przejazdy %>%
                     group_by(Stacja) %>%
                     summarise_at(vars(Licznik), list(suma = sum)))$suma, "</br>",
        "Średnia dzienna:",round((przejazdy %>%
                              group_by(Stacja) %>%
                              summarise_at(vars(Licznik), list(średnia = (mean))))$średnia, digits = 2), "</br>" ,
        "Obliczone z danych pobranych między 01.10.2013 a 31.03.2021",
        "</p>"))
    
    output$mapa <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng = 18.622, lat = 54.375, zoom=11.2) %>%
            addMarkers(data = punkty, label = punkty$stacja, popup = ~label)
    })
    
    # Zadanie 9
    output$korelogram <- renderPlot({
        przejazdy_numerycznie <-przejazdy[ ,unlist(lapply(przejazdy, is.numeric), use.names = FALSE)]
        corrplot(cor(przejazdy_numerycznie), type = "upper", order = "hclust",
                 tl.col = "black", tl.srt = 45)
    })
      
    output$warunek_a_warunek <- renderPlot({
        ggplot(data = przejazdy[przejazdy$Data >= input$dateRange1[1] &
                                    przejazdy$Data <= input$dateRange1[2],], mapping = aes(x = .data[[input$warunki_pogodowe]], y = .data[[input$warunki_pogodowe2]])) +
            theme_bw() +
            geom_point(color = 'cornflowerblue', alpha = 0.5) +
            geom_smooth(method = "lm", color = "coral3") +
            ylim(0, NA) +
            ggtitle('Zależności między zmiennymi opisującymi warunki pogodowe') +
            theme_minimal()
    })
    
    # Dodatkowe statystyki
    output$tabela <- renderTable(expr = {
        przejazdy %>% 
            filter(Data >= input$dateRange1[1] & Data <= input$dateRange1[2]) %>%
            group_by(Stacja) %>%
            filter(Stacja == input$points) %>%
            summarise(Q1 = quantile(.data[["Licznik"]], 0.25), 
                      Mediana = median(.data[["Licznik"]]),
                      Średnia = mean(.data[["Licznik"]]), 
                      Q3 = quantile(.data[["Licznik"]], 0.75))
    })
}

# uruchomienie aplikacji
shinyApp(ui = ui, server = server)

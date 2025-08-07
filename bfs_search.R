
################# Yaraslau Shemet#######################################
###################### Zadanie 1 ###############################################

# Cel - weryfikacja istnienia sciezki od punktu A do obszaru B w macierzy binarnej 

# Wykorzystany algorytm - przeszukiwanie wszersz (ang. breadth-first search BFS)

# Ogolna idea - labirynt jest przedstawiany jako graf, gdzie kazdy punkt to wezel, 
# a krawedzie - ruchy do sasiednich punktow. Sasiednie punkty sa sprawdzane az do momentu 
# trafienia na obszar koncowy. 

d0 <- readRDS(file = "./maze.RDS")
plot(as.raster(d0))

pathQ <- function(input_m, start_point, end_area){
  
  stack_neighbours <- list(start_point) #inicjalizacja sprawdzenia sasiadow
  #rekurencja (DFS) nie zadziala, bo jest zbyt gleboka
  visited_nodes <- matrix(FALSE, nrow(input_m), ncol(input_m)) #aby uniknac przetwarzania wezla wiecej niz jeden raz, oznaczane sa jako FALSE
  
  while (length(stack_neighbours) > 0) { #petla dziala do momentu, gdy istnieja wezly do sprawdzenia
    current_node <- stack_neighbours[[1]] #w taki sposob bedzie sprawdzany kazdy wezel

    if (current_node[[1]] %in% end_area[[1]] & current_node[[2]] %in% end_area[[2]]){ #warunek na bycie w obszarze koncowym
      return(TRUE) #gdy biezacy wezel nalezy do obszaru, zwracane jest TRUE
    } 
    
    moves <- list(c(0,1), #sasiad z gory
                  c(0,-1), #sąsiad z dolu
                  c(1, 0), #sasiad po prawej
                  c(-1,0)) #sasiad po lewej
    for (move in moves) {
      neighbor_x <- current_node[[1]] + move[1] #wspolrzedna sasiada
      neighbor_y <- current_node[[2]] + move[2] #j.w.
      
      if (neighbor_x >= 1 & neighbor_x <= ncol(input_m) & neighbor_y >= 1 & neighbor_y <= nrow(input_m)) { #poruszanie sie tylko wewnatrz granic macierzy wejsciowej
        if (!visited_nodes[neighbor_x, neighbor_y] & input_m[neighbor_x, neighbor_y]) { #tylko nieprzetworzone wezly, ktore nie przerywaja sciezke (FALSE) 
          stack_neighbours <- c(stack_neighbours, list(list(x = neighbor_x, y = neighbor_y))) #rozszerzenie listy sąsiadow
          visited_nodes[neighbor_x, neighbor_y] <- TRUE #oznaczenie sasiada jako sprawdzonego
        }
      }
    }
    
    visited_nodes[current_node[[1]], current_node[[2]]] <- TRUE #oznaczenie biezacego wezla jako sprawdzonego
    stack_neighbours <- stack_neighbours[-1] #sprawdzony wezel jest usuwany
  }
  
  return(FALSE)
}

#1 - z przykladu
logoPosition <- list(x = 387:413, y = 322:348)
startPoint <- list(x = 1, y = 1)
pathQ(d0, startPoint, logoPosition)

#2 - z przykladu
endPosition <- list(x = 220:230, y = 325:335)
startPoint <- list(x = 1, y = 1)
pathQ(d0, startPoint, endPosition)

#3 - dolny prawy obszar
endPosition2 <- list(x = 500:600, y = 200:300)
startPoint <- list(x = 1, y = 1)
pathQ(d0, startPoint, endPosition2)

#sprawdzenie czasu wykonania

# system.time({
#    result <- pathQ(d0, startPoint, endPosition)
#    print(result)
#  }) 

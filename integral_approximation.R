
################# Yaraslau Shemet #######################################
###################### Zadanie 3 ###############################################

# Cel - numeryczne przyblizenie wartosci calki podwojnej na okreslonym przedziale 

# Wykorzystany algorytm - zasada punktu srodkowego (ang. midpoint rule)

# Ogolna idea - podzielenie obszaru calkowania na male prostokaty o rownej szerokosci, 
# nastepnie znalezienie punktu srodkowego kazdego prostokata. 
# Suma objetosci poszczegolnych prostokatow stanowi przyblizenie calki


integrate_3d <- function(f, over, n_x=10^3, n_y=10^3) { #n_x i n_y to liczba prostokatow, na ktore podzielone jest pole pod powierzchnia funkcji
  delta_x <- (over$x[2] - over$x[1]) / n_x #(gorna granica predzialu - lewa granica przedzialu) / liczba prostokatow
  delta_y <- (over$y[2] - over$y[1]) / n_y #j.w.
  value <- 0 #inicjalizacja 
  
  for (i in 1:n_x) { 
    for (j in 1:n_y) {
      # Punkty srodkowe - srednia arytmetyczna wspolrzednych lewego i prawego kranca i-tego i j-tego prostokata
      x_i <- over$x[1] + delta_x/2 + i * delta_x #iteracyjne obliczenie punktow
      y_j <- over$y[1] + delta_y/2 + j * delta_y #j.w.
      value <- value + delta_x * delta_y * f(x_i, y_j) #wartosc calki jest kumulowana
      #szerokosci prostokatow * estymacja wartosci funkcji w punkcie srodkowym
    }
  }
  
  return(value)
  
}

#1 - mala liczba prostokatow 
integrate_3d(
  f = function(x, y) {cos(x) * y},
  over = list(x = c(0, pi / 2), y = c(0, 1)),
  n_x=10, n_y=10)

#2 - domyslna (10^3) 
integrate_3d(
  f = function(x, y) {cos(x) * y},
  over = list(x = c(0, pi / 2), y = c(0, 1)))

#3 - kolejna funkcja z przykladu
integrate_3d(
  f = function(x, y) { (cos(x) + 2) * (sin(y) + 1)},
  over = list(x = c(0, pi), y = c(0, pi)))

#4 - inna funkcja
integrate_3d(
  f = function(x, y) { cos(x^2) + exp(y)},
  over = list(x = c(0, 1), y = c(-1, 1)))

#sprawdzenie czasu wykonania - generalnie im wieksza jest liczba prostokatow, tym wiecej czasu zajmuje obliczenie
#wartosc dodatkowa precyzji aproksymacji jest natomiast coraz mniejsza 

# system.time({
#   result <- integrate_3d(
#     f = function(x, y) {cos(x) * y},
#     over = list(x = c(0, pi / 2), y = c(0, 1)),
#     nx=10^4, ny=10^4)
#   print(result)
# }) 
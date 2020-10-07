mpg <- read.csv("mpg.csv")
head(mpg)

#manufacturer
#model
#displ = engine displacement, in litres (size)
#year = year of manufacture
#cyl = number of cylinders
#trans = type of transmission
#drv = f = front-wheel drive, r = rear wheel drive, 4 = 4wd
#cty = city miles per gallon
#hwy = highway miles per gallon
#fl = fuel type
#class = "type" of car

# Pregunta: autos con motor grande gastan mas combustible que autos con motor chico?

plot(mpg$displ, mpg$hwy)

library(ggplot2)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))

unique(mpg$class)

# Agregando una variable mas al grafico
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, colour = class))
# scalling = proceso de adicionar una estetica a una valor unico de variable

# cambiando el tipo de punto en el grafico:
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# ggplot ocupa seis tipos de punto por vez

# Agregando un color unico al grafico. El color deja de informar algo (una variable). esta apenas cambiando la aparencia
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy) , colour = "red")
# ahora el shape tb deja de informar y queda solamente como estatico
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy) , shape = 6)

# que te parece que va presentar ese codigo?
# que ha de equivocado en ese comando?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

# Que pasa al ocupar una variable continua al color?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = cty))

# y en el size?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = cty))
# y en shape?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = cty))

# Como la estetica cambia en relación a variables categoricas / continuas?

# Mapeando la misma variable en distintos elementos esteticos
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = cty, color = cty))

# Se puede trabajar con condicionales (logicos)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = displ < 5))

# Dividindo los graficos acuerdo a las categorias = facet_warp()
# El primero argumento de facet_earp debe ser la formula en la cual se debe dividir los graficos
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~class)

# Para hacer la división en relación a dos variables categoricas, debemos usar fact_grid
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)

# que pasa si ocupamos una variable continua en facet?
head(mpg)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~cty)

# cuales son las ventajas de usar facet? y las desventajas? 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = class))

# Otros tipos de Graficos
# las capas geometricas geom_*
# las geometrías diferencian el tipo de grafico....
# ejemplo smooth (suavizador)
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

# Muchas veces los parametros de estatica (que figuran en aes() ), son los mismos. Pero no siempre
# ejemplo con smooth no se puede usar "shape", pero si se puede usar linetype
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

# como estamos trabajand con capas del grafico, podemos agregar distintas geometrias (no siempre)

# como se podría agregar al grafico de dispersión el grafico de suavización?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

# perciba que estamos repetindo algunos elementos esteticos ya que informamos para geom_point y geom_smooth.
# lo que se puede hacer es ponerlos de forma global en el ggplot
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

# cuales serían los beneficios de eso?

# Y se queremos que los puntos estén relacionados a la clase de auto?
# Atencion! solo los puntos!
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(aes(colour = class)) + 
  geom_smooth()

# que geom, vos ocuparias para crear un grafico de linea?
# y un boxplot?
# y un histogram?

# que grafico produciria ese codigo? Hay algo de nuevo en él? De que se trata?
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

# estos codigos producirán distintos graficos?
ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(aes(colour = class)) + 
  geom_smooth()




# carat = weight
# cut = quality
# x = length
# y = width
# z = depth
# table = width of top of diamond relative to widest point (43–95)
head(diamonds)
str(diamonds)

# Grafico de dispersión para entender el valor
plot(diamonds$price, diamonds$carat)

plot(diamonds$price, diamonds$table)

# Grafico de caja relacionando precio con categorias
?boxplot()
boxplot(diamonds$price ~ diamonds$clarity)
boxplot(diamonds$price ~ diamonds$cut)

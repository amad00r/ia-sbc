(defclass Bebida "Clase para representar una bebida."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Atributo de tipo Booleano para indicar si una bebida casual es alcohólica o no.
    (slot alcoholica
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Atributo de tipo Booleano para indicar si es elemento es libre de gluten.
    (slot glutenFree
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Atributo de tipo Booleano para indicar si el elemento es libre de lactosa.
    (slot lactosaFree
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Atributo de tipo String para indicar el nombre del elemento en cuestión.
    (slot nombre
        (type STRING)
        (create-accessor read-write))
    ;;; Atributo de tipo Float para indicar el precio del elemento en cuestión.
    (slot precio
        (type FLOAT)
        (create-accessor read-write))
)

(defclass Casual "Clase para representar una bebida casual."
    (is-a Bebida)
    (role concrete)
    (pattern-match reactive)
)

(defclass Vino "Clase para representar un vino."
    (is-a Bebida)
    (role concrete)
    (pattern-match reactive)
)

(defclass Categoria "Clase para representar la categoría de un plato."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Atributo de tipo String para indicar el nombre del elemento en cuestión.
    (slot nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass Ingrediente "Clase para representar un ingrediente de un plato."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Relación para indicar en qué temporada está disponible un ingrediente.
    (multislot disponibleEn
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Atributo de tipo Booleano para indicar si es elemento es libre de gluten.
    (slot glutenFree
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Atributo de tipo Booleano para indicar si el elemento es libre de lactosa.
    (slot lactosaFree
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Atributo de tipo String para indicar el nombre del elemento en cuestión.
    (slot nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass Menu "Clase para representar un menú."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Relación para indicar la primera bebida del menú.
    (slot 1rBebida
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Relación para indicar el primer plato del menú.
    (slot 1rPlato
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Relación para indicar la segunda bebida del menú.
    (slot 2oBebida
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Relación para indicar el segundo plato del menú.
    (slot 2oPlato
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Relación para indicar el postre de un menú.
    (slot postre
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Atributo de tipo Float para indicar el precio del elemento en cuestión.
    (slot precio
        (type FLOAT)
        (create-accessor read-write))
)

(defclass Origen "Clase para representar el origen de un plato."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Atributo de tipo String para indicar el nombre del elemento en cuestión.
    (slot nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass Plato "Clase para representar un plato."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Relación para indicar que ingredientes componen un plato.
    (multislot compuestoPor
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Relación para indicar la categoría de un plato.
    (slot esCategoria
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Relación para indicar el tipo de preparación de un plato.
    (slot esPreparacion
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Relación para indicar que bebidas son incompatibles con un plato.
    (multislot incompatibleConBebida
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Relación para indicar que platos son incompatibles con un plato concreto.
    (multislot incompatibleConPlato
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Relación para indicar el país de origen de un plato.
    (slot originarioDe
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Atributo de tipo Integer para indicar la dificultad de realizar un plato. Su rango va de 1 a 10, siendo 1 el valor que indica menor dificultad y 10 el valor que indica la máxima dificultad.
    (slot dificultad
        (type INTEGER)
        (create-accessor read-write))
    ;;; Atributo de tipo String para indicar el nombre del elemento en cuestión.
    (slot nombre
        (type STRING)
        (create-accessor read-write))
    ;;; Atributo de tipo Float para indicar el precio del elemento en cuestión.
    (slot precio
        (type FLOAT)
        (create-accessor read-write))
    ;;; Atributo de tipo Integer que indica el tipo de plato (Primero, Segundo, Postre). 0 -> Postre // 1 -> Primero // 2 -> Segundo // 3 -> Primero o Segundo
    (slot tipo
        (type INTEGER)
        (create-accessor read-write))
)

(defclass Preparacion "Clase para representar el método de preparación de un plato."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Atributo de tipo String para indicar el nombre del elemento en cuestión.
    (slot nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass Temporada "Clase para representar la temporada en la que está disponible un ingrediente."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Atributo de tipo String para indicar el nombre del elemento en cuestión.
    (slot nombre
        (type STRING)
        (create-accessor read-write))
)

(definstances instances
    ([aceite_de_oliva] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "aceite_de_oliva")
    )

    ([aceitunas] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "aceitunas")
    )

    ([agua_mineral] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "agua_mineral")
         (precio  1.5)
    )

    ([ajo] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "ajo")
    )

    ([albahaca] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "albahaca")
    )

    ([almejas] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "almejas")
    )

    ([apio] of Ingrediente
         (disponibleEn  [otono])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "apio")
    )

    ([arroz] of Categoria
         (nombre  "arroz")
    )

    ([arroz_arborio] of Ingrediente
         (disponibleEn  [otono])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "arroz_arborio")
    )

    ([arroz_blanco] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "arroz_blanco")
    )

    ([arroz_caldoso_con_marisco] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [almejas] [arroz_blanco] [gambas] [mejillones] [pimiento_rojo] [tomate])
         (esCategoria  [arroz])
         (esPreparacion  [hervido])
         (originarioDe  [espana])
         (dificultad  7)
         (nombre  "arroz_caldoso_con_marisco")
         (precio  15.0)
         (tipo 1)
    )

    ([azafran] of Ingrediente
         (disponibleEn  [otono])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "azafran")
    )

    ([bacalao] of Ingrediente
         (disponibleEn  [primavera])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "bacalao")
    )

    ([bacalao_al_pil_pil] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [bacalao] [guindilla])
         (esCategoria  [pescado])
         (esPreparacion  [sarten])
         (originarioDe  [espana])
         (dificultad  6)
         (nombre  "bacalao_al_pil_pil")
         (precio  12.0)
         (tipo 2)
    )

    ([barolo] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "barolo")
         (precio  18.0)
    )

    ([batido_de_fresa] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "batido_de_fresa")
         (precio  2.7)
    )

    ([berenjena] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "berenjena")
    )

    ([calabacin] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "calabacin")
    )

    ([calamar] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "calamar")
    )

    ([calamares_a_la_romana] of Plato
         (compuestoPor  [aceite_de_oliva] [calamar] [harina_de_trigo] [huevos] [limon])
         (esCategoria  [marisco])
         (esPreparacion  [frito])
         (originarioDe  [espana])
         (dificultad  4)
         (nombre  "calamares_a_la_romana")
         (precio  8.5)
         (tipo 1)

    )

    ([caldo_de_carne] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "caldo_de_carne")
    )

    ([caldo_de_verduras] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "caldo_de_verduras")
    )

    ([carne] of Categoria
         (nombre  "carne")
    )

    ([carne_de_res] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "carne_de_res")
    )

    ([cebolla] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "cebolla")
    )

    ([cerveza_artesanal] of Casual
         (alcoholica  TRUE)
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "cerveza_artesanal")
         (precio  3.0)
    )

    ([cerveza_sin_gluten] of Casual
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "cerveza_sin_gluten")
         (precio  3.2)
    )

    ([champinones] of Ingrediente
         (disponibleEn  [otono])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "champinones")
    )

    ([chianti_clasico] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "chianti_clasico")
         (precio  14.0)
    )

    ([chorizo] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "chorizo")
    )

    ([croquetas_de_jamon] of Plato
         (compuestoPor  [aceite_de_oliva] [harina_de_trigo] [jamon_serrano] [leche] [mantequilla])
         (esCategoria  [carne])
         (esPreparacion  [frito])
         (originarioDe  [espana])
         (dificultad  6)
         (nombre  "croquetas_de_jamon")
         (precio  7.5)
         (tipo 1)

    )

    ([crudo] of Preparacion
         (nombre  "crudo")
    )

    ([ensalada_campera] of Plato
         (compuestoPor  [aceite_de_oliva] [aceitunas] [cebolla] [huevos] [patata] [pimiento_verde] [tomate])
         (esCategoria  [vegetariano])
         (esPreparacion  [hervido])
         (originarioDe  [espana])
         (dificultad  3)
         (nombre  "ensalada_campera")
         (precio  6.5)
         (tipo 1)

    )

    ([escalivada] of Plato
         (compuestoPor  [aceite_de_oliva] [berenjena] [cebolla] [pimiento_rojo])
         (esCategoria  [vegetariano])
         (esPreparacion  [horno])
         (originarioDe  [espana])
         (dificultad  4)
         (nombre  "escalivada")
         (precio  7.0)
         (tipo 2)

    )

    ([espaguetis] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "espaguetis")
    )

    ([espana] of Origen
         (nombre  "espana")
    )

    ([esparragos] of Ingrediente
         (disponibleEn  [primavera])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "esparragos")
    )

    ([espinacas] of Ingrediente
         (disponibleEn  [primavera])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "espinacas")
    )

    ([espinacas_con_garbanzos] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [espinacas] [garbanzos] [pimenton])
         (esCategoria  [vegetariano])
         (esPreparacion  [sarten])
         (originarioDe  [espana])
         (dificultad  4)
         (nombre  "espinacas_con_garbanzos")
         (precio  6.5)
         (tipo 1)

    )

    ([fabada_asturiana] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [cebolla] [chorizo] [fabes] [jamon_serrano] [morcilla])
         (esCategoria  [carne])
         (esPreparacion  [hervido])
         (originarioDe  [espana])
         (dificultad  6)
         (nombre  "fabada_asturiana")
         (precio  11.0)
         (tipo 2)

    )

    ([fabes] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "fabes")
    )

    ([frito] of Preparacion
         (nombre  "frito")
    )

    ([frittata_di_zucchine] of Plato
         (compuestoPor  [aceite_de_oliva] [calabacin] [cebolla] [huevos] [queso_parmesano])
         (esCategoria  [huevo])
         (esPreparacion  [sarten])
         (originarioDe  [italia])
         (dificultad  4)
         (nombre  "frittata_di_zucchine")
         (precio  7.5)
         (tipo 0)

    )

    ([gambas] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "gambas")
    )

    ([garbanzos] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "garbanzos")
    )

    ([garrofo] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "garrofo")
    )

    ([gazpacho_andaluz] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [pan] [pepino] [pimiento_verde] [tomate] [vinagre])
         (esCategoria  [vegetariano])
         (esPreparacion  [crudo])
         (originarioDe  [espana])
         (dificultad  3)
         (nombre  "gazpacho_andaluz")
         (precio  6.0)
         (tipo 0)

    )

    ([gnocchi] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "gnocchi")
    )

    ([gnocchi_al_pesto] of Plato
         (compuestoPor  [gnocchi] [pesto] [queso_parmesano])
         (esCategoria  [pasta])
         (esPreparacion  [hervido])
         (originarioDe  [italia])
         (dificultad  5)
         (nombre  "gnocchi_al_pesto")
         (precio  10.0)
         (tipo 1)

    )

    ([guindilla] of Ingrediente
         (disponibleEn  [primavera])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "guindilla")
    )

    ([guisantes] of Ingrediente
         (disponibleEn  [primavera])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "guisantes")
    )

    ([harina_de_trigo] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "harina_de_trigo")
    )

    ([hervido] of Preparacion
         (nombre  "hervido")
    )

    ([horno] of Preparacion
         (nombre  "horno")
    )

    ([huevo] of Categoria
         (nombre  "huevo")
    )

    ([huevos] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "huevos")
    )

    ([huevos_rotos_con_jamon] of Plato
         (compuestoPor  [aceite_de_oliva] [huevos] [jamon_serrano] [patata])
         (esCategoria  [huevo])
         (esPreparacion  [sarten])
         (originarioDe  [espana])
         (dificultad  4)
         (nombre  "huevos_rotos_con_jamon")
         (precio  8.0)
         (tipo 1)

    )

    ([insalata_caprese] of Plato
         (compuestoPor  [aceite_de_oliva] [albahaca] [mozzarella] [tomate])
         (esCategoria  [vegetariano])
         (esPreparacion  [crudo])
         (originarioDe  [italia])
         (dificultad  2)
         (nombre  "insalata_caprese")
         (precio  6.0)
         (tipo 2)

    )

    ([invierno] of Temporada
         (nombre  "invierno")
    )

    ([italia] of Origen
         (nombre  "italia")
    )

    ([jamon_serrano] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "jamon_serrano")
    )

    ([judia_verde] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "judia_verde")
    )

    ([lambrusco] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "lambrusco")
         (precio  8.5)
    )

    ([laminas_de_lasana] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "laminas_de_lasana")
    )

    ([lasagna_alla_bolognese] of Plato
         (compuestoPor  [carne_de_res] [harina_de_trigo] [laminas_de_lasana] [leche] [mantequilla] [queso_parmesano] [tomate])
         (esCategoria  [pasta])
         (esPreparacion  [horno])
         (originarioDe  [italia])
         (dificultad  7)
         (nombre  "lasagna_alla_bolognese")
         (precio  13.5)
         (tipo 2)

    )

    ([leche] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "leche")
    )

    ([leche_con_cacao] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "leche_con_cacao")
         (precio  2.5)
    )

    ([limon] of Ingrediente
         (disponibleEn  [primavera])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "limon")
    )

    ([mantequilla] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "mantequilla")
    )

    ([marisco] of Categoria
         (nombre  "marisco")
    )

    ([masa_de_pizza] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "masa_de_pizza")
    )

    ([mejillones] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "mejillones")
    )

    ([melanzane_alla_parmigiana] of Plato
         (compuestoPor  [aceite_de_oliva] [albahaca] [berenjena] [mozzarella] [queso_parmesano] [tomate])
         (esCategoria  [vegetariano])
         (esPreparacion  [horno])
         (originarioDe  [italia])
         (dificultad  6)
         (nombre  "melanzane_alla_parmigiana")
         (precio  9.5)
         (tipo 2)

    )

    ([merluza] of Ingrediente
         (disponibleEn  [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "merluza")
    )

    ([merluza_a_la_koskera] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [almejas] [esparragos] [guisantes] [huevos] [merluza])
         (esCategoria  [pescado])
         (esPreparacion  [hervido])
         (originarioDe  [espana])
         (dificultad  6)
         (nombre  "merluza_a_la_koskera")
         (precio  12.0)
         (tipo 1)

    )

    ([morcilla] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  FALSE)
         (lactosaFree  FALSE)
         (nombre  "morcilla")
    )

    ([mozzarella] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "mozzarella")
    )

    ([nata] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "nata")
    )

    ([oregano] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "oregano")
    )

    ([osobuco_de_ternera] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "osobuco_de_ternera")
    )

    ([ossobuco_alla_milanese] of Plato
         (compuestoPor  [apio] [caldo_de_carne] [cebolla] [osobuco_de_ternera] [tomate] [vino_blanco] [zanahoria])
         (esCategoria  [carne])
         (esPreparacion  [sarten])
         (originarioDe  [italia])
         (dificultad  7)
         (nombre  "ossobuco_alla_milanese")
         (precio  16.0)
         (tipo 1)

    )

    ([otono] of Temporada
         (nombre  "otono")
    )

    ([paella_valenciana] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [arroz_blanco] [garrofo] [judia_verde] [pollo] [tomate])
         (esCategoria  [arroz])
         (esPreparacion  [horno])
         (originarioDe  [espana])
         (dificultad  7)
         (nombre  "paella_valenciana")
         (precio  14.5)
         (tipo 2)

    )

    ([pan] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "pan")
    )

    ([panceta] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "panceta")
    )

    ([pasta] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "pasta")
    )

    ([pasta_al_pomodoro] of Plato
         (compuestoPor  [aceite_de_oliva] [albahaca] [pasta] [queso_parmesano] [tomate])
         (esCategoria  [pasta])
         (esPreparacion  [hervido])
         (originarioDe  [italia])
         (dificultad  3)
         (nombre  "pasta_al_pomodoro")
         (precio  8.0)
         (tipo 1)

    )

    ([pasta_alla_norma] of Plato
         (compuestoPor  [aceite_de_oliva] [albahaca] [berenjena] [pasta] [ricotta] [tomate])
         (esCategoria  [pasta])
         (esPreparacion  [hervido])
         (originarioDe  [italia])
         (dificultad  5)
         (nombre  "pasta_alla_norma")
         (precio  9.0)
         (tipo 1)

    )

    ([patata] of Ingrediente
         (disponibleEn  [invierno] [otono])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "patata")
    )

    ([pepino] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pepino")
    )

    ([pescado] of Categoria
         (nombre  "pescado")
    )

    ([pesto] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "pesto")
    )

    ([pimenton] of Ingrediente
         (disponibleEn  [otono])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pimenton")
    )

    ([pimienta_negra] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pimienta_negra")
    )

    ([pimiento_rojo] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pimiento_rojo")
    )

    ([pimiento_verde] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pimiento_verde")
    )

    ([pimientos_rellenos] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [carne_de_res] [cebolla] [pimiento_rojo] [tomate])
         (esCategoria  [carne])
         (esPreparacion  [horno])
         (originarioDe  [espana])
         (dificultad  5)
         (nombre  "pimientos_rellenos")
         (precio  9.0)
         (tipo 2)

    )

    ([pizza_margherita] of Plato
         (compuestoPor  [aceite_de_oliva] [albahaca] [masa_de_pizza] [mozzarella] [tomate])
         (esCategoria  [vegetariano])
         (esPreparacion  [horno])
         (originarioDe  [italia])
         (dificultad  5)
         (nombre  "pizza_margherita")
         (precio  9.0)
         (tipo 2)

    )

    ([plancha] of Preparacion
         (nombre  "plancha")
    )

    ([polenta] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "polenta")
    )

    ([polenta_con_funghi] of Plato
         (compuestoPor  [aceite_de_oliva] [champinones] [polenta] [queso_parmesano])
         (esCategoria  [vegetariano])
         (esPreparacion  [hervido])
         (originarioDe  [italia])
         (dificultad  4)
         (nombre  "polenta_con_funghi")
         (precio  8.5)
         (tipo 0)

    )

    ([pollo] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pollo")
    )

    ([primavera] of Temporada
         (nombre  "primavera")
    )

    ([prosecco] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "prosecco")
         (precio  11.5)
    )

    ([pulpo] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pulpo")
    )

    ([pulpo_a_la_gallega] of Plato
         (compuestoPor  [aceite_de_oliva] [patata] [pimenton] [pulpo] [sal_gruesa])
         (esCategoria  [marisco])
         (esPreparacion  [hervido])
         (originarioDe  [espana])
         (dificultad  6)
         (nombre  "pulpo_a_la_gallega")
         (precio  13.0)
         (tipo 1)

    )

    ([queso_parmesano] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "queso_parmesano")
    )

    ([ravioli_di_ricotta_e_spinaci] of Plato
         (compuestoPor  [espinacas] [mantequilla] [queso_parmesano] [raviolis] [ricotta])
         (esCategoria  [pasta])
         (esPreparacion  [hervido])
         (originarioDe  [italia])
         (dificultad  6)
         (nombre  "ravioli_di_ricotta_e_spinaci")
         (precio  11.0)
         (tipo 2)

    )

    ([raviolis] of Ingrediente
         (disponibleEn  [otono])
         (glutenFree  FALSE)
         (lactosaFree  FALSE)
         (nombre  "raviolis")
    )

    ([refresco_de_cola] of Casual
         (alcoholica  FALSE)
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "refresco_de_cola")
         (precio  2.0)
    )

    ([ricotta] of Ingrediente
         (disponibleEn  [primavera])
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "ricotta")
    )

    ([risotto_ai_funghi] of Plato
         (compuestoPor  [arroz_arborio] [caldo_de_verduras] [cebolla] [champinones] [mantequilla] [queso_parmesano])
         (esCategoria  [arroz])
         (esPreparacion  [sarten])
         (originarioDe  [italia])
         (dificultad  6)
         (nombre  "risotto_ai_funghi")
         (precio  12.0)
         (tipo 1)

    )

    ([risotto_alla_milanese] of Plato
         (compuestoPor  [arroz_arborio] [azafran] [caldo_de_carne] [cebolla] [mantequilla] [queso_parmesano])
         (esCategoria  [arroz])
         (esPreparacion  [sarten])
         (originarioDe  [italia])
         (dificultad  6)
         (nombre  "risotto_alla_milanese")
         (precio  12.5)
         (tipo 2)

    )

    ([sal_gruesa] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "sal_gruesa")
    )

    ([sarten] of Preparacion
         (nombre  "sarten")
    )

    ([spaghetti_alla_carbonara] of Plato
         (compuestoPor  [espaguetis] [huevos] [panceta] [pimienta_negra] [queso_parmesano])
         (esCategoria  [pasta])
         (esPreparacion  [sarten])
         (originarioDe  [italia])
         (dificultad  5)
         (nombre  "spaghetti_alla_carbonara")
         (precio  10.5)
         (tipo 1)

    )

    ([te_helado_de_limon] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "te_helado_de_limon")
         (precio  2.2)
    )

    ([tomate] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "tomate")
    )

    ([tortilla_de_patatas] of Plato
         (compuestoPor  [aceite_de_oliva] [cebolla] [huevos] [patata])
         (esCategoria  [huevo])
         (esPreparacion  [sarten])
         (originarioDe  [espana])
         (dificultad  5)
         (nombre  "tortilla_de_patatas")
         (precio  7.5)
         (tipo 1)

    )

    ([trofie] of Ingrediente
         (disponibleEn  [primavera])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "trofie")
    )

    ([trofie_al_pesto] of Plato
         (compuestoPor  [pesto] [queso_parmesano] [trofie])
         (esCategoria  [pasta])
         (esPreparacion  [hervido])
         (originarioDe  [italia])
         (dificultad  4)
         (nombre  "trofie_al_pesto")
         (precio  10.0)
         (tipo 1)

    )

    ([vegetariano] of Categoria
         (nombre  "vegetariano")
    )

    ([verano] of Temporada
         (nombre  "verano")
    )

    ([vinagre] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "vinagre")
    )

    ([vino_blanco] of Ingrediente
         (disponibleEn  [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "vino_blanco")
    )

    ([vino_blanco_albarino] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "vino_blanco_albarino")
         (precio  10.0)
    )

    ([vino_rosado_navarra] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "vino_rosado_navarra")
         (precio  9.0)
    )

    ([vino_tinto_rioja] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "vino_tinto_rioja")
         (precio  12.5)
    )

    ([zanahoria] of Ingrediente
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "zanahoria")
    )

    ([zumo_de_naranja_natural] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "zumo_de_naranja_natural")
         (precio  2.8)
    )

)

;; Para almacenar las preferencias y restricciones del usuario.
(defclass Preferencias
     (is-a USER)
     (slot num_comensales)
     (slot precio_min)
     (slot precio_max)
     (slot temporada)
     (slot alcoholica)
     (slot vino)
     (slot diferentesBebidas)
     (slot intolerancia_gluten)
     (slot intolerancia_lactosa)
)

(defmodule MAIN 
     (export ?ALL)
)

;Módulo para solicitar las preferencias del usuario.
(defmodule RicoRico_Entrada
     (import MAIN ?ALL)
     (export ?ALL)
)

;Módulo para realizar el filtrado según las preferencias del usuario.
(defmodule RicoRico_Filtrado
     (import MAIN ?ALL)
     (import RicoRico_Entrada ?ALL)
     (export ?ALL)
)

;Módulo para generar los menús.
(defmodule RicoRico_Generador
     (import MAIN ?ALL)
     (import RicoRico_Entrada ?ALL)
     (import RicoRico_Filtrado ?ALL)
     (export ?ALL)
)

;Módulo para mostrar los menús generados al usuario.
(defmodule RicoRico_Salida
     (import MAIN ?ALL)
     (import RicoRico_Entrada ?ALL)
     (import RicoRico_Filtrado ?ALL)
     (import RicoRico_Generador ?ALL)
     (export ?ALL)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; MAIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule MAIN::inicio 
     (declare (salience 25)) 
     => 
     (printout t "Bienvenido al creador de menús para el catering RicoRico." crlf)
     (focus RicoRico_Entrada)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; ENTRADA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffunction RicoRico_Entrada::obtener_numero_comensales ()
     ;;; Función para obtener el número de comensales.
     (printout t"Introuce el número de comensales: ")
     (bind ?num_comensales (read))
     (while (<= ?num_comensales 0) do
          (printout t "Por favor, introduce un número de comensales correcto: ")
          (bind ?num_comensales (read))
     )
     (printout t crlf)
     (return ?num_comensales)
)

(deffunction RicoRico_Entrada::obtener_precio_min ()
     ;;; Función para obtener el precio mínimo del menú.
     (printout t"Introduce el precio mínimo del menú: ")
     (bind ?precio_min (read))
     (while (<= ?precio_min 0.0) do
          (printout t "Por favor, introduce un precio valido: ")
          (bind ?precio_min (read))
     )
     (printout t crlf)
     (return ?precio_min)
)

(deffunction RicoRico_Entrada::obtener_precio_max (?precio_min)
     ;;; Función para obtener el precio máximo del menú.
     (printout t "Introduce el precio máximo del menú: ")
     (bind ?precio_max (read))
     (while (or (<= ?precio_max 0.0) (<= ?precio_max ?precio_min)) do
          (if (< ?precio_max 0) then
               (printout t "Por favor, introduce un precio válido: ")
          else
               (printout t "El precio máximo debe ser superior al precio mínimo. Por favor, introduce un precio válido: ")
          )
          (bind ?precio_max (read))
     )
     (printout t crlf)
     (return ?precio_max)
)

(deffunction RicoRico_Entrada::seleccion_una_opcion (?question $?opcions)
     ;;; Función generica para seleccionar entre una opción de varias.
     (printout t ?question crlf)
     (printout t "Las opciones son: " $?opcions crlf)
     (printout t "Opcion: ")
     (bind ?respuesta (read))
     (while (not (member$ ?respuesta $?opcions)) do 
          (printout t "La respuesta introducida no forma parte de las opciones. Por favor, introduce una válida." crlf)
          (printout t "Opcion: ")
          (bind ?respuesta (read))
     )
     (printout t crlf)
     (return ?respuesta)
)

(deffunction RicoRico_Entrada::obtener_bebida () 
     ;;; Función para obtener las preferencias y restricciones de bebida.
     (bind ?alcoholica (seleccion_una_opcion "Quieres que el menú tenga bebidas alcoholicas? " si no))
     (bind ?alcoholica (if (eq ?alcoholica si) then TRUE else FALSE))

     (if (eq ?alcoholica TRUE) then 
          (bind ?vino (seleccion_una_opcion "Quieres vino con el menú? " si no)) 
          (if (eq ?vino si) then (bind ?vino TRUE) else (bind ?vino FALSE))
     else (bind ?vino FALSE))

     (bind ?diferentesBebidas (seleccion_una_opcion "Quieres una bebida diferente para el primer plato y el segundo? " si no))
     (if (eq ?diferentesBebidas si) then (bind ?diferentesBebidas TRUE) else (bind ?diferentesBebidas FALSE))

     (return (create$ ?alcoholica ?vino ?diferentesBebidas))
)

(deffunction RicoRico_Entrada::obtener_intolerancias ()
     ;;; Función para obtener las intolerancias alimentarias.
     (bind ?alguna_intolerancia (seleccion_una_opcion "¿Hay alguien con intolerancia alimentaria?" si no))
     (bind ?intolerancia_gluten FALSE)
     (bind ?intolerancia_lactosa FALSE)

     (if (eq ?alguna_intolerancia si) then 
          (bind ?intolerancia_gluten (seleccion_una_opcion "¿Es intolerante al gluten?" si no))
          (if (eq ?intolerancia_gluten si) then (bind ?intolerancia_gluten TRUE) else (bind ?intolerancia_gluten FALSE))

          (bind ?intolerancia_lactosa (seleccion_una_opcion "¿Es intolerante a la lactosa?" si no))
          (if (eq ?intolerancia_lactosa si) then (bind ?intolerancia_lactosa TRUE) else (bind ?intolerancia_lactosa FALSE))
     )

    (return (create$ ?intolerancia_gluten ?intolerancia_lactosa))
)

(deffunction RicoRico_Entrada::obtener_preferencias_restricciones ()
     ;;Obtenemos el número de comensales, el precio mínimo y máximo y la temporada
     (bind ?num_comensales (obtener_numero_comensales))
     (bind ?precio_min (obtener_precio_min))
     (bind ?precio_max (obtener_precio_max ?precio_min))
     (bind ?temporada (seleccion_una_opcion "Introduzca la temporada del año." invierno primavera otono verano))

     ;; Obtenemos las preferencias y restricciones de bebida
     (bind $?condiciones_bebida (obtener_bebida))
     (bind ?alcoholica (nth$ 1 $?condiciones_bebida))
     (bind ?vino (nth$ 2 $?condiciones_bebida))
     (bind ?diferentesBebidas (nth$ 3 $?condiciones_bebida))
     
     ;; Obtenemos las intolerancias alimentarias
     (bind $?lista_intolerancias (obtener_intolerancias))
     (bind ?intolerancia_gluten (nth$ 1 $?lista_intolerancias))
     (bind ?intolerancia_lactosa (nth$ 2 $?lista_intolerancias))

     (make-instance prefs of Preferencias
          (num_comensales ?num_comensales)
          (precio_min ?precio_min)
          (precio_max ?precio_max)
          (temporada ?temporada)
          (alcoholica ?alcoholica)
          (vino ?vino)
          (diferentesBebidas ?diferentesBebidas)
          (intolerancia_gluten ?intolerancia_gluten)
          (intolerancia_lactosa ?intolerancia_lactosa)
     )
)

(defrule RicoRico_Entrada::instanciacion_preferencias_restricciones
     (declare (salience 20))
     => 
     (printout t "Ahora vamos a hacerte una serie de preguntas para poder generar el menú en base a tus preferencias y restricciones." crlf crlf)
     (obtener_preferencias_restricciones)
     (printout t "Preferencias y restricciones recopiladas correctamente." crlf)
     (focus RicoRico_Filtrado)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; FILTRADO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffunction RicoRico_Filtrado::eliminar_platos_complejos ()
     (printout t "Eliminando platos complejos..." crlf)

     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (bind ?comensales (send ?pref get-num_comensales))

     (bind ?lista (find-all-instances ((?p Plato)) TRUE))

     (foreach ?plato ?lista
          (bind ?dificultad (send ?plato get-dificultad))
          (if (or 
               (and (> ?comensales 20) (> ?dificultad 9))
               (and (> ?comensales 50) (> ?dificultad 7))
               (and (> ?comensales 100) (> ?dificultad 5)))
          then
               (send ?plato delete)
          )
     )
)

(deffunction RicoRico_Filtrado::eliminar_platos_fuera_temporada ()

     (printout t "Eliminando platos fuera de temporada..." crlf) 

     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (bind ?temporada (send ?pref get-temporada))

     (bind ?lista1 (find-all-instances ((?l1 Plato)) TRUE))

     (foreach ?plato ?lista1
          (bind ?ingredienteList (send ?plato get-compuestoPor))
          (bind ?delete FALSE)

          (bind ?lista2 (find-all-instances ((?l2 Ingrediente)) TRUE))
          
          (foreach ?ingrediente ?lista2
               (bind ?disponibles (send ?ingrediente get-disponibleEn))

               (if (and (member$ (instance-name ?ingrediente) ?ingredienteList)
                        (not (member$ (instance-name ?temporada) ?disponibles)))
               then
                    (bind ?delete TRUE)
               )
          )

          (if ?delete then
               (send ?plato delete)     
          )
     )
)

(deffunction RicoRico_Filtrado::eliminar_bebidas_alcoholicas ()
    
     (printout t "Eliminando bebidas alcoholicas..." crlf)

     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (if (eq (send ?pref get-alcoholica) FALSE) then

          (do-for-all-instances ((?bebida Bebida))
               (if (eq (send ?bebida get-alcoholica) TRUE) then
                    (send ?bebida delete)
               )   
          )
     )
)

(deffunction RicoRico_Filtrado::eliminar_vinos ()

     (printout t "Eliminando vinos..." crlf)

     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (if (eq (send ?pref get-vino) FALSE) then

          (do-for-all-instances ((?vino Vino))
               (send ?vino delete)  
          )
     )
)

(deffunction RicoRico_Filtrado::eliminar_platos_lactosa ()

     (printout t "Eliminando platos con lactosa..." crlf)
    
     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (bind ?lactosa (send ?pref get-intolerancia_lactosa))
     (if ?lactosa then

          (bind ?lista1 (find-all-instances ((?l1 Plato)) TRUE))

          (foreach ?plato ?lista1
               (bind ?ingredienteList (send ?plato get-compuestoPor))
               (bind ?delete FALSE)
               
               (foreach ?ingrediente ?ingredienteList

                    (if (not (send ?ingrediente get-lactosaFree)) then
                         (bind ?delete TRUE)
                    )
               )

               (if ?delete then
                    (send ?plato delete)     
               )
          )
     )
)

(deffunction RicoRico_Filtrado::eliminar_bebidas_lactosa ()

     (printout t "Eliminando bebidas con lactosa..." crlf)

     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (if (send ?pref get-intolerancia_lactosa) then

          (do-for-all-instances ((?bebida Bebida))
               (if (not (send ?bebida get-lactosaFree)) then
                    (send ?bebida delete)
               )   
          )
     )
)

(deffunction RicoRico_Filtrado::eliminar_platos_gluten ()

     (printout t "Eliminando platos con gluten..." crlf)

     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (bind ?gluten (send ?pref get-intolerancia_gluten))

     (if ?gluten then

          (bind ?lista1 (find-all-instances ((?l1 Plato)) TRUE))
          (foreach ?plato ?lista1
               (bind ?ingredienteList (send ?plato get-compuestoPor))
               (bind ?delete FALSE)

               (foreach ?ingrediente ?ingredienteList

                    (if (not (send ?ingrediente get-glutenFree)) then
                         (bind ?delete TRUE)
                    )
               )

               (if ?delete then
                    (send ?plato delete)     
               )
          )
     )
)

(deffunction RicoRico_Filtrado::eliminar_bebidas_gluten ()

     (printout t "Eliminando bebidas con gluten..." crlf)
    
     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (if (send ?pref get-intolerancia_gluten) then

          (do-for-all-instances ((?bebida Bebida))
               (if (not (send ?bebida get-glutenFree)) then
                    (send ?bebida delete)
               )   
          )
     )
)

(defrule RicoRico_Filtrado::filtrado
     (declare (salience 15))
     =>
     (eliminar_bebidas_alcoholicas)
     (eliminar_bebidas_gluten)
     (eliminar_bebidas_lactosa)
     (eliminar_platos_complejos)
     ;(eliminar_platos_fuera_temporada)
     (eliminar_platos_gluten)
     (eliminar_platos_lactosa)
     (eliminar_vinos)
     (focus RicoRico_Generador)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; GENERADOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;TODO: CAMBIAR A FUNCION REAL; MOC!
; (deffunction RicoRico_Generador::crear_menus_hardcoded ()
;      (make-instance [menu1] of Menu
;           (1rBebida [agua_mineral])
;           (1rPlato [paella_valenciana])
;           (2oBebida [vino_tinto_rioja])
;           (2oPlato [tortilla_de_patatas])
;           (postre [gazpacho_andaluz])
;           (precio (+ (send [agua_mineral] get-precio)
;                      (send [paella_valenciana] get-precio)
;                      (send [vino_tinto_rioja] get-precio)
;                      (send [tortilla_de_patatas] get-precio)
;                      (send [gazpacho_andaluz] get-precio)))
;      )
;      (make-instance [menu2] of Menu
;           (1rBebida [prosecco])
;           (1rPlato [spaghetti_alla_carbonara])
;           (2oBebida [chianti_clasico])
;           (2oPlato [pizza_margherita])
;           (postre [insalata_caprese])
;           (precio (+ (send [prosecco] get-precio)
;                      (send [spaghetti_alla_carbonara] get-precio)
;                      (send [chianti_clasico] get-precio)
;                      (send [pizza_margherita] get-precio)
;                      (send [insalata_caprese] get-precio)))
;      )
;      (make-instance [menu3] of Menu
;           (1rBebida [cerveza_sin_gluten])
;           (1rPlato [arroz_caldoso_con_marisco])
;           (2oBebida [vino_blanco_albarino])
;           (2oPlato [ossobuco_alla_milanese])
;           (postre [ensalada_campera])
;           (precio (+ (send [cerveza_sin_gluten] get-precio)
;                      (send [arroz_caldoso_con_marisco] get-precio)
;                      (send [vino_blanco_albarino] get-precio)
;                      (send [ossobuco_alla_milanese] get-precio)
;                      (send [ensalada_campera] get-precio)))
;      )
; ) 

(deffunction RicoRico_Generador::compute_price (?b1 ?p1 ?b2 ?p2 ?pst) (+
    (send ?b1  get-precio)
    (send ?p1  get-precio)
    (if (eq (send ?b1 get-nombre) (send ?b2 get-nombre)) then 0 else (send ?b2 get-precio))
    (send ?p2  get-precio)
    (send ?pst get-precio)
))

(defrule RicoRico_Generador::generador
     (declare (salience 10))
     ?b1  <- (object (is-a Bebida))
     ?b2  <- (object (is-a Bebida))
     ?p1  <- (object (is-a Plato) (tipo ?t1&:(or (eq 1 ?t1) (eq 3 ?t1))))
     ?p2  <- (object (is-a Plato) (tipo ?t2&:(or (eq 2 ?t2) (eq 3 ?t2))))
     ?pst <- (object (is-a Plato) (tipo 0))
     =>
     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (bind ?price (compute_price ?b1 ?p1 ?b2 ?p2 ?pst))
     (if (and
          (neq ?p1 ?p2)
          (>= ?price (send ?pref get-precio_min))
          (<= ?price (send ?pref get-precio_max))
          (or
               (and (eq TRUE (send ?pref get-diferentesBebidas)) (neq ?b1 ?b2))
               (and (eq FALSE (send ?pref get-diferentesBebidas)) (eq ?b1 ?b2))
          ))
          then
          (make-instance of Menu
               (1rBebida ?b1)
               (1rPlato ?p1)
               (2oBebida ?b2)
               (2oPlato ?p2)
               (postre ?pst)
               (precio ?price))
     )
)

(defrule RicoRico_Generador::filter_menus
    (declare (salience 9))
    =>
    (bind ?menus (find-all-instances ((?m Menu)) TRUE))
    (bind ?count (length$ ?menus))
    (if (> ?count 3) then
        ;(bind ?sorted (sort ?menus compare-by-precio))

        ;; Select 3 random indices between 1 and ?count (inclusive)
        ;(bind ?m1 (nth$ (random 1 (- ?count 2)) ?sorted))
        ;(bind ?m2 (nth$ (random (+ ?m1 1) (- ?count 1)) ?sorted))
        ;(bind ?m3 (nth$ (random (+ ?m1 1) (- ?m2 1)) ?sorted))
        (bind ?m1 (nth$ (random 1 ?count) ?menus))
        (bind ?m2 (nth$ (random 1 ?count) ?menus))
        (bind ?m3 (nth$ (random 1 ?count) ?menus))

        (do-for-all-instances ((?m Menu)) TRUE
            (if (and (neq ?m ?m1) (neq ?m ?m2) (neq ?m ?m3)) then
                (send ?m delete)))
    )

     (focus RicoRico_Salida)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; SALIDA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule RicoRico_Salida::escrituraSalida
    (declare (salience 5))
    =>
    (printout t "Procesando la salida ..." crlf)

    ; Mostrar las preferencias y restricciones del usuario
    (bind ?prefs (find-instance ((?p Preferencias)) TRUE))
    (if (neq ?prefs nil) then
        (bind ?prefs (nth$ 1 ?prefs))
        (printout t "Preferencias y restricciones definidas por el usuario:" crlf)
        (printout t "       Número de comensales: " (send ?prefs get-num_comensales) crlf)
        (printout t "       Precio mínimo: " (send ?prefs get-precio_min) "€" crlf)
        (printout t "       Precio máximo: " (send ?prefs get-precio_max) "€" crlf)
        (printout t "       Temporada: " (send ?prefs get-temporada) crlf)
        (printout t "       ¿Bebida alcohólica?: " (if (eq (send ?prefs get-alcoholica) TRUE) then "Si" else "No") crlf)
        (printout t "       ¿Vino?: " (if (eq (send ?prefs get-vino) TRUE) then "Si" else "No") crlf)
        (printout t "       ¿Diferentes bebidas?: " (if (eq (send ?prefs get-diferentesBebidas) TRUE) then "Si" else "No") crlf)
        (printout t "       ¿Intolerancia al gluten?: " (if (eq (send ?prefs get-intolerancia_gluten) TRUE) then "Si" else "No") crlf)
        (printout t "       ¿Intolerancia a la lactosa?: " (if (eq (send ?prefs get-intolerancia_lactosa) TRUE) then "Si" else "No") crlf crlf)
    else
        (printout t "No hay preferencias definidas." crlf)
    )

    ; Mostrar menús según el número disponible
    (switch (length$ (find-all-instances ((?m Menu)) TRUE))
        (case 1 then
            (printout t "Con las restricciones actuales, solo se ha podido generar 1 menú." crlf)
            (printout t "Menú disponible:" crlf)
        )
        (case 2 then
            (printout t "Con las restricciones actuales, solo se han podido generar 2 menús." crlf)
            (printout t "Menús disponibles:" crlf)
        )
        (case 3 then
            (printout t "Se han generado los 3 menús." crlf)
            (printout t "Menús disponibles:" crlf)
        )
        (default
            (printout t "No se ha podido generar ningún menú - Condiciones demasiado restrictivas." crlf)
        )
    )
)

(defrule RicoRico_Salida::escrituraMenu
    (declare (salience 3))
    ?menu <- (object
        (is-a      Menu)
        (1rBebida ?b1)
        (1rPlato  ?p1)
        (2oBebida ?b2)
        (2oPlato  ?p2)
        (postre   ?pst)
        (precio   ?precio))
    =>
    (if (eq (send ?b1 get-nombre) (send ?b2 get-nombre)) then
        (printout t
            "Menú (" ?precio " euros)" crlf
            "    Bebida: "        (send ?b1  get-nombre) crlf
            "    Primer plato: "  (send ?p1  get-nombre) crlf
            "    Segundo plato: " (send ?p2  get-nombre) crlf
            "    Postre: "        (send ?pst get-nombre) crlf
            crlf
        )
    else
        (printout t
            "Menú (" ?precio " euros)" crlf
            "    Primera bebida: " (send ?b1  get-nombre) crlf
            "    Primer plato: "   (send ?p1  get-nombre) crlf
            "    Segunda bebida: " (send ?b2  get-nombre) crlf
            "    Segundo plato: "  (send ?p2  get-nombre) crlf
            "    Postre: "         (send ?pst get-nombre) crlf
            crlf
        )
    )
)

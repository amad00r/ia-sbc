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
    )

    ([escalivada] of Plato
         (compuestoPor  [aceite_de_oliva] [berenjena] [cebolla] [pimiento_rojo])
         (esCategoria  [vegetariano])
         (esPreparacion  [horno])
         (originarioDe  [espana])
         (dificultad  4)
         (nombre  "escalivada")
         (precio  7.0)
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
    )

    ([fabada_asturiana] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [cebolla] [chorizo] [fabes] [jamon_serrano] [morcilla])
         (esCategoria  [carne])
         (esPreparacion  [hervido])
         (originarioDe  [espana])
         (dificultad  6)
         (nombre  "fabada_asturiana")
         (precio  11.0)
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
    )

    ([insalata_caprese] of Plato
         (compuestoPor  [aceite_de_oliva] [albahaca] [mozzarella] [tomate])
         (esCategoria  [vegetariano])
         (esPreparacion  [crudo])
         (originarioDe  [italia])
         (dificultad  2)
         (nombre  "insalata_caprese")
         (precio  6.0)
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
    )

    ([pasta_alla_norma] of Plato
         (compuestoPor  [aceite_de_oliva] [albahaca] [berenjena] [pasta] [ricotta] [tomate])
         (esCategoria  [pasta])
         (esPreparacion  [hervido])
         (originarioDe  [italia])
         (dificultad  5)
         (nombre  "pasta_alla_norma")
         (precio  9.0)
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
    )

    ([pizza_margherita] of Plato
         (compuestoPor  [aceite_de_oliva] [albahaca] [masa_de_pizza] [mozzarella] [tomate])
         (esCategoria  [vegetariano])
         (esPreparacion  [horno])
         (originarioDe  [italia])
         (dificultad  5)
         (nombre  "pizza_margherita")
         (precio  9.0)
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
    )

    ([risotto_alla_milanese] of Plato
         (compuestoPor  [arroz_arborio] [azafran] [caldo_de_carne] [cebolla] [mantequilla] [queso_parmesano])
         (esCategoria  [arroz])
         (esPreparacion  [sarten])
         (originarioDe  [italia])
         (dificultad  6)
         (nombre  "risotto_alla_milanese")
         (precio  12.5)
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

(defmodule RicoRico_Salida
    (import MAIN ?ALL)
    (export ?ALL)
)

(defrule MAIN::inicio
	(declare (salience 30))
	=> 
	(printout t "Test salidas en función de los menús disponibles" crlf)
	(focus RicoRico_Salida)
)


(deffunction RicoRico_Salida::crear_menus_hardcoded ()
     (printout t "Creando menús hardcoded..." crlf)
     (make-instance [menu1] of Menu
          (1rBebida [agua_mineral])
          (1rPlato [paella_valenciana])
          (2oBebida [vino_tinto_rioja])
          (2oPlato [tortilla_de_patatas])
          (postre [gazpacho_andaluz])
          (precio (+
    (send [agua_mineral]  get-precio)
    (send [paella_valenciana]  get-precio)
    (send [vino_tinto_rioja]  get-precio)
    (send [tortilla_de_patatas]  get-precio)
    (send [gazpacho_andaluz] get-precio)
))
     )
     (make-instance [menu2] of Menu
          (1rBebida [prosecco])
          (1rPlato [spaghetti_alla_carbonara])
          (2oBebida [chianti_clasico])
          (2oPlato [pizza_margherita])
          (postre [insalata_caprese])
          (precio (+
    (send [prosecco]  get-precio)
    (send [spaghetti_alla_carbonara]  get-precio)
    (send [chianti_clasico]  get-precio)
    (send [pizza_margherita]  get-precio)
    (send [insalata_caprese] get-precio)
))
     )
     (make-instance [menu3] of Menu
          (1rBebida [cerveza_sin_gluten])
          (1rPlato [arroz_caldoso_con_marisco])
          (2oBebida [vino_blanco_albarino])
          (2oPlato [ossobuco_alla_milanese])
          (postre [ensalada_campera])
          (precio (+
    (send [cerveza_sin_gluten]  get-precio)
    (send [arroz_caldoso_con_marisco]  get-precio)
    (send [vino_blanco_albarino]  get-precio)
    (send [ossobuco_alla_milanese]  get-precio)
    (send [ensalada_campera] get-precio)
))
     )
)

(deffunction RicoRico_Salida::print-menu (?m)
    (bind ?price    (send ?m get-precio))
    (bind ?b1-name  (send (send ?m get-1rBebida) get-nombre))
    (bind ?p1-name  (send (send ?m get-1rPlato)  get-nombre))
    (bind ?b2-name  (send (send ?m get-2oBebida) get-nombre))
    (bind ?p2-name  (send (send ?m get-2oPlato)  get-nombre))
    (bind ?pst-name (send (send ?m get-postre)   get-nombre))

    (if (eq ?b1-name ?b2-name) then
        (printout t
            "    Precio:        " ?price " euros" crlf
            "    Bebida:        " ?b1-name        crlf
            "    Primer plato:  " ?p1-name        crlf
            "    Segundo plato: " ?p2-name        crlf
            "    Postre:        " ?pst-name       crlf
                                                  crlf
        )
    else
        (printout t
            "    Precio:         " ?price " euros" crlf
            "    Primera bebida: " ?b1-name        crlf
            "    Primer plato:   " ?p1-name        crlf
            "    Segunda bebida: " ?b2-name        crlf
            "    Segundo plato:  " ?p2-name        crlf
            "    Postre:         " ?pst-name       crlf
                                                   crlf
        )
    )
)

(deffunction RicoRico_Salida::cmp-menu (?m1 ?m2)
    (<= (send ?m1 get-precio) (send ?m2 get-precio))
)

(defrule RicoRico_Salida::escrituraSalida
    (declare (salience 20))
    =>
    (make-instance prefs of Preferencias
        (num_comensales 40)
        (precio_min 5.24)
        (precio_max 20.84)
        (temporada otono)
        (alcoholica TRUE)
        (vino TRUE)
        (diferentesBebidas FALSE)
        (intolerancia_gluten FALSE)
        (intolerancia_lactosa FALSE)
    )
    (printout t "Procesando la salida ..." crlf crlf)
    (RicoRico_Salida::crear_menus_hardcoded)

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

    (bind ?menus (find-all-instances ((?m Menu)) TRUE))
    ; Mostrar menús según el número disponible
    (switch (length$ ?menus)
        (case 1 then
            (printout t "INFO: Con las restricciones actuales, solo se ha podido generar 1 menú." crlf crlf)
            (printout t "Menú:" crlf)
            (print-menu (nth 1 ?menus))
        )
        (case 2 then
            (bind ?m1 (nth 1 ?menus))
            (bind ?m2 (nth 2 ?menus))
            (bind ?ordered-menus (if (cmp-menu ?m1 ?m2) then (create$ ?m1 ?m2) else (create$ ?m2 ?m1)))

            (printout t "INFO: Con las restricciones actuales, solo se han podido generar 2 menús." crlf crlf)
            (printout t "Menú Barato:" crlf)
            (print-menu (nth 1 ?ordered-menus))
            (printout t "Menú Caro:" crlf)
            (print-menu (nth 2 ?ordered-menus))
        )
        (case 3 then
            (bind ?m1 (nth 1 ?menus))
            (bind ?m2 (nth 2 ?menus))
            (bind ?m3 (nth 3 ?menus))
            (bind ?ordered-menus (
                if       (and (cmp-menu ?m1 ?m2) (cmp-menu ?m2 ?m3)) then (create$ ?m1 ?m2 ?m3)
                else (if (and (cmp-menu ?m1 ?m3) (cmp-menu ?m3 ?m2)) then (create$ ?m1 ?m3 ?m2)
                else (if (and (cmp-menu ?m2 ?m1) (cmp-menu ?m1 ?m2)) then (create$ ?m2 ?m1 ?m3)
                else (if (and (cmp-menu ?m2 ?m3) (cmp-menu ?m3 ?m1)) then (create$ ?m2 ?m3 ?m1)
                else (if (and (cmp-menu ?m3 ?m1) (cmp-menu ?m1 ?m2)) then (create$ ?m3 ?m1 ?m2)
                else                                                 then (create$ ?m3 ?m2 ?m1)
            ))))))

            (printout t "INFO: Se han generado 3 menús." crlf crlf)
            (printout t "Menú Barato:" crlf)
            (print-menu (nth 1 ?ordered-menus))
            (printout t "Menú Estándar:" crlf)
            (print-menu (nth 2 ?ordered-menus))
            (printout t "Menú Caro:" crlf)
            (print-menu (nth 3 ?ordered-menus))
        )
        (default
            (printout t "INFO: No se ha podido generar ningún menú - Condiciones demasiado restrictivas." crlf)
        )
    )
)

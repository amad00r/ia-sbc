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
         (compuestoPor  [aceite_de_oliva] [calamar] [harina_de_trigo] [huevo] [limon])
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
     (declare (salience 20)) 
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
     (declare (salience 15))
     => 
     (printout t "Ahora vamos a hacerte una serie de preguntas para poder generar el menú en base a tus preferencias y restricciones." crlf crlf)
     (obtener_preferencias_restricciones)
     (printout t "Preferencias y restricciones recopiladas correctamente." crlf)
     (focus RicoRico_Generador)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; FILTRADO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; GENERADOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;TODO: CAMBIAR A FUNCION REAL; MOC!
(deffunction RicoRico_Generador::crear_menus_hardcoded ()
     (make-instance [menu1] of Menu
          (nombre "Menú Español Tradicional")
          (1rBebida [agua_mineral])
          (1rPlato [paella_valenciana])
          (2oBebida [vino_tinto_rioja])
          (2oPlato [tortilla_de_patatas])
          (postre [gazpacho_andaluz])
     )
     (make-instance [menu2] of Menu
          (nombre "Menú Italiano Clásico")
          (1rBebida [prosecco])
          (1rPlato [spaghetti_alla_carbonara])
          (2oBebida [chianti_clasico])
          (2oPlato [pizza_margherita])
          (postre [insalata_caprese])
     )
     (make-instance [menu3] of Menu
          (nombre "Menú Mar y Tierra")
          (1rBebida [cerveza_sin_gluten])
          (1rPlato [arroz_caldoso_con_marisco])
          (2oBebida [vino_blanco_albarino])
          (2oPlato [ossobuco_alla_milanese])
          (postre [ensalada_campera])
     )
) 

(defrule RicoRico_Generador::generador
     (declare (salience 10))
     => 
     (printout t "--> MOC - GENERANDO MENUS <--" crlf)
     (crear_menus_hardcoded)
     (printout t "--> MOC - SE HAN GENERADO CORRECTAMENTE <--" crlf)
     (focus RicoRico_Salida)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; SALIDA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffunction RicoRico_Salida::imprimir_preferencias_y_restricciones ()
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
)

(deffunction RicoRico_Salida::calcular_precio_menu (?menu)
     ;Función para calcular el precio del menú
     (bind ?precioTotal 0)
     (bind ?precioTotal (+ ?precioTotal (send (send ?menu get-1rBebida) get-precio)))
     (bind ?precioTotal (+ ?precioTotal (send (send ?menu get-1rPlato) get-precio)))
     (bind ?precioTotal (+ ?precioTotal (send (send ?menu get-2oBebida) get-precio)))
     (bind ?precioTotal (+ ?precioTotal (send (send ?menu get-2oPlato) get-precio)))
     (bind ?precioTotal (+ ?precioTotal (send (send ?menu get-postre) get-precio)))
     ?precioTotal
)

(deffunction RicoRico_Salida::imprimir_menu (?menu)
     ; Imprimir el menú con un formato amigable
     (printout t "   Menú: " (send ?menu get-nombre) crlf)
     (printout t "   Precio: " (calcular_precio_menu ?menu) "€" crlf)
     (printout t "       Primera bebida: " (send (send ?menu get-1rBebida) get-nombre) crlf)
     (printout t "       Primer plato: " (send (send ?menu get-1rPlato) get-nombre) crlf)
     (printout t "       Segunda bebida: " (send (send ?menu get-2oBebida) get-nombre) crlf)
     (printout t "       Segundo plato: " (send (send ?menu get-2oPlato) get-nombre) crlf)
     (printout t "       Postre: " (send (send ?menu get-postre) get-nombre) crlf crlf)
)

(deffunction RicoRico_Salida::procesar_salida ()
     ; Obtener todos los menús disponibles
     (bind ?menus (find-all-instances ((?menu Menu)) TRUE))

     ; Sacar el número de menús disponibles
     (bind ?numMenus (length$ ?menus))

     ; Mostrar las preferencias y restricciones del usuario
     (imprimir_preferencias_y_restricciones)

     ; Mostrar menús según el número disponible
     (switch ?numMenus
          (case 1 then
               (printout t "Con las restricciones actuales, solo se ha podido generar 1 menú." crlf)
               (printout t "Menú disponible:" crlf)
               (foreach ?menu ?menus
                    (RicoRico_Salida::imprimir_menu ?menu)
               )
          )
          (case 2 then
               (printout t "Con las restricciones actuales, solo se han podido generar 2 menús." crlf)
               (printout t "Menús disponibles:" crlf)
               (foreach ?menu ?menus
                    (RicoRico_Salida::imprimir_menu ?menu)
               )
          )
          (case 3 then
               (printout t "Se han generado los 3 menús." crlf)
               (printout t "Menús disponibles:" crlf)
               (foreach ?menu ?menus
                    (RicoRico_Salida::imprimir_menu ?menu)
               )
          )
          (default
               (printout t "No se ha podido generar ningún menú - Condiciones demasiado restrictivas." crlf)
          )
     )
)

(defrule RicoRico_Salida::escrituraSalida
     (declare (salience 5))
     =>
     (printout t "Procesando la salida..." crlf)
     (RicoRico_Salida::procesar_salida)
)

;;; ---------------------------------------------------------
;;; RicoRico_Ontology.clp
;;; Translated by owl2clips
;;; Translated to CLIPS from ontology ricorico.ttl
;;; :Date 15/05/2025 19:25:49

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
    ;;; Atributo de tipo String para indicar el nombre del elemento en cuestión.
    (slot nombre
        (type STRING)
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
    ([carne] of Categoria
         (nombre  "carne")
    )

    ([pescado] of Categoria
         (nombre  "pescado")
    )

    ([marisco] of Categoria
         (nombre  "marisco")
    )

    ([vegetariano] of Categoria
         (nombre  "vegetariano")
    )

    ([pasta] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "pasta")
    )

    ([arroz] of Categoria
         (nombre  "arroz")
    )

    ([huevo] of Categoria
         (nombre  "huevo")
    )

    ([horno] of Preparacion
         (nombre  "horno")
    )

    ([sarten] of Preparacion
         (nombre  "sarten")
    )

    ([hervido] of Preparacion
         (nombre  "hervido")
    )

    ([crudo] of Preparacion
         (nombre  "crudo")
    )

    ([frito] of Preparacion
         (nombre  "frito")
    )

    ([plancha] of Preparacion
         (nombre  "plancha")
    )

    ([primavera] of Temporada
         (nombre  "primavera")
    )

    ([verano] of Temporada
         (nombre  "verano")
    )

    ([otono] of Temporada
         (nombre  "otono")
    )

    ([invierno] of Temporada
         (nombre  "invierno")
    )

    ([espana] of Origen
         (nombre  "espana")
    )

    ([italia] of Origen
         (nombre  "italia")
    )

    ([tomate] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "tomate")
    )

    ([aceite_de_oliva] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "aceite_de_oliva")
    )

    ([ajo] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "ajo")
    )

    ([cebolla] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "cebolla")
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

    ([calabacin] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "calabacin")
    )

    ([berenjena] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "berenjena")
    )

    ([champinones] of Ingrediente
         (disponibleEn  [otono])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "champinones")
    )

    ([pollo] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pollo")
    )

    ([carne_de_res] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "carne_de_res")
    )

    ([chorizo] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "chorizo")
    )

    ([jamon_serrano] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "jamon_serrano")
    )

    ([huevos] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "huevos")
    )

    ([bacalao] of Ingrediente
         (disponibleEn  [primavera])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "bacalao")
    )

    ([merluza] of Ingrediente
         (disponibleEn  [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "merluza")
    )

    ([pulpo] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pulpo")
    )

    ([gambas] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "gambas")
    )

    ([mejillones] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "mejillones")
    )

    ([almejas] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "almejas")
    )

    ([arroz_blanco] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "arroz_blanco")
    )

    ([queso_parmesano] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "queso_parmesano")
    )

    ([mozzarella] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "mozzarella")
    )

    ([albahaca] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "albahaca")
    )

    ([oregano] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "oregano")
    )

    ([harina_de_trigo] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "harina_de_trigo")
    )

    ([pan] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "pan")
    )

    ([leche] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "leche")
    )

    ([nata] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "nata")
    )

    ([espinacas] of Ingrediente
         (disponibleEn  [primavera])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "espinacas")
    )

    ([judia_verde] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "judia_verde")
    )

    ([garrofo] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "garrofo")
    )

    ([vinagre] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "vinagre")
    )

    ([patata] of Ingrediente
         (disponibleEn  [otono] [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "patata")
    )

    ([pimenton] of Ingrediente
         (disponibleEn  [otono])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pimenton")
    )

    ([guindilla] of Ingrediente
         (disponibleEn  [primavera])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "guindilla")
    )

    ([fabes] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "fabes")
    )

    ([morcilla] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  FALSE)
         (lactosaFree  FALSE)
         (nombre  "morcilla")
    )

    ([aceitunas] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "aceitunas")
    )

    ([esparragos] of Ingrediente
         (disponibleEn  [primavera])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "esparragos")
    )

    ([guisantes] of Ingrediente
         (disponibleEn  [primavera])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "guisantes")
    )

    ([calamar] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "calamar")
    )

    ([limon] of Ingrediente
         (disponibleEn  [primavera])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "limon")
    )

    ([garbanzos] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "garbanzos")
    )

    ([mantequilla] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "mantequilla")
    )

    ([espaguetis] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "espaguetis")
    )

    ([panceta] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "panceta")
    )

    ([pimienta_negra] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pimienta_negra")
    )

    ([arroz_arborio] of Ingrediente
         (disponibleEn  [otono])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "arroz_arborio")
    )

    ([caldo_de_verduras] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "caldo_de_verduras")
    )

    ([laminas_de_lasana] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "laminas_de_lasana")
    )

    ([masa_de_pizza] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "masa_de_pizza")
    )

    ([gnocchi] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "gnocchi")
    )

    ([pesto] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "pesto")
    )

    ([osobuco_de_ternera] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "osobuco_de_ternera")
    )

    ([apio] of Ingrediente
         (disponibleEn  [otono])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "apio")
    )

    ([vino_blanco] of Ingrediente
         (disponibleEn  [primavera] [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "vino_blanco")
    )

    ([caldo_de_carne] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "caldo_de_carne")
    )

    ([polenta] of Ingrediente
         (disponibleEn  [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "polenta")
    )

    ([raviolis] of Ingrediente
         (disponibleEn  [otono])
         (glutenFree  FALSE)
         (lactosaFree  FALSE)
         (nombre  "raviolis")
    )

    ([ricotta] of Ingrediente
         (disponibleEn  [primavera])
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "ricotta")
    )

    ([azafran] of Ingrediente
         (disponibleEn  [otono])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "azafran")
    )

    ([trofie] of Ingrediente
         (disponibleEn  [primavera])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "trofie")
    )

    ([pepino] of Ingrediente
         (disponibleEn  [verano])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pepino")
    )

    ([sal_gruesa] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "sal_gruesa")
    )

    ([zanahoria] of Ingrediente
         (disponibleEn  [primavera] [verano] [otono] [invierno])
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "zanahoria")
    )

    ([vino_tinto_rioja] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "vino_tinto_rioja")
         (precio  12.5)
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

    ([chianti_clasico] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "chianti_clasico")
         (precio  14.0)
    )

    ([prosecco] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "prosecco")
         (precio  11.5)
    )

    ([lambrusco] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "lambrusco")
         (precio  8.5)
    )

    ([barolo] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "barolo")
         (precio  18.0)
    )

    ([agua_mineral] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "agua_mineral")
         (precio  1.5)
    )

    ([refresco_de_cola] of Casual
         (alcoholica  FALSE)
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "refresco_de_cola")
         (precio  2.0)
    )

    ([te_helado_de_limon] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "te_helado_de_limon")
         (precio  2.2)
    )

    ([zumo_de_naranja_natural] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "zumo_de_naranja_natural")
         (precio  2.8)
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

    ([leche_con_cacao] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "leche_con_cacao")
         (precio  2.5)
    )

    ([batido_de_fresa] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "batido_de_fresa")
         (precio  2.7)
    )

    ([paella_valenciana] of Plato
         (compuestoPor  [arroz_blanco] [pollo] [judia_verde] [garrofo] [aceite_de_oliva] [tomate] [ajo])
         (esCategoria  [arroz])
         (esPreparacion  [horno])
         (originarioDe  [espana])
         (dificultad  7)
         (nombre  "paella_valenciana")
         (precio  14.5)
    )

    ([gazpacho_andaluz] of Plato
         (compuestoPor  [tomate] [pimiento_verde] [pepino] [ajo] [aceite_de_oliva] [pan] [vinagre])
         (esCategoria  [vegetariano])
         (esPreparacion  [crudo])
         (originarioDe  [espana])
         (dificultad  3)
         (nombre  "gazpacho_andaluz")
         (precio  6.0)
    )

    ([tortilla_de_patatas] of Plato
         (compuestoPor  [huevos] [patata] [cebolla] [aceite_de_oliva])
         (esCategoria  [huevo])
         (esPreparacion  [sarten])
         (originarioDe  [espana])
         (dificultad  5)
         (nombre  "tortilla_de_patatas")
         (precio  7.5)
    )

    ([pulpo_a_la_gallega] of Plato
         (compuestoPor  [pulpo] [pimenton] [aceite_de_oliva] [sal_gruesa] [patata])
         (esCategoria  [marisco])
         (esPreparacion  [hervido])
         (originarioDe  [espana])
         (dificultad  6)
         (nombre  "pulpo_a_la_gallega")
         (precio  13.0)
    )

    ([bacalao_al_pil_pil] of Plato
         (compuestoPor  [bacalao] [ajo] [aceite_de_oliva] [guindilla])
         (esCategoria  [pescado])
         (esPreparacion  [sarten])
         (originarioDe  [espana])
         (dificultad  6)
         (nombre  "bacalao_al_pil_pil")
         (precio  12.0)
    )

    ([fabada_asturiana] of Plato
         (compuestoPor  [fabes] [chorizo] [morcilla] [jamon_serrano] [ajo] [cebolla] [aceite_de_oliva])
         (esCategoria  [carne])
         (esPreparacion  [hervido])
         (originarioDe  [espana])
         (dificultad  6)
         (nombre  "fabada_asturiana")
         (precio  11.0)
    )

    ([pimientos_rellenos] of Plato
         (compuestoPor  [pimiento_rojo] [carne_de_res] [cebolla] [tomate] [ajo] [aceite_de_oliva])
         (esCategoria  [carne])
         (esPreparacion  [horno])
         (originarioDe  [espana])
         (dificultad  5)
         (nombre  "pimientos_rellenos")
         (precio  9.0)
    )

    ([calamares_a_la_romana] of Plato
         (compuestoPor  [calamar] [harina_de_trigo] [huevo] [aceite_de_oliva] [limon])
         (esCategoria  [marisco])
         (esPreparacion  [frito])
         (originarioDe  [espana])
         (dificultad  4)
         (nombre  "calamares_a_la_romana")
         (precio  8.5)
    )

    ([ensalada_campera] of Plato
         (compuestoPor  [patata] [tomate] [pimiento_verde] [huevos] [aceitunas] [cebolla] [aceite_de_oliva])
         (esCategoria  [vegetariano])
         (esPreparacion  [hervido])
         (originarioDe  [espana])
         (dificultad  3)
         (nombre  "ensalada_campera")
         (precio  6.5)
    )

    ([merluza_a_la_koskera] of Plato
         (compuestoPor  [merluza] [esparragos] [almejas] [huevos] [guisantes] [ajo] [aceite_de_oliva])
         (esCategoria  [pescado])
         (esPreparacion  [hervido])
         (originarioDe  [espana])
         (dificultad  6)
         (nombre  "merluza_a_la_koskera")
         (precio  12.0)
    )

    ([escalivada] of Plato
         (compuestoPor  [pimiento_rojo] [berenjena] [cebolla] [aceite_de_oliva])
         (esCategoria  [vegetariano])
         (esPreparacion  [horno])
         (originarioDe  [espana])
         (dificultad  4)
         (nombre  "escalivada")
         (precio  7.0)
    )

    ([arroz_caldoso_con_marisco] of Plato
         (compuestoPor  [arroz_blanco] [gambas] [mejillones] [almejas] [ajo] [tomate] [pimiento_rojo] [aceite_de_oliva])
         (esCategoria  [arroz])
         (esPreparacion  [hervido])
         (originarioDe  [espana])
         (dificultad  7)
         (nombre  "arroz_caldoso_con_marisco")
         (precio  15.0)
    )

    ([huevos_rotos_con_jamon] of Plato
         (compuestoPor  [huevos] [jamon_serrano] [patata] [aceite_de_oliva])
         (esCategoria  [huevo])
         (esPreparacion  [sarten])
         (originarioDe  [espana])
         (dificultad  4)
         (nombre  "huevos_rotos_con_jamon")
         (precio  8.0)
    )

    ([espinacas_con_garbanzos] of Plato
         (compuestoPor  [espinacas] [garbanzos] [ajo] [pimenton] [aceite_de_oliva])
         (esCategoria  [vegetariano])
         (esPreparacion  [sarten])
         (originarioDe  [espana])
         (dificultad  4)
         (nombre  "espinacas_con_garbanzos")
         (precio  6.5)
    )

    ([croquetas_de_jamon] of Plato
         (compuestoPor  [jamon_serrano] [harina_de_trigo] [leche] [mantequilla] [aceite_de_oliva])
         (esCategoria  [carne])
         (esPreparacion  [frito])
         (originarioDe  [espana])
         (dificultad  6)
         (nombre  "croquetas_de_jamon")
         (precio  7.5)
    )

    ([spaghetti_alla_carbonara] of Plato
         (compuestoPor  [espaguetis] [huevos] [panceta] [queso_parmesano] [pimienta_negra])
         (esCategoria  [pasta])
         (esPreparacion  [sarten])
         (originarioDe  [italia])
         (dificultad  5)
         (nombre  "spaghetti_alla_carbonara")
         (precio  10.5)
    )

    ([risotto_ai_funghi] of Plato
         (compuestoPor  [arroz_arborio] [champinones] [queso_parmesano] [cebolla] [mantequilla] [caldo_de_verduras])
         (esCategoria  [arroz])
         (esPreparacion  [sarten])
         (originarioDe  [italia])
         (dificultad  6)
         (nombre  "risotto_ai_funghi")
         (precio  12.0)
    )

    ([lasagna_alla_bolognese] of Plato
         (compuestoPor  [laminas_de_lasana] [carne_de_res] [tomate] [queso_parmesano] [leche] [harina_de_trigo] [mantequilla])
         (esCategoria  [pasta])
         (esPreparacion  [horno])
         (originarioDe  [italia])
         (dificultad  7)
         (nombre  "lasagna_alla_bolognese")
         (precio  13.5)
    )

    ([pizza_margherita] of Plato
         (compuestoPor  [masa_de_pizza] [tomate] [mozzarella] [albahaca] [aceite_de_oliva])
         (esCategoria  [vegetariano])
         (esPreparacion  [horno])
         (originarioDe  [italia])
         (dificultad  5)
         (nombre  "pizza_margherita")
         (precio  9.0)
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

    ([melanzane_alla_parmigiana] of Plato
         (compuestoPor  [berenjena] [tomate] [mozzarella] [queso_parmesano] [albahaca] [aceite_de_oliva])
         (esCategoria  [vegetariano])
         (esPreparacion  [horno])
         (originarioDe  [italia])
         (dificultad  6)
         (nombre  "melanzane_alla_parmigiana")
         (precio  9.5)
    )

    ([ossobuco_alla_milanese] of Plato
         (compuestoPor  [osobuco_de_ternera] [zanahoria] [apio] [cebolla] [vino_blanco] [tomate] [caldo_de_carne])
         (esCategoria  [carne])
         (esPreparacion  [sarten])
         (originarioDe  [italia])
         (dificultad  7)
         (nombre  "ossobuco_alla_milanese")
         (precio  16.0)
    )

    ([trofie_al_pesto] of Plato
         (compuestoPor  [trofie] [pesto] [queso_parmesano])
         (esCategoria  [pasta])
         (esPreparacion  [hervido])
         (originarioDe  [italia])
         (dificultad  4)
         (nombre  "trofie_al_pesto")
         (precio  10.0)
    )

    ([polenta_con_funghi] of Plato
         (compuestoPor  [polenta] [champinones] [queso_parmesano] [aceite_de_oliva])
         (esCategoria  [vegetariano])
         (esPreparacion  [hervido])
         (originarioDe  [italia])
         (dificultad  4)
         (nombre  "polenta_con_funghi")
         (precio  8.5)
    )

    ([ravioli_di_ricotta_e_spinaci] of Plato
         (compuestoPor  [raviolis] [ricotta] [espinacas] [queso_parmesano] [mantequilla])
         (esCategoria  [pasta])
         (esPreparacion  [hervido])
         (originarioDe  [italia])
         (dificultad  6)
         (nombre  "ravioli_di_ricotta_e_spinaci")
         (precio  11.0)
    )

    ([pasta_al_pomodoro] of Plato
         (compuestoPor  [pasta] [tomate] [albahaca] [aceite_de_oliva] [queso_parmesano])
         (esCategoria  [pasta])
         (esPreparacion  [hervido])
         (originarioDe  [italia])
         (dificultad  3)
         (nombre  "pasta_al_pomodoro")
         (precio  8.0)
    )

    ([risotto_alla_milanese] of Plato
         (compuestoPor  [arroz_arborio] [azafran] [cebolla] [mantequilla] [caldo_de_carne] [queso_parmesano])
         (esCategoria  [arroz])
         (esPreparacion  [sarten])
         (originarioDe  [italia])
         (dificultad  6)
         (nombre  "risotto_alla_milanese")
         (precio  12.5)
    )

    ([frittata_di_zucchine] of Plato
         (compuestoPor  [huevos] [calabacin] [queso_parmesano] [cebolla] [aceite_de_oliva])
         (esCategoria  [huevo])
         (esPreparacion  [sarten])
         (originarioDe  [italia])
         (dificultad  4)
         (nombre  "frittata_di_zucchine")
         (precio  7.5)
    )

    ([pasta_alla_norma] of Plato
         (compuestoPor  [pasta] [berenjena] [tomate] [ricotta] [albahaca] [aceite_de_oliva])
         (esCategoria  [pasta])
         (esPreparacion  [hervido])
         (originarioDe  [italia])
         (dificultad  5)
         (nombre  "pasta_alla_norma")
         (precio  9.0)
    )

    ([insalata_caprese] of Plato
         (compuestoPor  [tomate] [mozzarella] [albahaca] [aceite_de_oliva])
         (esCategoria  [vegetariano])
         (esPreparacion  [crudo])
         (originarioDe  [italia])
         (dificultad  2)
         (nombre  "insalata_caprese")
         (precio  6.0)
    )
)

;; Para almacenar las preferencias y restricciones del usuario.
(deftemplate preferencias
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

(defmodule MAIN (export ?ALL))


(defmodule RicoRico_Filtrado
    (import MAIN ?ALL)
    (export ?ALL)
)

(defrule MAIN::inicio 
    (declare (salience 20)) 
    => 
    (printout t "Bienvenido al sistema de filtrado." crlf)
    (focus RicoRico_Filtrado)
)

(defrule RicoRico_Filtrado::eliminar_platos_complejos
    (preferencias (num_comensales ?comensales))
    ?plato <- (object (is-a Plato)
                      (dificultad ?dificultad))
    (test (> ?dificultad ?comensales))
    =>
    (send ?plato delete)
)

(defrule RicoRico_Filtrado::eliminar_platos_fuera_temporada
    (preferencias (temporada ?temporada))
    ?plato <- (object (is-a Plato) (compuestoPor $?ingredienteList))
    ?ingrediente <- (object (is-a Ingrediente) (disponibleEn $?disponibles))
    (test (member$ ?ingrediente ?ingredienteList))
    (test (not (member$ ?temporada ?disponibles)))
    =>
    (send ?plato delete)
)

(defrule RicoRico_Filtrado::eliminar_bebidas_alcoholicas
    (preferencias (alcoholica FALSE))
    ?bebida <- (object (is-a Bebida) (alcoholica TRUE))
    =>
    (send ?bebida delete)
)

(defrule RicoRico_Filtrado::eliminar_vinos
    (preferencias (vino FALSE))
    ?vino <- (object (is-a Vino))
    =>
    (send ?vino delete)
)

(defrule RicoRico_Filtrado::eliminar_platos_lactosa
    (preferencias (intolerancia_lactosa TRUE))
    ?plato <- (object (is-a Plato) (compuestoPor $?ingredienteList))
    ?ingrediente <- (object (is-a Ingrediente) (lactosaFree FALSE))
    (test (member$ ?ingrediente ?ingredienteList))
    =>
    (send ?plato delete)
)

(defrule RicoRico_Filtrado::eliminar_platos_gluten
    (preferencias (intolerancia_gluten TRUE))
    ?plato <- (object (is-a Plato) (compuestoPor $?ingredienteList))
    ?ingrediente <- (object (is-a Ingrediente) (glutenFree FALSE))
    (test (member$ ?ingrediente ?ingredienteList))
    =>
    (send ?plato delete)
)

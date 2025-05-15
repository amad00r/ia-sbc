;; PARA TESTEAR EL FILTRO DE FORMA INDEPENDIENTE
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
    ?p <- (preferencias (num_comensales ?comensales))
    ?plato <- (object (is-a Plato)
                      (dificultad ?dificultad))

    (test (> ?dificultad ?comensales))

    =>

    (send ?plato delete)
)

(defrule RicoRico_Filtrado::eliminar_platos_fuera_temporada
    ?p <- (preferencias (temporada ?temporada))
    ?plato <- (object (is-a Plato)
                      (compuestoPor $? ?ingrediente $?))
    ?ingrediente <- (object (is-a Ingrediente)
                            (disponibleEn $?disponibles))

    (test (not (member$ ?temporada ?disponibles)))

    =>

    (send ?plato delete)
)

(defrule RicoRico_Filtrado::eliminar_bebidas_alcoholicas
    ?alcoholicas <- (preferencias (alcoholica FALSE))
    ?bebida <- (object (is-a Bebida)
                       (alcoholica TRUE))

    =>

    (send ?bebida delete)
)

(defrule RicoRico_Filtrado::eliminar_vinos
    ?vinos <- (preferencias (vino FALSE))
    ?vino <- (object (is-a Vino))

    =>

    (send ?vino delete)
)

(defrule RicoRico_Filtrado::eliminar_platos_lactosa
    ?int_lacteos <- (preferencias (intolerancia_lactosa TRUE))
    ?plato <- (object (is-a Plato)
                      (compuestoPor $? ?ingrediente $?))
    ?ingrediente <- (object (is-a Ingrediente)
                            (lactosaFree FALSE))

    =>

    (send ?plato delete)
)

(defrule RicoRico_Filtrado::eliminar_platos_gluten
    ?int_gluten <- (preferencias (intolerancia_gluten TRUE))
    ?plato <- (object (is-a Plato)
                      (compuestoPor $? ?ingrediente $?))
    ?ingrediente <- (object (is-a Ingrediente)
                            (glutenFree FALSE))

    =>

    (send ?plato delete)
)

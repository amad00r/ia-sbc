;;; ---------------------------------------------------------
;;; RicoRico_Ontology.clp
;;; Translated by owl2clips
;;; Translated to CLIPS from ontology ricorico.ttl
;;; :Date 11/05/2025 19:54:09

(defclass Bebida "Clase para representar una bebida."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Atributo de tipo Booleano para indicar si es elemento es libre de gluten.
    (multislot glutenFree
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Atributo de tipo Booleano para indicar si el elemento es libre de lactosa.
    (multislot lactosaFree
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
    ;;; Atributo de tipo Booleano para indicar si una bebida casual es alcohólica o no.
    (slot alcoholica
        (type SYMBOL)
        (create-accessor read-write))
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
    ;;; Atributo de tipo Booleano para indicar si un menú ha sido generado o no.
    (slot generado
        (type SYMBOL)
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
    ;;; Atributo de tipo Booleano para indicar si es elemento es libre de gluten.
    (multislot glutenFree
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Atributo de tipo Booleano para indicar si el elemento es libre de lactosa.
    (multislot lactosaFree
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
    ([mocBebida1] of Vino
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "vinoMoc")
         (precio  10.5)
    )

    ([mocBebida2] of Casual
         (alcoholica  TRUE)
         (glutenFree  FALSE)
         (lactosaFree  FALSE)
         (nombre  "cocacolaMoc")
         (precio  1.2)
    )

    ([mocCategoria] of Categoria
         (nombre  "VegetarianoMoc")
    )

    ([mocIngrediente1] of Ingrediente
         (disponibleEn  [mocTemporada] [mocTemporada2])
         (nombre  "ArrozMoc")
    )

    ([mocIngrediente2] of Ingrediente
         (disponibleEn  [mocTemporada])
         (nombre  "LechugaMoc")
    )

    ([mocMenu1] of Menu
         (1rBebida  [mocBebida1])
         (1rPlato  [mocPlato1])
         (2oBebida  [mocBebida2])
         (2oPlato  [mocPlato2])
         (postre  [mocPostre])
         (generado  FALSE)
         (nombre  "Opción 1")
    )

    ([mocMenu2] of Menu
         (1rBebida  [mocBebida1])
         (1rPlato  [mocPlato1])
         (2oBebida  [mocBebida2])
         (2oPlato  [mocPlato2])
         (postre  [mocPostre])
         (generado  FALSE)
         (nombre  "Opción 2")
    )

    ([mocMenu3] of Menu
         (1rBebida  [mocBebida1])
         (1rPlato  [mocPlato1])
         (2oBebida  [mocBebida2])
         (2oPlato  [mocPlato2])
         (postre  [mocPostre])
         (generado  FALSE)
         (nombre  "Opción 3")
    )

    ([mocOrigen] of Origen
         (nombre  "EspañaMoc")
    )

    ([mocPlato1] of Plato
         (compuestoPor  [mocIngrediente1] [mocIngrediente2])
         (esCategoria  [mocCategoria])
         (esPreparacion  [mocPreparacion])
         (incompatibleConBebida  [mocBebida1] [mocBebida2])
         (incompatibleConPlato  [mocPlato2] [mocPostre])
         (originarioDe  [mocOrigen])
         (dificultad  7)
         (nombre  "Plato1Moc")
         (precio  7.23)
         (tipo  1)
    )

    ([mocPlato2] of Plato
         (compuestoPor  [mocIngrediente2])
         (esCategoria  [mocCategoria])
         (esPreparacion  [mocPreparacion])
         (originarioDe  [mocOrigen])
         (dificultad  1)
         (nombre  "Plato2Moc")
         (precio  2.56)
         (tipo  2)
    )

    ([mocPostre] of Plato
         (compuestoPor  [mocIngrediente1])
         (esCategoria  [mocCategoria])
         (esPreparacion  [mocPreparacion])
         (incompatibleConBebida  [mocBebida1])
         (incompatibleConPlato  [mocPlato1])
         (originarioDe  [mocOrigen])
         (dificultad  4)
         (nombre  "PostreMoc")
         (precio  8.2)
         (tipo  0)
    )

    ([mocPreparacion] of Preparacion
         (nombre  "mocPreparacion")
    )

    ([mocTemporada] of Temporada
         (nombre  "VeranoMoc")
    )

    ([mocTemporada2] of Temporada
         (nombre  "PrimaveraMoc")
    )

)

(defmodule MAIN 
    (export ?ALL)
)

(defmodule RicoRico_Salida
    (import MAIN ?ALL)
    (export ?ALL)
)

(defrule MAIN::inicio 
	(declare (salience 20)) 
	=> 
	(printout t "Test salidas en función de los menús disponibles" crlf)
	(focus RicoRico_Salida)
)

(deffunction RicoRico_Salida::procesar_salida ()
    ; Obtener todos los menús disponibles
    (bind ?menus (find-all-instances ((?menu Menu)) (eq (send ?menu get-generado) TRUE)))

    ; Sacar el número de menús disponibles
    (bind ?numMenus (length$ ?menus))

    ; Mostrar menús según el número disponible
    (switch ?numMenus
        (case 1 then
            (printout t "Con las restricciones actuales, solo se ha podido generar 1 menú." crlf)
            (printout t "Menú disponible:" crlf)
            (foreach ?menu ?menus
                (printout t "   Menú: " (send ?menu get-nombre) crlf)
                (printout t "       Primera bebida: " (send (send ?menu get-1rBebida) get-nombre) crlf)
                (printout t "       Primer plato: " (send (send ?menu get-1rPlato) get-nombre) crlf)
                (printout t "       Segunda bebida: " (send (send ?menu get-2oBebida) get-nombre) crlf)
                (printout t "       Segundo plato: " (send (send ?menu get-2oPlato) get-nombre) crlf)
                (printout t "       Postre: " (send (send ?menu get-postre) get-nombre) crlf crlf)
            )
        )
        (case 2 then
            (printout t "Con las restricciones actuales, solo se han podido generar 2 menús." crlf)
            (printout t "Menús disponibles:" crlf)
            (foreach ?menu ?menus
                (printout t "   Menú: " (send ?menu get-nombre) crlf)
                (printout t "       Primera bebida: " (send (send ?menu get-1rBebida) get-nombre) crlf)
                (printout t "       Primer plato: " (send (send ?menu get-1rPlato) get-nombre) crlf)
                (printout t "       Segunda bebida: " (send (send ?menu get-2oBebida) get-nombre) crlf)
                (printout t "       Segundo plato: " (send (send ?menu get-2oPlato) get-nombre) crlf)
                (printout t "       Postre: " (send (send ?menu get-postre) get-nombre) crlf crlf)
            )
        )
        (case 3 then
            (printout t "Se han generado los 3 menús." crlf)
            (printout t "Menús disponibles:" crlf)
            (foreach ?menu ?menus
                (printout t "   Menú: " (send ?menu get-nombre) crlf)
                (printout t "       Primera bebida: " (send (send ?menu get-1rBebida) get-nombre) crlf)
                (printout t "       Primer plato: " (send (send ?menu get-1rPlato) get-nombre) crlf)
                (printout t "       Segunda bebida: " (send (send ?menu get-2oBebida) get-nombre) crlf)
                (printout t "       Segundo plato: " (send (send ?menu get-2oPlato) get-nombre) crlf)
                (printout t "       Postre: " (send (send ?menu get-postre) get-nombre) crlf crlf)
            )
        )
        (default
            (printout t "No se ha podido generar ningún menú - Condiciones demasiado restrictivas." crlf)
        )
    )
)

(defrule RicoRico_Salida::escrituraSalida
    (declare (salience 10))
    =>
    (printout t "Procesando la salida ..." crlf crlf)
    (RicoRico_Salida::procesar_salida)
)
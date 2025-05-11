;;; ---------------------------------------------------------
;;; RicoRico_Ontology.clp
;;; Translated by owl2clips
;;; Translated to CLIPS from ontology ricorico.ttl
;;; :Date 05/05/2025 19:43:45

;;; ---------------------------------------------------------
;;; RicoRico_Ontology.clp
;;; Translated by owl2clips
;;; Translated to CLIPS from ontology ricorico.ttl
;;; :Date 11/05/2025 12:24:40

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

    ([mocMenu] of Menu
         (1rBebida  [mocBebida1])
         (1rPlato  [mocPlato1])
         (2oBebida  [mocBebida2])
         (2oPlato  [mocPlato2])
         (postre  [mocPostre])
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


(defmodule MAIN (export ?ALL))

(defrule MAIN::inicio 
    (declare (salience 20)) 
    => 
    (printout t "Bienvenido al creador de menús para el catering RicoRico." crlf)
    (focus RicoRico_Entrada)
)

(defmodule RicoRico_Entrada (export ?ALL) (import MAIN ?ALL))

(deffunction RicoRico_Entrada::obtener_numero_comensales ()
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

(deffunction RicoRico_Entrada::obtener_vino () 
    (bind ?vino (seleccion_una_opcion "Quieres vino con el menú? " Si No))
    (if (eq ?vino Si) then
        (return (create$ TRUE TRUE))
    else
        (printout t crlf)
        (bind ?alcoholica (seleccion_una_opcion "Quieres que el menú tenga bebidas alcoholicas? " Si No))
        (return (create$ FALSE (if (eq ?alcoholica Si) then TRUE else FALSE)))
    )
)

(deffunction RicoRico_Entrada::obtener_nombres_ingredientes ()
    (bind $?ingredientes (find-all-instances ((?i Ingrediente)) TRUE))
    (bind $?nombres (create$))
    (do-for-all-instances ((?ing Ingrediente)) TRUE
        (bind $?nombres (insert$ $?nombres (+ (length$ $?nombres) 1) (send ?ing get-nombre)))
    )
    (return $?nombres)
)

(deffunction RicoRico_Entrada::obtener_ingredientes_prohibidos ($?lista_ingredientes)
    (printout t "¿Quieres prohibir algun ingrediente?" crlf)
    (printout t "Estos son los ingredientes disponibles: " $?lista_ingredientes crlf)
    (printout t "Introduzca el ingrediente que quieres prohibir o FIN para terminar: " crlf)
    (bind ?respuesta (str-cat (read)))
    (bind $?lista_prohibidos (create$))
    (while (not (eq ?respuesta "FIN")) do
        (if (and (member$ ?respuesta $?lista_ingredientes)(not(member$ ?respuesta $?lista_prohibidos))) then
            (bind $?lista_prohibidos (insert$ $?lista_prohibidos (+ (length$ $?lista_prohibidos) 1) ?respuesta))
        )
        (bind ?respuesta (str-cat (read)))
    )
    (printout t crlf)
    (return $?lista_prohibidos)
)

(deffunction RicoRico_Entrada::obtener_preferencias_restricciones ()
    (bind ?num_comensales (obtener_numero_comensales))
    (bind ?precio_min (obtener_precio_min))
    (bind ?precio_max (obtener_precio_max ?precio_min))
    (bind ?temporada (seleccion_una_opcion "Introduzca la temporada del año." Invierno Primavera Otono Verano))

    ;;Obtener si quiere vino o bebida alcoholica
    (bind $?vino_alcoholica (obtener_vino))
    (bind ?vino (nth$ 1 $?vino_alcoholica))
    (bind ?alcoholica (nth$ 2 $?vino_alcoholica))
    
    ;; Crear una lista con todos los ingredientes + prohibir los que quiera el usuario
    (bind $?lista_ingredientes (obtener_nombres_ingredientes))
    (bind $?lista_prohibidos (obtener_ingredientes_prohibidos $?lista_ingredientes))
    (printout t "Se han prohibido los siguientes ingredientes: ")
    (printout t $?lista_prohibidos crlf)

    ;; Devolvemos los datos recopilados
    (return (create$ ?num_comensales ?precio_min ?precio_max ?temporada ?vino ?alcoholica $?lista_prohibidos))

)

(defrule RicoRico_Entrada::intanciacion_preferencias_restricciones
	(declare (salience 10))
	=> 
	(printout t "Ahora vamos a hacerte una serie de preguntas para poder generate el menú en base a tus preferencias y restricciones." crlf crlf)
	(obtener_preferencias_restricciones)
    (printout t "Preferencias y restricciones recopiladas correctamente." crlf)
)
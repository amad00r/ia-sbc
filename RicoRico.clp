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
    ([mocBebida1] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "vinoMoc")
         (precio  10.5)
    )

    ([mocBebida2] of Casual
         (alcoholica  FALSE)
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
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "ArrozMoc")
    )

    ([mocIngrediente2] of Ingrediente
         (disponibleEn  [mocTemporada])
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "LechugaMoc")
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

(defrule MAIN::inicio 
    (declare (salience 20)) 
    => 
    (printout t "Bienvenido al creador de menús para el catering RicoRico." crlf)
    (focus RicoRico_Entrada)
)

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
    (bind ?alcoholica (seleccion_una_opcion "Quieres que el menú tenga bebidas alcoholicas? " Si No))
    (bind ?alcoholica (if (eq ?alcoholica Si) then TRUE else FALSE))

    (if (eq ?alcoholica TRUE) then 
        (bind ?vino (seleccion_una_opcion "Quieres vino con el menú? " Si No)) 
        (if (eq ?vino Si) then (bind ?vino TRUE) else (bind ?vino FALSE))
    else (bind ?vino FALSE))

    (bind ?diferentesBebidas (seleccion_una_opcion "Quieres una bebida diferente para el primer plato y el segundo? " Si No))
    (if (eq ?diferentesBebidas Si) then (bind ?diferentesBebidas TRUE) else (bind ?diferentesBebidas FALSE))

    (return (create$ ?alcoholica ?vino ?diferentesBebidas))
)

(deffunction RicoRico_Entrada::obtener_intolerancias ()
    ;;; Función para obtener las intolerancias alimentarias.
    (bind ?alguna_intolerancia (seleccion_una_opcion "¿Hay alguien con intolerancia alimentaria?" Si No))
    (bind ?intolerancia_gluten FALSE)
    (bind ?intolerancia_lactosa FALSE)

    (if (eq ?alguna_intolerancia Si) then 
        (bind ?intolerancia_gluten (seleccion_una_opcion "¿Es intolerante al gluten?" Si No))
        (if (eq ?intolerancia_gluten Si) then (bind ?intolerancia_gluten TRUE) else (bind ?intolerancia_gluten FALSE))

        (bind ?intolerancia_lactosa (seleccion_una_opcion "¿Es intolerante a la lactosa?" Si No))
        (if (eq ?intolerancia_lactosa Si) then (bind ?intolerancia_lactosa TRUE) else (bind ?intolerancia_lactosa FALSE))
    )

    (return (create$ ?intolerancia_gluten ?intolerancia_lactosa))
)

(deffunction RicoRico_Entrada::obtener_preferencias_restricciones ()
    ;;Obtenemos el número de comensales, el precio mínimo y máximo y la temporada
    (bind ?num_comensales (obtener_numero_comensales))
    (bind ?precio_min (obtener_precio_min))
    (bind ?precio_max (obtener_precio_max ?precio_min))
    (bind ?temporada (seleccion_una_opcion "Introduzca la temporada del año." Invierno Primavera Otono Verano))

    ;; Obtenemos las preferencias y restricciones de bebida
    (bind $?condiciones_bebida (obtener_bebida))
    (bind ?alcoholica (nth$ 1 $?condiciones_bebida))
    (bind ?vino (nth$ 2 $?condiciones_bebida))
    (bind ?diferentesBebidas (nth$ 3 $?condiciones_bebida))
    
    ;; Obtenemos las intolerancias alimentarias
    (bind $?lista_intolerancias (obtener_intolerancias))
    (bind ?intolerancia_gluten (nth$ 1 $?lista_intolerancias))
    (bind ?intolerancia_lactosa (nth$ 2 $?lista_intolerancias))

    (assert (preferencias
        (num_comensales ?num_comensales)
        (precio_min ?precio_min)
        (precio_max ?precio_max)
        (temporada ?temporada)
        (alcoholica ?alcoholica)
        (vino ?vino)
        (diferentesBebidas ?diferentesBebidas)
        (intolerancia_gluten ?intolerancia_gluten)
        (intolerancia_lactosa ?intolerancia_lactosa)
    ))
)

(defrule RicoRico_Entrada::instanciacion_preferencias_restricciones
	(declare (salience 15))
	=> 
	(printout t "Ahora vamos a hacerte una serie de preguntas para poder generar el menú en base a tus preferencias y restricciones." crlf crlf)
	(obtener_preferencias_restricciones)
    (printout t "Preferencias y restricciones recopiladas correctamente." crlf)
    (focus RicoRico_Generador)
)

;;;TODO: CAMBIAR A FUNCION REAL; MOC!
(deffunction RicoRico_Generador::crear_menus_hardcoded ()
    (make-instance [mocMenu1] of Menu
        (nombre "Menú1Moc")
        (1rBebida [mocBebida1])
        (1rPlato [mocPlato1])
        (2oBebida [mocBebida2])
        (2oPlato [mocPlato2])
        (postre [mocPostre])
    )
    (make-instance [mocMenu2] of Menu
        (nombre "Menú2Moc")
        (1rBebida [mocBebida2])
        (1rPlato [mocPlato2])
        (2oBebida [mocBebida1])
        (2oPlato [mocPlato1])
        (postre [mocPostre])
    )
    (make-instance [mocMenu3] of Menu
        (nombre "Menú3Moc")
        (1rBebida [mocBebida1])
        (1rPlato [mocPlato2])
        (2oBebida [mocBebida2])
        (2oPlato [mocPlato1])
        (postre [mocPostre])
    )
)

(defrule RicoRico_Generador::instanciacion_preferencias_restricciones
	(declare (salience 10))
	=> 
	(printout t "--> MOC - GENERANDO MENUS <--" crlf)
	(crear_menus_hardcoded)
    (printout t "--> MOC - SE HAN GENERADO CORRECTAMENTE <--" crlf)
    (focus RicoRico_Salida)
)

(deffunction RicoRico_Salida::imprimir_menu (?menu)
    ; Imprimir el menú con un formato amigable
    (printout t "   Menú: " (send ?menu get-nombre) crlf)
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

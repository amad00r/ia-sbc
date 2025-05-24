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
     (bind ?tipo_evento (seleccion_una_opcion "¿Qué tipo de evento es?" casual formal))
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
          (tipo_evento ?tipo_evento)
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
     => 
     (printout t "Ahora vamos a hacerte una serie de preguntas para poder generar el menú en base a tus preferencias y restricciones." crlf crlf)
     (obtener_preferencias_restricciones)
     (printout t "Preferencias y restricciones recopiladas correctamente." crlf)

     (focus RicoRico_Filtrado)
)

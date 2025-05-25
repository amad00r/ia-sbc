(deffunction RicoRico_Entrada::calcular_precio_maximo (?instancias)
     ;;; Funcion para calcula el precio máximo de una lista de instancias
     (bind ?max 0.0)
     (foreach ?inst ?instancias
          (if (> (send ?inst get-precio) ?max) then
          (bind ?max (send ?inst get-precio))
          )
     )
     ?max
)

(deffunction RicoRico_Entrada::calcular_precio_minimo (?instancias)
     ;;; Funcion para calcula el precio mínimo de una lista de instancias
     (bind ?min 999999.0)
     (foreach ?inst ?instancias
          (if (< (send ?inst get-precio) ?min) then
          (bind ?min (send ?inst get-precio))
          )
     )
     ?min
)

(deffunction RicoRico_Entrada::obtener_platos_primero ()
     ;;; Función para obtener todos los primeros platos del sistema.
     (bind ?lista (create$))
     (do-for-all-instances ((?p Plato)) TRUE
          (if (or (eq (send ?p get-tipo) 1) (eq (send ?p get-tipo) 3)) then
          (bind ?lista (insert$ ?lista (+ (length$ ?lista) 1) ?p))
          )
     )
     ?lista
)

(deffunction RicoRico_Entrada::obtener_platos_segundo ()
     ;;; Función para obtener todos los segundos platos del sistema.
     (bind ?lista (create$))
     (do-for-all-instances ((?p Plato)) TRUE
          (if (eq (send ?p get-tipo) 2) then
          (bind ?lista (insert$ ?lista (+ (length$ ?lista) 1) ?p))
          )
     )
     ?lista
)

(deffunction RicoRico_Entrada::obtener_platos_postre ()
     ;;; Función para obtener todos los postres del sistema
     (bind ?lista (create$))
     (do-for-all-instances ((?p Plato)) TRUE
          (if (eq (send ?p get-tipo) 0) then
          (bind ?lista (insert$ ?lista (+ (length$ ?lista) 1) ?p))
          )
     )
     ?lista
)

(deffunction RicoRico_Entrada::obtener_todas_bebidas ()
     ;;; Función para obtener todas las bebidas del sistema.
     (bind ?lista (create$))

     (do-for-all-instances ((?v Vino)) TRUE
          (bind ?lista (insert$ ?lista (+ (length$ ?lista) 1) ?v))
     )

     (do-for-all-instances ((?b Casual)) TRUE
          (bind ?lista (insert$ ?lista (+ (length$ ?lista) 1) ?b))
     )
     ?lista
)

(deffunction RicoRico_Entrada::calcular_precios_menu_min_max ()
     ;; Función para calcular el precio mínimo y máximo del menú.
    
     (bind ?platos_primero (obtener_platos_primero))
     (bind ?platos_segundo (obtener_platos_segundo))
     (bind ?platos_postre (obtener_platos_postre))
     (bind ?todas_bebidas (obtener_todas_bebidas))

     (bind ?precio_min_primero (calcular_precio_minimo ?platos_primero))
     (bind ?precio_min_segundo (calcular_precio_minimo ?platos_segundo))
     (bind ?precio_min_postre (calcular_precio_minimo ?platos_postre))
     (bind ?precio_min_bebida (calcular_precio_minimo ?todas_bebidas))
     
     (bind ?precio_max_primero (calcular_precio_maximo ?platos_primero))
     (bind ?precio_max_segundo (calcular_precio_maximo ?platos_segundo))
     (bind ?precio_max_postre (calcular_precio_maximo ?platos_postre))
     (bind ?precio_max_bebida (calcular_precio_maximo ?todas_bebidas))
     
     (bind ?precio_total_min (+ 
          ?precio_min_primero
          ?precio_min_segundo
          ?precio_min_postre
          ?precio_min_bebida
     ))
     
     (bind ?precio_2_bebidas_max (* 2 ?precio_max_bebida))
     (bind ?precio_total_max (+ 
          ?precio_max_primero
          ?precio_max_segundo
          ?precio_max_postre
          ?precio_2_bebidas_max
     ))
     
     (return (create$ ?precio_total_min ?precio_total_max))
)

(deffunction RicoRico_Entrada::obtener_numero_comensales ()
     ;;; Función para obtener el número de comensales.
     (printout t "Introduce el número de comensales: ")
     (bind ?num_comensales (read))
     
     ;; Validar que sea un entero positivo
     (while (or (not (integerp ?num_comensales)) (<= ?num_comensales 0)) do
          (if (not (integerp ?num_comensales))
               then (printout t "Por favor, introduce un número entero válido: ")
               else (printout t "Por favor, introduce un número de comensales mayor que 0: ")
          )
          (bind ?num_comensales (read))
     )
     
     (printout t crlf)
     (return ?num_comensales)
)

(deffunction RicoRico_Entrada::obtener_precio_min ()
     ;;; Función para obtener el precio mínimo del menú que el usuario esta dispuesto a pagar.
     (printout t "Introduce el precio mínimo del menú: ")
     (bind ?precio_min (read))
     
     ;; Validar que sea un número y que sea mayor que 0
     (while (or (not (numberp ?precio_min)) (<= ?precio_min 0.0)) do
          (if (not (numberp ?precio_min))
               then (printout t "Por favor, introduce un número válido: ")
               else (printout t "Por favor, introduce un precio válido (mayor que 0): ")
          )
          (bind ?precio_min (read))
     )
     
     (printout t crlf)
     (return ?precio_min)
)

(deffunction RicoRico_Entrada::obtener_precio_max (?precio_min)
     ;;; Función para obtener el precio máximo del menú que el usuario está dispuesto a pagar.
     (printout t "Introduce el precio máximo del menú: ")
     (bind ?precio_max (read))
     
     ;; Validar que sea un número, mayor que 0, mayor que precio_min y con diferencia mínima de 5€
     (while (or (not (numberp ?precio_max)) (<= ?precio_max 0.0) (<= ?precio_max ?precio_min) (< ?precio_max (+ ?precio_min 5))) do
          (if (not (numberp ?precio_max))
               then (printout t "Por favor, introduce un número válido: ")
          else (if (<= ?precio_max 0)
                    then (printout t "Por favor, introduce un precio válido (mayor que 0): ")
               else (if (<= ?precio_max ?precio_min)
                         then (printout t "El precio máximo debe ser superior al precio mínimo. Por favor, introduce un precio válido: ")
                         else (printout t "Rango de precios muy pequeño. Introduce un nuevo precio máximo con una diferencia de 5€ mínimo: ")
                    )
               )
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
     (bind ?precios_menu_min_max (calcular_precios_menu_min_max))
     (bind ?precio_menu_min(nth$ 1 ?precios_menu_min_max))
     (bind ?precio_menu_max (nth$ 2 ?precios_menu_min_max))

     ;;Obtenemos el número de comensales, el precio mínimo y máximo y la temporada
     (bind ?tipo_evento (seleccion_una_opcion "¿Qué tipo de evento es?" casual formal))
     (bind ?num_comensales (obtener_numero_comensales))
     (bind ?precio_min (obtener_precio_min))

     ;; Ajustar precio mínimo al precio mínimo de un menú en el sistema.
     (if (< ?precio_min ?precio_menu_min) then
          (printout t "ADVERTENCIA: Ajustando el precio mínimo del menú a " ?precio_menu_min "€. Es el precio mínimo disponible en el sistema." crlf crlf)
          (bind ?precio_min ?precio_menu_min)
     )

     (bind ?precio_max (obtener_precio_max ?precio_min))

     ;; Ajustar precio máximo al precio máximo de un menú en el sistema.
     (if (> ?precio_max ?precio_menu_max) then
          (printout t "ADVERTENCIA: Ajustando el precio máximo del menú a " ?precio_menu_max "€. Es el precio máximo disponible en el sistema." crlf crlf)
          (bind ?precio_max ?precio_menu_max)
          
          ;; Verificar que el precio mínimo mantenga la diferencia de 5€
          (if (< ?precio_max (+ ?precio_min 5)) then
               (bind ?precio_min (- ?precio_max 5))
               (printout t "ADVERTENCIA: Ajustando el precio mínimo a " ?precio_min "€ para mantener una diferencia mínima de 5€." crlf crlf)
          )
     )

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

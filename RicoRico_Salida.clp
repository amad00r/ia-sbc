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

(defrule RicoRico_Salida::escrituraSalida
    ?pref <- (object (is-a Preferencias) (precio_min ?min) (precio_max ?max))
    =>
    (printout t "Procesando la salida ..." crlf crlf)

    ; Mostrar las preferencias y restricciones del usuario
    (bind ?prefs (find-instance ((?p Preferencias)) TRUE))
    (if (neq ?prefs nil) then
        (bind ?prefs (nth$ 1 ?prefs))
        (printout t "Preferencias y restricciones definidas por el usuario:" crlf)
        (printout t "       Tipo de evento:              " (send ?prefs get-tipo_evento) crlf)
        (printout t "       Número de comensales:        " (send ?prefs get-num_comensales) crlf)
        (printout t "       Precio mínimo:               " (send ?prefs get-precio_min) "€" crlf)
        (printout t "       Precio máximo:               " (send ?prefs get-precio_max) "€" crlf)
        (printout t "       Temporada:                   " (send ?prefs get-temporada) crlf)
        (printout t "       ¿Bebida alcohólica?:         " (if (eq (send ?prefs get-alcoholica) TRUE) then "Si" else "No") crlf)
        (printout t "       ¿Vino?:                      " (if (eq (send ?prefs get-vino) TRUE) then "Si" else "No") crlf)
        (printout t "       ¿Diferentes bebidas?:        " (if (eq (send ?prefs get-diferentesBebidas) TRUE) then "Si" else "No") crlf)
        (printout t "       ¿Intolerancia al gluten?:    " (if (eq (send ?prefs get-intolerancia_gluten) TRUE) then "Si" else "No") crlf)
        (printout t "       ¿Intolerancia a la lactosa?: " (if (eq (send ?prefs get-intolerancia_lactosa) TRUE) then "Si" else "No") crlf crlf)
    else
        (printout t "No hay preferencias definidas." crlf)
    )

    (bind ?menus (find-all-instances ((?m Menu)) (and
        (>= (send ?m get-precio) ?min)
        (<= (send ?m get-precio) ?max)
    )))
    (bind ?menus (sort cmp-menu $?menus))
    ; Mostrar menús según el número disponible
    (switch (length$ ?menus)
        (case 1 then
            (printout t "INFO: Con las restricciones actuales, solo se ha podido generar 1 menú." crlf crlf)
            (printout t "Menú:" crlf)
            (print-menu (nth 1 ?menus))
        )
        (case 2 then
            (printout t "INFO: Con las restricciones actuales, solo se han podido generar 2 menús." crlf crlf)
            (printout t "Menú Caro:" crlf)
            (print-menu (nth 1 ?menus))
            (printout t "Menú Barato:" crlf)
            (print-menu (nth 2 ?menus))
        )
        (case 3 then
            (printout t "INFO: Se han generado 3 menús." crlf crlf)
            (printout t "Menú Caro:" crlf)
            (print-menu (nth 1 ?menus))
            (printout t "Menú Estándar:" crlf)
            (print-menu (nth 2 ?menus))
            (printout t "Menú Barato:" crlf)
            (print-menu (nth 3 ?menus))
        )
        (default
            (printout t "INFO: No se ha podido generar ningún menú - Condiciones demasiado restrictivas." crlf)
        )
    )


    (bind ?menus-alternativos (find-all-instances ((?m Menu)) (or
        (< (send ?m get-precio) ?min)
        (> (send ?m get-precio) ?max)
    )))
    (bind ?menus-alternativos-size (length$ ?menus-alternativos))

    (if (> ?menus-alternativos-size 0) then
        (printout t "INFO: Proponemos " ?menus-alternativos-size " menús alternativos que se acercan lo máximo al rango de precios deseado." crlf crlf)
        (foreach ?m ?menus-alternativos
            (printout t "Menú Alternativo:" crlf)
            (print-menu ?m)
        )
    )
)

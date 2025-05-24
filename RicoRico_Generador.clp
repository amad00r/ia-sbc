(deffunction RicoRico_Generador::compute_price (?b1 ?p1 ?b2 ?p2 ?pst) (+
    (send ?b1  get-precio)
    (send ?p1  get-precio)
    (if (eq (send ?b1 get-nombre) (send ?b2 get-nombre)) then 0 else (send ?b2 get-precio))
    (send ?p2  get-precio)
    (send ?pst get-precio)
))

(defrule RicoRico_Generador::generate_menu
    ?pref <- (object (is-a Preferencias) (precio_min ?min) (precio_max ?max) (diferentesBebidas ?diffBebidas))
    =>
    (bind ?bebidas (find-all-instances ((?b Bebida)) TRUE))
    (bind ?platos1 (find-all-instances ((?p Plato)) (or (eq 1 (send ?p get-tipo)) (eq 3 (send ?p get-tipo)))))
    (bind ?platos2 (find-all-instances ((?p Plato)) (or (eq 2 (send ?p get-tipo)) (eq 3 (send ?p get-tipo)))))
    (bind ?postres (find-all-instances ((?p Plato)) (eq 0 (send ?p get-tipo))))

    (bind ?bebidas-size (length$ ?bebidas))
    (bind ?platos1-size (length$ ?platos1))
    (bind ?platos2-size (length$ ?platos2))
    (bind ?postres-size (length$ ?postres))

    (bind ?visited-menus (create$))

    ; Number of desired valid menus
    (bind ?valid-menus 30)
    ; Number of attempts
    (bind ?attempts 10000)

    (if ?diffBebidas then
        (bind ?max-menus (* ?bebidas-size ?bebidas-size ?platos1-size ?platos2-size ?postres-size))

        (while (and (> ?valid-menus 0) (> ?attempts 0) (< (length$ ?visited-menus) ?max-menus))
            (bind ?b1-idx  (random 1 ?bebidas-size))
            (bind ?b2-idx  (random 1 ?bebidas-size))
            (bind ?p1-idx  (random 1 ?platos1-size))
            (bind ?p2-idx  (random 1 ?platos2-size))
            (bind ?pst-idx (random 1 ?postres-size))

            (bind ?menu-id (+
                (* (- ?b1-idx 1) ?bebidas-size ?platos1-size ?platos2-size ?postres-size)
                (* (- ?b2-idx 1) ?platos1-size ?platos2-size ?postres-size)
                (* (- ?p1-idx 1) ?platos2-size ?postres-size)
                (* (- ?p2-idx 1) ?postres-size)
                (- ?pst-idx 1)
            ))

            (if (not (member$ ?menu-id ?visited-menus)) then
                (bind ?b1                        (nth$ ?b1-idx ?bebidas))
                (bind ?b2                        (nth$ ?b2-idx ?bebidas))
                (bind ?p1                        (nth$ ?p1-idx ?platos1))
                (bind ?p2                        (nth$ ?p2-idx ?platos2))
                (bind ?pst                       (nth$ ?pst-idx ?postres))
                (bind ?price                     (compute_price ?b1 ?p1 ?b2 ?p2 ?pst))
                (bind ?p1-bebidas-incompatibles  (send ?p1 get-incompatibleConBebida))
                (bind ?p2-bebidas-incompatibles  (send ?p2 get-incompatibleConBebida))
                (bind ?pst-bebidas-incompatibles (send ?pst get-incompatibleConBebida))
                (bind ?p1-platos-incompatibles   (send ?p1 get-incompatibleConPlato))
                (bind ?p2-platos-incompatibles   (send ?p2 get-incompatibleConPlato))
                (bind ?pst-platos-incompatibles  (send ?pst get-incompatibleConPlato))

                (bind ?is-almost-valid (and
                    ; Check that Bebidas are different
                    (neq ?b1 ?b2)

                    ; Platos cannot be the same
                    (neq ?p1 ?p2)

                    ; Check Bebida incompatibilities
                    (not (member$ ?b1 (send ?p1 get-incompatibleConBebida)))
                    (not (member$ ?b1 (send ?p2 get-incompatibleConBebida)))
                    (not (member$ ?b1 (send ?pst get-incompatibleConBebida)))
                    (not (member$ ?b2 (send ?p1 get-incompatibleConBebida)))
                    (not (member$ ?b2 (send ?p2 get-incompatibleConBebida)))
                    (not (member$ ?b2 (send ?pst get-incompatibleConBebida)))

                    ; Check Plato incompatibilities
                    (not (member$ ?p1 (send ?p2 get-incompatibleConPlato)))
                    (not (member$ ?p1 (send ?pst get-incompatibleConPlato)))
                    (not (member$ ?p2 (send ?p1 get-incompatibleConPlato)))
                    (not (member$ ?p2 (send ?pst get-incompatibleConPlato)))
                    (not (member$ ?pst (send ?p1 get-incompatibleConPlato)))
                    (not (member$ ?pst (send ?p2 get-incompatibleConPlato)))
                ))

                (bind ?is-in-range (and
                    (>= ?price ?min)
                    (<= ?price ?max)
                ))

                (if ?is-almost-valid then
                    (make-instance of Menu
                        (1rBebida ?b1)
                        (1rPlato  ?p1)
                        (2oBebida ?b2)
                        (2oPlato  ?p2)
                        (postre   ?pst)
                        (precio   ?price))
                    
                    (if ?is-in-range then (bind ?valid-menus (- ?valid-menus 1)))
                )

                (bind ?visited-menus (insert$ ?visited-menus 1 ?menu-id))
            )
            (bind ?attempts (- ?attempts 1))
        )


    else
        (bind ?max-menus (* ?bebidas-size ?platos1-size ?platos2-size ?postres-size))

        (while (and (> ?valid-menus 0) (> ?attempts 0) (< (length$ ?visited-menus) ?max-menus))
            (bind ?b-idx   (random 1 ?bebidas-size))
            (bind ?p1-idx  (random 1 ?platos1-size))
            (bind ?p2-idx  (random 1 ?platos2-size))
            (bind ?pst-idx (random 1 ?postres-size))

            (bind ?menu-id (+
                (* (- ?b-idx 1)  ?platos1-size ?platos2-size ?postres-size)
                (* (- ?p1-idx 1) ?platos2-size ?postres-size)
                (* (- ?p2-idx 1) ?postres-size)
                (- ?pst-idx 1)
            ))

            (if (not (member$ ?menu-id ?visited-menus)) then
                (bind ?b                         (nth$ ?b-idx ?bebidas))
                (bind ?p1                        (nth$ ?p1-idx ?platos1))
                (bind ?p2                        (nth$ ?p2-idx ?platos2))
                (bind ?pst                       (nth$ ?pst-idx ?postres))
                (bind ?price                     (compute_price ?b ?p1 ?b ?p2 ?pst))
                (bind ?p1-bebidas-incompatibles  (send ?p1 get-incompatibleConBebida))
                (bind ?p2-bebidas-incompatibles  (send ?p2 get-incompatibleConBebida))
                (bind ?pst-bebidas-incompatibles (send ?pst get-incompatibleConBebida))
                (bind ?p1-platos-incompatibles   (send ?p1 get-incompatibleConPlato))
                (bind ?p2-platos-incompatibles   (send ?p2 get-incompatibleConPlato))
                (bind ?pst-platos-incompatibles  (send ?pst get-incompatibleConPlato))

                (bind ?is-almost-valid (and
                    ; Platos cannot be the same
                    (neq ?p1 ?p2)

                    ; Check Bebida incompatibilities
                    (not (member$ ?b (send ?p1 get-incompatibleConBebida)))
                    (not (member$ ?b (send ?p2 get-incompatibleConBebida)))
                    (not (member$ ?b (send ?pst get-incompatibleConBebida)))

                    ; Check Plato incompatibilities
                    (not (member$ ?p1 (send ?p2 get-incompatibleConPlato)))
                    (not (member$ ?p1 (send ?pst get-incompatibleConPlato)))
                    (not (member$ ?p2 (send ?p1 get-incompatibleConPlato)))
                    (not (member$ ?p2 (send ?pst get-incompatibleConPlato)))
                    (not (member$ ?pst (send ?p1 get-incompatibleConPlato)))
                    (not (member$ ?pst (send ?p2 get-incompatibleConPlato)))
                ))

                (bind ?is-in-range (and
                    (>= ?price ?min)
                    (<= ?price ?max)
                ))

                (if ?is-almost-valid then
                    (make-instance of Menu
                        (1rBebida ?b)
                        (1rPlato  ?p1)
                        (2oBebida ?b)
                        (2oPlato  ?p2)
                        (postre   ?pst)
                        (precio   ?price))
                    
                    (if ?is-in-range then (bind ?valid-menus (- ?valid-menus 1)))
                )

                (bind ?visited-menus (insert$ ?visited-menus 1 ?menu-id))
            )
            (bind ?attempts (- ?attempts 1))
        )
    )



    (bind ?final-menus (find-all-instances ((?m Menu)) (and
        (>= (send ?m get-precio) ?min)
        (<= (send ?m get-precio) ?max)
    )))

    (if (> (length$ ?final-menus) 3) then
        (bind ?final-menus (sort cmp-menu $?final-menus))
        (bind ?final-menus (create$
            (nth$ 1 ?final-menus)
            (nth$ (div (length$ ?final-menus) 2) ?final-menus)
            (nth$ (length$ ?final-menus) ?final-menus)
        ))
    else
        (bind ?almost-valid-menus (find-all-instances ((?m Menu)) (or
            (< (send ?m get-precio) ?min)
            (> (send ?m get-precio) ?max)
        )))

        (bind ?mid-price (/ (+ ?min ?max) 2))
        (while (and (< (length$ ?final-menus) 3) (> (length$ ?almost-valid-menus) 0))
            (bind ?min-price-dist-idx 1)
            (bind ?min-price-dist (abs (- ?mid-price (send (nth$ 1 ?almost-valid-menus) get-precio))))
            (loop-for-count (?i 2 (length$ ?almost-valid-menus))
                (bind ?dist (abs (- ?mid-price (send (nth$ ?i ?almost-valid-menus) get-precio))))
                (if (< ?dist ?min-price-dist) then
                    (bind ?min-price-dist-idx ?i)
                    (bind ?min-price-dist ?dist)
                )
            )
            (bind ?final-menus (insert$ ?final-menus 1 (nth$ ?min-price-dist-idx ?almost-valid-menus)))
            (bind ?almost-valid-menus (delete$ ?almost-valid-menus ?min-price-dist-idx ?min-price-dist-idx))
        )
    )

    (do-for-all-instances ((?m Menu)) (not (member$ ?m ?final-menus))
        (send ?m delete)
    )

    (focus RicoRico_Salida)
)

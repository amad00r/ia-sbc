(deffunction RicoRico_Generador::compute_price (?b1 ?p1 ?b2 ?p2 ?pst) (+
    (send ?b1  get-precio)
    (send ?p1  get-precio)
    (if (eq (send ?b1 get-nombre) (send ?b2 get-nombre)) then 0 else (send ?b2 get-precio))
    (send ?p2  get-precio)
    (send ?pst get-precio)
))

(defrule RicoRico_Generador::generate_menu
    (declare (salience 1))
    ?b1  <- (object (is-a Bebida))
    ?b2  <- (object (is-a Bebida))
    ?p1  <- (object (is-a Plato) (tipo ?t1&:(or (eq 1 ?t1) (eq 3 ?t1))))
    ?p2  <- (object (is-a Plato) (tipo ?t2&:(or (eq 2 ?t2) (eq 3 ?t2))))
    ?pst <- (object (is-a Plato) (tipo 0))
    =>
    (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
    (bind ?price (compute_price ?b1 ?p1 ?b2 ?p2 ?pst))
    (bind ?p1-bebidas-incompatibles (send ?p1 get-incompatibleConBebida))
    (bind ?p2-bebidas-incompatibles (send ?p2 get-incompatibleConBebida))
    (bind ?pst-bebidas-incompatibles (send ?pst get-incompatibleConBebida))
    (bind ?p1-platos-incompatibles (send ?p1 get-incompatibleConPlato))
    (bind ?p2-platos-incompatibles (send ?p2 get-incompatibleConPlato))
    (bind ?pst-platos-incompatibles (send ?pst get-incompatibleConPlato))

    (if (and
        (neq ?p1 ?p2)
        (>= ?price (send ?pref get-precio_min))
        (<= ?price (send ?pref get-precio_max))
        (or
            (and (send ?pref get-diferentesBebidas) (neq ?b1 ?b2))
            (and (not (send ?pref get-diferentesBebidas)) (eq ?b1 ?b2))
        )
        (not (member$ ?b1 ?p1-bebidas-incompatibles))
        (not (member$ ?b1 ?p2-bebidas-incompatibles))
        (not (member$ ?b1 ?pst-bebidas-incompatibles))
        (not (member$ ?b2 ?p1-bebidas-incompatibles))
        (not (member$ ?b2 ?p2-bebidas-incompatibles))
        (not (member$ ?b2 ?pst-bebidas-incompatibles))
        (not (member$ ?p1 ?p2-platos-incompatibles))
        (not (member$ ?p1 ?pst-platos-incompatibles))
        (not (member$ ?p2 ?p1-platos-incompatibles))
        (not (member$ ?p2 ?pst-platos-incompatibles))
        (not (member$ ?pst ?p1-platos-incompatibles))
        (not (member$ ?pst ?p2-platos-incompatibles))
     ) then
        (make-instance of Menu
            (1rBebida ?b1)
            (1rPlato ?p1)
            (2oBebida ?b2)
            (2oPlato ?p2)
            (postre ?pst)
            (precio ?price))
    )
)

(deffunction RicoRico_Generador::compare-by-precio (?a ?b)
    (< (send ?a get-precio) (send ?b get-precio)))

(defrule RicoRico_Generador::select_final_menus
    =>
    (bind ?menus (find-all-instances ((?m Menu)) TRUE))
    (bind ?count (length$ ?menus))
    (if (> ?count 3) then
        (bind ?m1 (nth$ (random 1 ?count) ?menus))
        (bind ?m2 (nth$ (random 1 ?count) ?menus))
        (bind ?m3 (nth$ (random 1 ?count) ?menus))

        (do-for-all-instances ((?m Menu)) TRUE
            (if (and (neq ?m ?m1) (neq ?m ?m2) (neq ?m ?m3)) then
                (send ?m delete)))
    )

    (focus RicoRico_Salida)
)

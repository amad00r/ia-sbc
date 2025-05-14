;; PARA TESTEAR EL FILTRO DE FORMA INDEPENDIENTE

(defmodule RicoRico_Filtrado
    (import MAIN ?ALL)
    (import RicoRico_Entrada ?ALL)
    (export ?ALL)
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

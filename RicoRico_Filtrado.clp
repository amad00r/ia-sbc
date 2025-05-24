(deffunction RicoRico_Filtrado::eliminar_platos_complejos ()
     (printout t "Eliminando platos complejos..." crlf)

     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (bind ?comensales (send ?pref get-num_comensales))

     (bind ?lista (find-all-instances ((?p Plato)) TRUE))

     (foreach ?plato ?lista
          (bind ?dificultad (send ?plato get-dificultad))
          (if (or 
               (and (> ?comensales 20) (> ?dificultad 9))
               (and (> ?comensales 50) (> ?dificultad 7))
               (and (> ?comensales 100) (> ?dificultad 5)))
          then
               (send ?plato delete)
          )
     )
)

(deffunction RicoRico_Filtrado::eliminar_platos_fuera_temporada ()
     (printout t "Eliminando platos fuera de temporada..." crlf) 

     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (bind ?temporada (send ?pref get-temporada))

     (bind ?lista1 (find-all-instances ((?l1 Plato)) TRUE))

     (foreach ?plato ?lista1
          (bind ?temporadas (send ?plato get-disponibleEn))
          (if (not (member$ (instance-name ?temporada) ?temporadas)) then
               (send ?plato delete)
          )
     )
)

(deffunction RicoRico_Filtrado::eliminar_bebidas_alcoholicas ()
     (printout t "Eliminando bebidas alcoholicas..." crlf)

     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (do-for-all-instances ((?bebida Bebida))
          (if (not (eq (send ?bebida get-alcoholica) (send ?pref get-alcoholica))) then
               (send ?bebida delete)
          )
     )
)

(deffunction RicoRico_Filtrado::eliminar_vinos ()
     (printout t "Eliminando vinos..." crlf)

     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (if (eq (send ?pref get-vino) FALSE) then

          (do-for-all-instances ((?vino Vino))
               (send ?vino delete)  
          )
     else
          (do-for-all-instances ((?casual Casual))
               (send ?casual delete)
          )
     )
)

(deffunction RicoRico_Filtrado::eliminar_platos_lactosa ()
     (printout t "Eliminando platos con lactosa..." crlf)
    
     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (bind ?lactosa (send ?pref get-intolerancia_lactosa))
     (if ?lactosa then

          (bind ?lista1 (find-all-instances ((?l1 Plato)) TRUE))

          (foreach ?plato ?lista1
               (bind ?ingredienteList (send ?plato get-compuestoPor))
               (bind ?delete FALSE)
               
               (foreach ?ingrediente ?ingredienteList

                    (if (not (send ?ingrediente get-lactosaFree)) then
                         (bind ?delete TRUE)
                    )
               )

               (if ?delete then
                    (send ?plato delete)     
               )
          )
     )
)

(deffunction RicoRico_Filtrado::eliminar_bebidas_lactosa ()
     (printout t "Eliminando bebidas con lactosa..." crlf)

     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (if (send ?pref get-intolerancia_lactosa) then

          (do-for-all-instances ((?bebida Bebida))
               (if (not (send ?bebida get-lactosaFree)) then
                    (send ?bebida delete)
               )   
          )
     )
)

(deffunction RicoRico_Filtrado::eliminar_platos_gluten ()
     (printout t "Eliminando platos con gluten..." crlf)

     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (bind ?gluten (send ?pref get-intolerancia_gluten))

     (if ?gluten then

          (bind ?lista1 (find-all-instances ((?l1 Plato)) TRUE))
          (foreach ?plato ?lista1
               (bind ?ingredienteList (send ?plato get-compuestoPor))
               (bind ?delete FALSE)

               (foreach ?ingrediente ?ingredienteList
                    (if (not (send ?ingrediente get-glutenFree)) then
                         (bind ?delete TRUE)
                    )
               )

               (if ?delete then
                    (send ?plato delete)     
               )
          )
     )
)

(deffunction RicoRico_Filtrado::eliminar_bebidas_gluten ()
     (printout t "Eliminando bebidas con gluten..." crlf)
    
     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (if (send ?pref get-intolerancia_gluten) then

          (do-for-all-instances ((?bebida Bebida))
               (if (not (send ?bebida get-glutenFree)) then
                    (send ?bebida delete)
               )   
          )
     )
)

(defrule RicoRico_Filtrado::filtrado
     =>
     (eliminar_bebidas_alcoholicas)
     (eliminar_bebidas_gluten)
     (eliminar_bebidas_lactosa)
     (eliminar_platos_complejos)
     (eliminar_platos_fuera_temporada)
     (eliminar_platos_gluten)
     (eliminar_platos_lactosa)
     (eliminar_vinos)

     (focus RicoRico_Generador)
)

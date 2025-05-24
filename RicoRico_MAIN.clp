(defmodule MAIN (export ?ALL))

;Módulo para solicitar las preferencias del usuario.
(defmodule RicoRico_Entrada (import MAIN ?ALL) (export ?ALL))

;Módulo para realizar el filtrado según las preferencias del usuario.
(defmodule RicoRico_Filtrado (import MAIN ?ALL) (export ?ALL))

;Módulo para generar los menús.
(defmodule RicoRico_Generador (import MAIN ?ALL) (export ?ALL))

;Módulo para mostrar los menús generados al usuario.
(defmodule RicoRico_Salida (import MAIN ?ALL) (export ?ALL))


;; Para almacenar las preferencias y restricciones del usuario.
(defclass MAIN::Preferencias
     (is-a USER)
     (slot tipo_evento)
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

(deffunction MAIN::cmp-menu (?m1 ?m2)
    (<= (send ?m1 get-precio) (send ?m2 get-precio))
)

(defrule MAIN::inicio 
     => 
     (printout t "Bienvenido al creador de menús para el catering RicoRico." crlf)
     (focus RicoRico_Entrada)
)

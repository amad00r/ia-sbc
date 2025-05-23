;;; ---------------------------------------------------------
;;; RicoRico_Ontology.clp
;;; Translated by owl2clips
;;; Translated to CLIPS from ontology ricorico.ttl
;;; :Date 15/05/2025 19:25:49

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
    ;;; Atributo de tipo Float para indicar el precio del elemento en cuestión.
    (slot precio
        (type FLOAT)
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
    ;;; Relación para indicar en qué temporada está disponible un plato.
    (multislot disponibleEn
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
    ([a_la_brasa] of Preparacion
         (nombre  "a_la_brasa")
    )

    ([a_la_parrilla] of Preparacion
         (nombre  "a_la_parrilla")
    )

    ([a_la_plancha] of Preparacion
         (nombre  "a_la_plancha")
    )

    ([aceite_de_oliva] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "aceite_de_oliva")
    )

    ([agua_con_gas] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "agua_con_gas")
         (precio  1.5)
    )

    ([agua_sin_gas] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "agua_sin_gas")
         (precio  1.5)
    )

    ([ajo] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "ajo")
    )

    ([albahaca] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "albahaca")
    )

    ([albarino] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "albarino")
         (precio  12.0)
    )

    ([alcaparras] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "alcaparras")
    )

    ([almejas] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "almejas")
    )

    ([almendra] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "almendra")
    )

    ([anchoas] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "anchoas")
    )

    ([apio] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "apio")
    )

    ([arroz] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "arroz")
    )

    ([arroz_caldoso] of Plato
         (compuestoPor  [ajo] [arroz] [calamares] [mariscos] [pimiento_rojo])
         (disponibleEn  [invierno] [otono])
         (esCategoria  [arroz])
         (esPreparacion  [guisado])
         (incompatibleConBebida  [cerveza_artesanal] [lambrusco] [tinto_de_verano])
         (incompatibleConPlato  [arroz_con_leche] [paella_valenciana] [risotto_ai_funghi])
         (originarioDe  [espana])
         (dificultad  7)
         (nombre  "arroz_caldoso")
         (precio  13.5)
    )

    ([arroz_con_leche] of Plato
         (compuestoPor  [arroz] [azucar] [canela] [leche] [limon])
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (esCategoria  [postre])
         (esPreparacion  [hervido])
         (incompatibleConBebida  [barolo] [cabernet_sauvignon] [chianti])
         (incompatibleConPlato  [arroz_caldoso] [paella_valenciana] [risotto_ai_funghi])
         (originarioDe  [espana])
         (dificultad  4)
         (nombre  "arroz_con_leche")
         (precio  4.5)
    )

    ([asado_y_frito] of Preparacion
         (nombre  "asado_y_frito")
    )

    ([assiette_de_fromages] of Plato
         (compuestoPor  [baguette] [brie] [comte] [queso_de_cabra] [roquefort])
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (esCategoria  [quesoc])
         (esPreparacion  [montado])
         (incompatibleConBebida  [fanta_naranja] [pepsi] [7up])
         (incompatibleConPlato  [salade_de_chevre_chaud] [spanakopita] [tiropita])
         (originarioDe  [francia])
         (dificultad  0)
         (nombre  "assiette_de_fromages")
         (precio  8.0)
    )

    ([atun] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "atun")
    )

    ([aves] of Categoria
         (nombre  "aves")
    )

    ([azafran] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "azafran")
    )

    ([azucar] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "azucar")
    )

    ([azucar_moreno] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "azucar_moreno")
    )

    ([bacon] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "bacon")
    )

    ([baguette] of Ingrediente
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "baguette")
    )

    ([baklava] of Plato
         (compuestoPor  [azucar] [mantequilla] [masa_filo] [miel] [nuez] [pistacho])
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (esCategoria  [postre])
         (esPreparacion  [horno])
         (incompatibleConBebida  [barolo] [cabernet_sauvignon] [cerveza_artesanal] [cerveza_rubia] [merlot])
         (incompatibleConPlato  [cannoli_siciliani] [tarta_de_santiago] [torta_caprese])
         (originarioDe  [grecia])
         (dificultad  6)
         (nombre  "baklava")
         (precio  6.0)
    )

    ([barolo] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "barolo")
         (precio  22.0)
    )

    ([batido_de_chocolate] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "batido_de_chocolate")
         (precio  3.0)
    )

    ([bebida_de_yogur] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "bebida_de_yogur")
         (precio  2.8)
    )

    ([berenjena] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "berenjena")
    )

    ([bistecca_alla_fiorentina] of Plato
         (compuestoPor  [aceite_de_oliva] [pimienta_negra] [sal_gruesa] [ternera])
         (disponibleEn  [verano])
         (esCategoria  [carne])
         (esPreparacion  [a_la_plancha])
         (incompatibleConBebida  [agua_con_gas] [agua_sin_gas] [cerveza_sin_alcohol])
         (incompatibleConPlato  [kleftiko] [ossobuco_alla_milanese] [steak_frites])
         (originarioDe  [italia])
         (dificultad  7)
         (nombre  "bistecca_alla_fiorentina")
         (precio  16.5)
    )

    ([bizcocho] of Ingrediente
         (glutenFree  FALSE)
         (lactosaFree  FALSE)
         (nombre  "bizcocho")
    )

    ([blanco_albarino] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "blanco_albarino")
         (precio  11.0)
    )

    ([blanco_verdejo] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "blanco_verdejo")
         (precio  10.0)
    )

    ([boeuf_bourguignon] of Plato
         (compuestoPor  [ajo] [bacon] [caldo_de_carne] [carne_de_res] [cebolla] [champinones] [harina] [laurel] [mantequilla] [pasta_de_tomate] [pimienta] [sal] [tomillo] [vino_tinto] [zanahoria])
         (disponibleEn  [invierno] [otono])
         (esCategoria  [carne])
         (esPreparacion  [estofado])
         (incompatibleConBebida  [agua_sin_gas] [sprite] [7up])
         (incompatibleConPlato  [cocido_madrileno] [coq_au_vin] [fabada_asturiana])
         (originarioDe  [francia])
         (dificultad  8)
         (nombre  "boeuf_bourguignon")
         (precio  13.0)
    )

    ([bouillabaisse] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [azafran] [caldo_de_pescado] [cebolla] [hinojo] [laurel] [mariscos] [pescados_variados] [puerro] [ralladura_de_naranja] [tomate] [tomillo])
         (disponibleEn  [verano])
         (esCategoria  [mariscos])
         (esPreparacion  [rehogado])
         (incompatibleConBebida  [cerveza_artesanal] [fanta_naranja] [pepsi])
         (incompatibleConPlato  [calamares_a_la_romana] [moules_marinieres] [pulpo_a_la_gallega])
         (originarioDe  [francia])
         (dificultad  10)
         (nombre  "bouillabaisse")
         (precio  17.0)
    )

    ([brie] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "brie")
    )

    ([cabernet_sauvignon] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "cabernet_sauvignon")
         (precio  19.5)
    )

    ([cacao_en_polvo] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "cacao_en_polvo")
    )

    ([cafe] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "cafe")
    )

    ([calabacin] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "calabacin")
    )

    ([calamares] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "calamares")
    )

    ([calamares_a_la_romana] of Plato
         (compuestoPor  [aceite_de_oliva] [calamares] [harina] [limon])
         (disponibleEn  [verano])
         (esCategoria  [mariscos])
         (esPreparacion  [frito])
         (incompatibleConBebida  [fanta_naranja] [pepsi] [refresco_de_cola])
         (incompatibleConPlato  [bouillabaisse] [moules_marinieres] [pulpo_a_la_gallega])
         (originarioDe  [espana])
         (dificultad  6)
         (nombre  "calamares_a_la_romana")
         (precio  10.0)
    )

    ([caldo_de_carne] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "caldo_de_carne")
    )

    ([caldo_de_pescado] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "caldo_de_pescado")
    )

    ([caldo_de_pollo] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "caldo_de_pollo")
    )

    ([caldo_de_verduras] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "caldo_de_verduras")
    )

    ([canela] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "canela")
    )

    ([cannoli_siciliani] of Plato
         (compuestoPor  [azucar] [canela] [cascara_de_naranja] [chocolate] [harina] [ricotta])
         (disponibleEn  [invierno] [otono])
         (esCategoria  [dulce])
         (esPreparacion  [frito])
         (incompatibleConBebida  [agua_con_gas] [agua_sin_gas] [limonada_casera])
         (incompatibleConPlato  [baklava] [tarta_de_santiago] [tiramisu])
         (originarioDe  [italia])
         (dificultad  6)
         (nombre  "cannoli_siciliani")
         (precio  6.0)
    )

    ([caracoles] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "caracoles")
    )

    ([caracolesc] of Categoria
         (nombre  "caracolesc")
    )

    ([caramelizado_y_horneado] of Preparacion
         (nombre  "caramelizado_y_horneado")
    )

    ([carne] of Categoria
         (nombre  "carne")
    )

    ([carne_de_cerdo] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "carne_de_cerdo")
    )

    ([carne_de_res] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "carne_de_res")
    )

    ([carne_picada] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "carne_picada")
    )

    ([cascara_de_naranja] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "cascara_de_naranja")
    )

    ([casqueria] of Categoria
         (nombre  "casqueria")
    )

    ([cazuela] of Categoria
         (nombre  "cazuela")
    )

    ([cebolla] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "cebolla")
    )

    ([cerveza_artesanal] of Casual
         (alcoholica  TRUE)
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "cerveza_artesanal")
         (precio  4.0)
    )

    ([cerveza_rubia] of Casual
         (alcoholica  TRUE)
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "cerveza_rubia")
         (precio  3.5)
    )

    ([cerveza_sin_alcohol] of Casual
         (alcoholica  FALSE)
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "cerveza_sin_alcohol")
         (precio  3.5)
    )

    ([chalotas] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "chalotas")
    )

    ([champinones] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "champinones")
    )

    ([chardonnay] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "chardonnay")
         (precio  17.0)
    )

    ([chianti] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "chianti")
         (precio  13.0)
    )

    ([chocolate] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "chocolate")
    )

    ([chorizo] of Ingrediente
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "chorizo")
    )

    ([cocido] of Preparacion
         (nombre  "cocido")
    )

    ([cocido_a_baja_temperatura_y_frito] of Preparacion
         (nombre  "cocido_a_baja_temperatura_y_frito")
    )

    ([cocido_madrileno] of Plato
         (compuestoPor  [chorizo] [garbanzos] [morcilla] [patatas] [ternera] [zanahoria])
         (disponibleEn  [invierno])
         (esCategoria  [legumbres])
         (esPreparacion  [cocido])
         (incompatibleConBebida  [limonada_casera] [sprite] [7up])
         (incompatibleConPlato  [boeuf_bourguignon] [coq_au_vin] [fabada_asturiana])
         (originarioDe  [espana])
         (dificultad  9)
         (nombre  "cocido_madrileno")
         (precio  14.0)
    )

    ([cocido_y_batido] of Preparacion
         (nombre  "cocido_y_batido")
    )

    ([comte] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "comte")
    )

    ([confit_de_canard] of Plato
         (compuestoPor  [ajo] [grasa_de_pato] [laurel] [muslos_de_pato] [pimienta] [sal] [tomillo])
         (disponibleEn  [invierno] [otono])
         (esCategoria  [pato])
         (esPreparacion  [cocido_a_baja_temperatura_y_frito])
         (incompatibleConBebida  [agua_con_gas] [cerveza_sin_alcohol] [sprite])
         (incompatibleConPlato  [foie_gras] [kleftiko] [ossobuco_alla_milanese])
         (originarioDe  [francia])
         (dificultad  9)
         (nombre  "confit_de_canard")
         (precio  15.0)
    )

    ([coq_au_vin] of Plato
         (compuestoPor  [ajo] [bacon] [caldo_de_pollo] [cebolla] [champinones] [harina] [laurel] [mantequilla] [pimienta] [pollo] [sal] [tomillo] [vino_tinto] [zanahoria])
         (disponibleEn  [invierno] [otono])
         (esCategoria  [aves])
         (esPreparacion  [estofado])
         (incompatibleConBebida  [fanta_naranja] [limonada_casera] [pepsi])
         (incompatibleConPlato  [boeuf_bourguignon] [fabada_asturiana] [pollo_alla_cacciatora])
         (originarioDe  [francia])
         (dificultad  8)
         (nombre  "coq_au_vin")
         (precio  12.0)
    )

    ([cordero] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "cordero")
    )

    ([crema_catalana] of Plato
         (compuestoPor  [azucar] [huevos] [leche] [limon] [maicena])
         (disponibleEn  [primavera] [verano])
         (esCategoria  [postre])
         (esPreparacion  [hervido])
         (incompatibleConBebida  [albarino] [cerveza_artesanal] [tinto_rioja])
         (incompatibleConPlato  [creme_brulee] [natillas_caseras] [panna_cotta])
         (originarioDe  [espana])
         (dificultad  6)
         (nombre  "crema_catalana")
         (precio  4.0)
    )

    ([creme_brulee] of Plato
         (compuestoPor  [azucar] [azucar_moreno] [huevos] [nata] [vainilla])
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (esCategoria  [postre])
         (esPreparacion  [horneado_y_caramelizado])
         (incompatibleConBebida  [cerveza_artesanal] [cerveza_rubia] [tinto_de_verano])
         (incompatibleConPlato  [crema_catalana] [natillas_caseras] [panna_cotta])
         (originarioDe  [francia])
         (dificultad  6)
         (nombre  "creme_brulee")
         (precio  5.5)
    )

    ([crudo] of Preparacion
         (nombre  "crudo")
    )

    ([dip] of Categoria
         (nombre  "dip")
    )

    ([dolmades] of Plato
         (compuestoPor  [aceite_de_oliva] [arroz] [carne_de_res] [cebolla] [cordero] [hierbas] [hojas_de_parra] [limon])
         (disponibleEn  [primavera] [verano])
         (esCategoria  [carne])
         (esPreparacion  [vapor])
         (incompatibleConBebida  [barolo] [tinto_de_verano] [zinfandel])
         (incompatibleConPlato  [gemista] [kleftiko] [souvlaki])
         (originarioDe  [grecia])
         (dificultad  6)
         (nombre  "dolmades")
         (precio  7.0)
    )

    ([dulce] of Categoria
         (nombre  "dulce")
    )

    ([empanada_gallega] of Plato
         (compuestoPor  [aceite_de_oliva] [atun] [cebolla] [harina] [pimiento_rojo])
         (disponibleEn  [otono] [primavera])
         (esCategoria  [pescado])
         (esPreparacion  [horno])
         (incompatibleConBebida  [agua_con_gas] [batido_de_chocolate] [bebida_de_yogur])
         (incompatibleConPlato  [quiche_lorraine] [spanakopita] [tiropita])
         (originarioDe  [espana])
         (dificultad  5)
         (nombre  "empanada_gallega")
         (precio  7.5)
    )

    ([eneldo] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "eneldo")
    )

    ([escalivada] of Plato
         (compuestoPor  [aceite_de_oliva] [berenjena] [cebolla] [pimiento_rojo])
         (disponibleEn  [verano])
         (esCategoria  [vegetariano])
         (esPreparacion  [horno])
         (incompatibleConBebida  [cerveza_rubia] [cerveza_sin_alcohol] [tinto_de_verano])
         (incompatibleConPlato  [parmigiana_di_melanzane] [pisto_manchego] [ratatouille])
         (originarioDe  [espana])
         (dificultad  3)
         (nombre  "escalivada")
         (precio  7.0)
    )

    ([escargots_de_bourgogne] of Plato
         (compuestoPor  [ajo] [caracoles] [chalotas] [mantequilla] [perejil] [pimienta] [sal])
         (disponibleEn  [invierno] [otono])
         (esCategoria  [caracolesc])
         (esPreparacion  [horno])
         (incompatibleConBebida  [cerveza_rubia] [fanta_naranja] [pepsi])
         (incompatibleConPlato  [bouillabaisse] [moules_marinieres])
         (originarioDe  [francia])
         (dificultad  6)
         (nombre  "escargots_de_bourgogne")
         (precio  9.0)
    )

    ([espaguetis] of Ingrediente
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "espaguetis")
    )

    ([espana] of Origen
         (nombre  "espana")
    )

    ([espinacas] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "espinacas")
    )

    ([estofado] of Preparacion
         (nombre  "estofado")
    )

    ([fabada_asturiana] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [chorizo] [garbanzos] [morcilla])
         (disponibleEn  [invierno])
         (esCategoria  [legumbres])
         (esPreparacion  [guisado])
         (incompatibleConBebida  [agua_con_gas] [cerveza_sin_alcohol] [limonada_casera])
         (incompatibleConPlato  [boeuf_bourguignon] [cocido_madrileno] [coq_au_vin])
         (originarioDe  [espana])
         (dificultad  8)
         (nombre  "fabada_asturiana")
         (precio  12.0)
    )

    ([fanta_naranja] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "fanta_naranja")
         (precio  2.5)
    )

    ([fasolada] of Plato
         (compuestoPor  [aceite_de_oliva] [apio] [cebolla] [frijol] [hierbas] [tomate] [zanahoria])
         (disponibleEn  [invierno] [otono])
         (esCategoria  [sopa])
         (esPreparacion  [sopa])
         (incompatibleConBebida  [cerveza_artesanal] [chardonnay] [merlot] [zinfandel])
         (incompatibleConPlato  [fabada_asturiana] [gigantes_plaki] [minestrone])
         (originarioDe  [grecia])
         (dificultad  4)
         (nombre  "fasolada")
         (precio  4.0)
    )

    ([foie_gras] of Plato
         (compuestoPor  [higado_de_pato_o_ganso] [pimienta] [sal])
         (disponibleEn  [invierno] [otono])
         (esCategoria  [casqueria])
         (esPreparacion  [sellado_curado])
         (incompatibleConBebida  [cerveza_artesanal] [fanta_naranja] [pepsi])
         (incompatibleConPlato  [bistecca_alla_fiorentina] [confit_de_canard] [ossobuco_alla_milanese])
         (originarioDe  [francia])
         (dificultad  9)
         (nombre  "foie_gras")
         (precio  14.0)
    )

    ([francia] of Origen
         (nombre  "francia")
    )

    ([frijol] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "frijol")
    )

    ([frio] of Preparacion
         (nombre  "frio")
    )

    ([frito] of Preparacion
         (nombre  "frito")
    )

    ([frittata_di_zucchine] of Plato
         (compuestoPor  [aceite_de_oliva] [calabacin] [huevos] [queso_parmesano])
         (disponibleEn  [primavera] [verano])
         (esCategoria  [huevo])
         (esPreparacion  [frito])
         (incompatibleConBebida  [cabernet_sauvignon] [merlot] [zinfandel])
         (incompatibleConPlato  [huevos_rotos_con_jamon] [revuelto_de_setas] [tortilla_de_patatas])
         (originarioDe  [italia])
         (dificultad  3)
         (nombre  "frittata_di_zucchine")
         (precio  8.5)
    )

    ([galaktoboureko] of Plato
         (compuestoPor  [azucar] [leche] [limon] [mantequilla] [masa_filo] [semola])
         (disponibleEn  [primavera] [verano])
         (esCategoria  [postre])
         (esPreparacion  [horno])
         (incompatibleConBebida  [cabernet_sauvignon] [chianti] [merlot])
         (incompatibleConPlato  [loukoumades] [tarte_tatin] [torrijas])
         (originarioDe  [grecia])
         (dificultad  6)
         (nombre  "galaktoboureko")
         (precio  6.5)
    )

    ([garbanzos] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "garbanzos")
    )

    ([gazpacho_andaluz] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [pan] [pepino] [pimiento_rojo] [tomate])
         (disponibleEn  [verano])
         (esCategoria  [vegetariano])
         (esPreparacion  [crudo])
         (incompatibleConBebida  [barolo] [batido_de_chocolate] [bebida_de_yogur])
         (incompatibleConPlato  [salmorejo_cordobes] [sopa_avgolemono] [soupe_a_loignon])
         (originarioDe  [espana])
         (dificultad  3)
         (nombre  "gazpacho_andaluz")
         (precio  6.5)
    )

    ([gelatina] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "gelatina")
    )

    ([gemista] of Plato
         (compuestoPor  [aceite_de_oliva] [arroz] [calabacin] [cebolla] [hierbas] [pimiento_rojo] [tomate])
         (disponibleEn  [verano])
         (esCategoria  [vegetariano])
         (esPreparacion  [horno])
         (incompatibleConBebida  [barolo] [cabernet_sauvignon] [fanta_naranja] [lambrusco] [pepsi])
         (incompatibleConPlato  [dolmades] [pisto_manchego] [ratatouille])
         (originarioDe  [grecia])
         (dificultad  6)
         (nombre  "gemista")
         (precio  7.5)
    )

    ([gigantes_plaki] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [cebolla] [frijol] [hierbas] [tomate])
         (disponibleEn  [invierno] [otono])
         (esCategoria  [vegetariano])
         (esPreparacion  [horno])
         (incompatibleConBebida  [barolo] [cabernet_sauvignon] [chianti] [fanta_naranja] [7up])
         (incompatibleConPlato  [fabada_asturiana] [fasolada] [minestrone])
         (originarioDe  [grecia])
         (dificultad  4)
         (nombre  "gigantes_plaki")
         (precio  5.0)
    )

    ([gnocchi_al_pomodoro] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [pasta] [queso_parmesano] [tomate])
         (disponibleEn  [otono] [primavera])
         (esCategoria  [pasta])
         (esPreparacion  [hervido])
         (incompatibleConBebida  [fanta_naranja] [sprite] [7up])
         (incompatibleConPlato  [pasta_al_pesto_genovese] [pasta_alla_norma] [spaghetti_carbonara])
         (originarioDe  [italia])
         (dificultad  4)
         (nombre  "gnocchi_al_pomodoro")
         (precio  8.0)
    )

    ([grasa_de_pato] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "grasa_de_pato")
    )

    ([grecia] of Origen
         (nombre  "grecia")
    )

    ([guisado] of Preparacion
         (nombre  "guisado")
    )

    ([harina] of Ingrediente
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "harina")
    )

    ([hervido] of Preparacion
         (nombre  "hervido")
    )

    ([hierbas] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "hierbas")
    )

    ([higado_de_pato_o_ganso] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "higado_de_pato_o_ganso")
    )

    ([hinojo] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "hinojo")
    )

    ([hojas_de_parra] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "hojas_de_parra")
    )

    ([hojas_verdes] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "hojas_verdes")
    )

    ([horneado_y_caramelizado] of Preparacion
         (nombre  "horneado_y_caramelizado")
    )

    ([horneado_y_en_capas] of Preparacion
         (nombre  "horneado_y_en_capas")
    )

    ([horno] of Preparacion
         (nombre  "horno")
    )

    ([huevo] of Categoria
         (nombre  "huevo")
    )

    ([huevos] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "huevos")
    )

    ([huevos_rotos_con_jamon] of Plato
         (compuestoPor  [aceite_de_oliva] [huevos] [jamon_serrano] [patatas])
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (esCategoria  [huevo])
         (esPreparacion  [frito])
         (incompatibleConBebida  [chardonnay] [sauvignon_blanc] [verdicchio])
         (incompatibleConPlato  [frittata_di_zucchine] [revuelto_de_setas] [tortilla_de_patatas])
         (originarioDe  [espana])
         (dificultad  4)
         (nombre  "huevos_rotos_con_jamon")
         (precio  9.5)
    )

    ([invierno] of Temporada
         (nombre  "invierno")
    )

    ([italia] of Origen
         (nombre  "italia")
    )

    ([jamon_serrano] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "jamon_serrano")
    )

    ([judias] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "judias")
    )

    ([kleftiko] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [cordero] [limon] [patatas] [romero])
         (disponibleEn  [invierno] [otono])
         (esCategoria  [carne])
         (esPreparacion  [cocido])
         (incompatibleConBebida  [albarino] [sprite] [txakoli] [7up])
         (incompatibleConPlato  [bistecca_alla_fiorentina] [confit_de_canard] [ossobuco_alla_milanese])
         (originarioDe  [grecia])
         (dificultad  7)
         (nombre  "kleftiko")
         (precio  11.0)
    )

    ([lambrusco] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "lambrusco")
         (precio  9.5)
    )

    ([lardones] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "lardones")
    )

    ([lasagna_alla_bolognese] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [carne_picada] [cebolla] [pasta] [queso_parmesano] [tomate])
         (disponibleEn  [invierno] [otono])
         (esCategoria  [pasta])
         (esPreparacion  [horno])
         (incompatibleConBebida  [limonada_casera] [sprite] [7up])
         (incompatibleConPlato  [moussaka] [pastitsio] [tagliatelle_al_ragu])
         (originarioDe  [italia])
         (dificultad  8)
         (nombre  "lasagna_alla_bolognese")
         (precio  13.0)
    )

    ([laurel] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "laurel")
    )

    ([leche] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "leche")
    )

    ([legumbres] of Categoria
         (nombre  "legumbres")
    )

    ([levadura] of Ingrediente
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "levadura")
    )

    ([limon] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "limon")
    )

    ([limonada_casera] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "limonada_casera")
         (precio  2.5)
    )

    ([loukoumades] of Plato
         (compuestoPor  [azucar] [canela] [harina] [levadura] [miel])
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (esCategoria  [postre])
         (esPreparacion  [frito])
         (incompatibleConBebida  [barolo] [chardonnay] [zinfandel])
         (incompatibleConPlato  [galaktoboureko] [tarte_tatin] [torrijas])
         (originarioDe  [grecia])
         (dificultad  5)
         (nombre  "loukoumades")
         (precio  4.0)
    )

    ([maicena] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "maicena")
    )

    ([mantequilla] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "mantequilla")
    )

    ([manzana] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "manzana")
    )

    ([mariscos] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "mariscos")
    )

    ([masa_de_hojaldre] of Ingrediente
         (glutenFree  FALSE)
         (lactosaFree  FALSE)
         (nombre  "masa_de_hojaldre")
    )

    ([masa_filo] of Ingrediente
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "masa_filo")
    )

    ([masa_quebrada] of Ingrediente
         (glutenFree  FALSE)
         (lactosaFree  FALSE)
         (nombre  "masa_quebrada")
    )

    ([mejillones] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "mejillones")
    )

    ([merlot] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "merlot")
         (precio  14.0)
    )

    ([mezclado] of Preparacion
         (nombre  "mezclado")
    )

    ([miel] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "miel")
    )

    ([minestrone] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [calabacin] [cebolla] [judias] [zanahoria])
         (disponibleEn  [invierno] [otono])
         (esCategoria  [vegetariano])
         (esPreparacion  [hervido])
         (incompatibleConBebida  [batido_de_chocolate] [bebida_de_yogur] [chianti])
         (incompatibleConPlato  [fasolada] [sopa_avgolemono] [soupe_a_loignon])
         (originarioDe  [italia])
         (dificultad  5)
         (nombre  "minestrone")
         (precio  7.5)
    )

    ([montado] of Preparacion
         (nombre  "montado")
    )

    ([montado_y_tostado] of Preparacion
         (nombre  "montado_y_tostado")
    )

    ([montepulciano] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "montepulciano")
         (precio  12.0)
    )

    ([morcilla] of Ingrediente
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "morcilla")
    )

    ([mostaza_de_dijon] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "mostaza_de_dijon")
    )

    ([moules_marinieres] of Plato
         (compuestoPor  [ajo] [chalotas] [mantequilla] [mejillones] [perejil] [pimienta] [sal] [vino_blanco])
         (disponibleEn  [verano])
         (esCategoria  [mariscos])
         (esPreparacion  [vapor])
         (incompatibleConBebida  [sprite] [tinto_de_verano] [7up])
         (incompatibleConPlato  [bouillabaisse] [calamares_a_la_romana] [pulpo_a_la_gallega])
         (originarioDe  [francia])
         (dificultad  4)
         (nombre  "moules_marinieres")
         (precio  9.5)
    )

    ([moussaka] of Plato
         (compuestoPor  [ajo] [berenjena] [carne_de_res] [cebolla] [salsa_bechamel] [tomate])
         (disponibleEn  [otono] [verano])
         (esCategoria  [cazuela])
         (esPreparacion  [horneado_y_en_capas])
         (incompatibleConBebida  [agua_con_gas] [sprite] [7up])
         (incompatibleConPlato  [lasagna_alla_bolognese] [pastitsio] [tagliatelle_al_ragu])
         (originarioDe  [grecia])
         (dificultad  7)
         (nombre  "moussaka")
         (precio  9.5)
    )

    ([mozzarella] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "mozzarella")
    )

    ([muslos_de_pato] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "muslos_de_pato")
    )

    ([nata] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "nata")
    )

    ([nata_espesa] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "nata_espesa")
    )

    ([natillas_caseras] of Plato
         (compuestoPor  [azucar] [canela] [huevos] [leche] [maicena])
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (esCategoria  [postre])
         (esPreparacion  [hervido])
         (incompatibleConBebida  [cerveza_artesanal] [cerveza_rubia] [tinto_de_verano])
         (incompatibleConPlato  [crema_catalana] [creme_brulee] [panna_cotta])
         (originarioDe  [espana])
         (dificultad  3)
         (nombre  "natillas_caseras")
         (precio  3.5)
    )

    ([nuez] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "nuez")
    )

    ([nuez_moscada] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "nuez_moscada")
    )

    ([oregano] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "oregano")
    )

    ([ossobuco_alla_milanese] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [cebolla] [ternera] [vino_blanco] [zanahoria])
         (disponibleEn  [invierno])
         (esCategoria  [carne])
         (esPreparacion  [guisado])
         (incompatibleConBebida  [limonada_casera] [sprite] [7up])
         (incompatibleConPlato  [bistecca_alla_fiorentina] [kleftiko] [steak_frites])
         (originarioDe  [italia])
         (dificultad  7)
         (nombre  "ossobuco_alla_milanese")
         (precio  15.0)
    )

    ([otono] of Temporada
         (nombre  "otono")
    )

    ([paella_valenciana] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [arroz] [mariscos] [pimiento_rojo] [pollo])
         (disponibleEn  [primavera] [verano])
         (esCategoria  [arroz])
         (esPreparacion  [cocido])
         (incompatibleConBebida  [cerveza_artesanal] [lambrusco] [tinto_de_verano])
         (incompatibleConPlato  [arroz_caldoso] [arroz_con_leche] [risotto_ai_funghi])
         (originarioDe  [espana])
         (dificultad  7)
         (nombre  "paella_valenciana")
         (precio  14.5)
    )

    ([pan] of Ingrediente
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "pan")
    )

    ([panceta] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "panceta")
    )

    ([panna_cotta] of Plato
         (compuestoPor  [azucar] [gelatina] [nata] [vainilla])
         (disponibleEn  [primavera] [verano])
         (esCategoria  [dulce])
         (esPreparacion  [frio])
         (incompatibleConBebida  [fanta_naranja] [pepsi] [refresco_de_cola])
         (incompatibleConPlato  [creme_brulee] [natillas_caseras] [tiramisu])
         (originarioDe  [italia])
         (dificultad  4)
         (nombre  "panna_cotta")
         (precio  5.5)
    )

    ([parmigiana_di_melanzane] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [berenjena] [queso_parmesano] [tomate])
         (disponibleEn  [verano])
         (esCategoria  [vegetariano])
         (esPreparacion  [horno])
         (incompatibleConBebida  [cerveza_artesanal] [cerveza_rubia] [tinto_de_verano])
         (incompatibleConPlato  [escalivada] [pisto_manchego] [ratatouille])
         (originarioDe  [italia])
         (dificultad  6)
         (nombre  "parmigiana_di_melanzane")
         (precio  9.5)
    )

    ([pasta] of Ingrediente
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "pasta")
    )

    ([pasta_al_pesto_genovese] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [albahaca] [pasta] [pinones] [queso_parmesano])
         (disponibleEn  [primavera] [verano])
         (esCategoria  [pasta])
         (esPreparacion  [hervido])
         (incompatibleConBebida  [barolo] [batido_de_chocolate] [bebida_de_yogur])
         (incompatibleConPlato  [gnocchi_al_pomodoro] [pasta_alla_norma] [spaghetti_carbonara])
         (originarioDe  [italia])
         (dificultad  4)
         (nombre  "pasta_al_pesto_genovese")
         (precio  9.0)
    )

    ([pasta_alla_norma] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [berenjena] [pasta] [queso_parmesano] [tomate])
         (disponibleEn  [verano])
         (esCategoria  [pasta])
         (esPreparacion  [hervido])
         (incompatibleConBebida  [limonada_casera] [sprite] [7up])
         (incompatibleConPlato  [gnocchi_al_pomodoro] [pasta_al_pesto_genovese] [spaghetti_carbonara])
         (originarioDe  [italia])
         (dificultad  5)
         (nombre  "pasta_alla_norma")
         (precio  9.5)
    )

    ([pasta_de_tomate] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pasta_de_tomate")
    )

    ([pastel_salado] of Categoria
         (nombre  "pastel_salado")
    )

    ([pastitsio] of Plato
         (compuestoPor  [ajo] [canela] [carne_de_res] [cebolla] [pasta] [queso] [salsa_bechamel])
         (disponibleEn  [invierno] [otono])
         (esCategoria  [carne])
         (esPreparacion  [horno])
         (incompatibleConBebida  [albarino] [fanta_naranja] [txakoli] [verdicchio] [7up])
         (incompatibleConPlato  [lasagna_alla_bolognese] [moussaka] [tagliatelle_al_ragu])
         (originarioDe  [grecia])
         (dificultad  6)
         (nombre  "pastitsio")
         (precio  8.5)
    )

    ([patatas] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "patatas")
    )

    ([pato] of Categoria
         (nombre  "pato")
    )

    ([pecorino] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "pecorino")
    )

    ([pepino] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pepino")
    )

    ([pepsi] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pepsi")
         (precio  2.5)
    )

    ([perejil] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "perejil")
    )

    ([pescado] of Categoria
         (nombre  "pescado")
    )

    ([pescados_variados] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pescados_variados")
    )

    ([pimenton] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pimenton")
    )

    ([pimienta] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pimienta")
    )

    ([pimienta_negra] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pimienta_negra")
    )

    ([pimiento_rojo] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pimiento_rojo")
    )

    ([pinones] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pinones")
    )

    ([pistacho] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pistacho")
    )

    ([pisto_manchego] of Plato
         (compuestoPor  [aceite_de_oliva] [berenjena] [calabacin] [pimiento_rojo] [tomate])
         (disponibleEn  [otono] [verano])
         (esCategoria  [vegetariano])
         (esPreparacion  [cocido])
         (incompatibleConBebida  [cerveza_artesanal] [cerveza_rubia] [txakoli])
         (incompatibleConPlato  [escalivada] [parmigiana_di_melanzane] [ratatouille])
         (originarioDe  [espana])
         (dificultad  5)
         (nombre  "pisto_manchego")
         (precio  8.0)
    )

    ([pita] of Ingrediente
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "pita")
    )

    ([pollo] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pollo")
    )

    ([pollo_alla_cacciatora] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [pollo] [tomate] [vino_tinto])
         (disponibleEn  [invierno] [otono])
         (esCategoria  [carne])
         (esPreparacion  [guisado])
         (incompatibleConBebida  [agua_con_gas] [agua_sin_gas] [limonada_casera])
         (incompatibleConPlato  [coq_au_vin] [sopa_avgolemono] [souvlaki])
         (originarioDe  [italia])
         (dificultad  5)
         (nombre  "pollo_alla_cacciatora")
         (precio  11.5)
    )

    ;;; Relación para indicar el postre de un menú.
    ([postre] of Categoria
         (nombre  "postre")
    )

    ([primavera] of Temporada
         (nombre  "primavera")
    )

    ([puerro] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "puerro")
    )

    ([pulpo] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "pulpo")
    )

    ([pulpo_a_la_gallega] of Plato
         (compuestoPor  [aceite_de_oliva] [pimenton] [pulpo] [sal_gruesa])
         (disponibleEn  [otono])
         (esCategoria  [mariscos])
         (esPreparacion  [cocido])
         (incompatibleConBebida  [fanta_naranja] [sprite] [7up])
         (incompatibleConPlato  [bouillabaisse] [calamares_a_la_romana] [moules_marinieres])
         (originarioDe  [espana])
         (dificultad  6)
         (nombre  "pulpo_a_la_gallega")
         (precio  13.0)
    )

    ([queso] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "queso")
    )

    ([queso_de_cabra] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "queso_de_cabra")
    )

    ([queso_feta] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "queso_feta")
    )

    ([queso_gruyere] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "queso_gruyere")
    )

    ([queso_manchego] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "queso_manchego")
    )

    ([queso_mascarpone] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "queso_mascarpone")
    )

    ([queso_parmesano] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "queso_parmesano")
    )

    ([quesoc] of Categoria
         (nombre  "quesoc")
    )

    ([quiche_lorraine] of Plato
         (compuestoPor  [huevos] [lardones] [masa_quebrada] [nata] [nuez_moscada] [pimienta] [queso_gruyere] [sal])
         (disponibleEn  [otono] [primavera] [verano])
         (esCategoria  [pastel_salado])
         (esPreparacion  [horno])
         (incompatibleConBebida  [agua_con_gas] [limonada_casera] [tinto_de_verano])
         (incompatibleConPlato  [empanada_gallega] [spanakopita] [tiropita])
         (originarioDe  [francia])
         (dificultad  3)
         (nombre  "quiche_lorraine")
         (precio  6.5)
    )

    ([ralladura_de_naranja] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "ralladura_de_naranja")
    )

    ([ratatouille] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [albahaca] [berenjena] [calabacin] [cebolla] [pimienta] [pimiento_rojo] [sal] [tomate] [tomillo])
         (disponibleEn  [verano])
         (esCategoria  [vegetariano])
         (esPreparacion  [estofado])
         (incompatibleConBebida  [batido_de_chocolate] [bebida_de_yogur] [cerveza_rubia])
         (incompatibleConPlato  [escalivada] [parmigiana_di_melanzane] [pisto_manchego])
         (originarioDe  [francia])
         (dificultad  5)
         (nombre  "ratatouille")
         (precio  7.0)
    )

    ([refresco_de_cola] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "refresco_de_cola")
         (precio  2.0)
    )

    ([rehogado] of Preparacion
         (nombre  "rehogado")
    )

    ([rehogado_y_gratinado] of Preparacion
         (nombre  "rehogado_y_gratinado")
    )

    ([revuelto_de_setas] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [champinones] [huevos])
         (disponibleEn  [otono])
         (esCategoria  [huevo])
         (esPreparacion  [a_la_plancha])
         (incompatibleConBebida  [barolo] [cabernet_sauvignon] [zinfandel])
         (incompatibleConPlato  [frittata_di_zucchine] [huevos_rotos_con_jamon] [tortilla_de_patatas])
         (originarioDe  [espana])
         (dificultad  4)
         (nombre  "revuelto_de_setas")
         (precio  8.0)
    )

    ([ricotta] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "ricotta")
    )

    ([risotto_ai_funghi] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [arroz] [caldo_de_verduras] [champinones] [queso_parmesano])
         (disponibleEn  [otono])
         (esCategoria  [arroz])
         (esPreparacion  [guisado])
         (incompatibleConPlato  [arroz_caldoso] [arroz_con_leche] [paella_valenciana])
         (originarioDe  [italia])
         (dificultad  6)
         (nombre  "risotto_ai_funghi")
         (precio  12.0)
    )

    ([romero] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "romero")
    )

    ([roquefort] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "roquefort")
    )

    ([sal] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "sal")
    )

    ([sal_gruesa] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "sal_gruesa")
    )

    ([salade_de_chevre_chaud] of Plato
         (compuestoPor  [aceite_de_oliva] [baguette] [hojas_verdes] [miel] [mostaza_de_dijon] [nuez] [pimienta] [queso_de_cabra] [sal] [vinagre_balsamico])
         (disponibleEn  [primavera] [verano])
         (esCategoria  [vegetariano])
         (esPreparacion  [montado_y_tostado])
         (incompatibleConBebida  [batido_de_chocolate] [bebida_de_yogur] [cerveza_rubia])
         (incompatibleConPlato  [assiette_de_fromages] [spanakopita] [tiropita])
         (originarioDe  [francia])
         (dificultad  1)
         (nombre  "salade_de_chevre_chaud")
         (precio  7.5)
    )

    ([salmorejo_cordobes] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [huevos] [pan] [tomate])
         (disponibleEn  [verano])
         (esCategoria  [vegetariano])
         (esPreparacion  [crudo])
         (incompatibleConBebida  [batido_de_chocolate] [bebida_de_yogur] [merlot])
         (incompatibleConPlato  [gazpacho_andaluz] [sopa_avgolemono] [soupe_a_loignon])
         (originarioDe  [espana])
         (dificultad  4)
         (nombre  "salmorejo_cordobes")
         (precio  6.0)
    )

    ([salsa_bechamel] of Ingrediente
         (glutenFree  FALSE)
         (lactosaFree  FALSE)
         (nombre  "salsa_bechamel")
    )

    ([saltimbocca_alla_romana] of Plato
         (compuestoPor  [aceite_de_oliva] [jamon_serrano] [salvia] [ternera] [vino_blanco])
         (disponibleEn  [primavera] [verano])
         (esCategoria  [carne])
         (esPreparacion  [a_la_plancha])
         (incompatibleConBebida  [agua_con_gas] [agua_sin_gas] [cerveza_sin_alcohol])
         (incompatibleConPlato  [bistecca_alla_fiorentina] [ossobuco_alla_milanese] [vitello_tonnato])
         (originarioDe  [italia])
         (dificultad  6)
         (nombre  "saltimbocca_alla_romana")
         (precio  13.0)
    )

    ([salvia] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "salvia")
    )

    ([sauvignon_blanc] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "sauvignon_blanc")
         (precio  18.0)
    )

    ([sellado_curado] of Preparacion
         (nombre  "sellado_curado")
    )

    ([semifreddo_de_limon] of Plato
         (compuestoPor  [azucar] [huevos] [limon] [nata] [vainilla])
         (disponibleEn  [primavera] [verano])
         (esCategoria  [dulce])
         (esPreparacion  [frio])
         (incompatibleConBebida  [barolo] [chianti] [merlot])
         (incompatibleConPlato  [creme_brulee] [panna_cotta] [tiramisu])
         (originarioDe  [italia])
         (dificultad  5)
         (nombre  "semifreddo_de_limon")
         (precio  5.0)
    )

    ([semola] of Ingrediente
         (glutenFree  FALSE)
         (lactosaFree  TRUE)
         (nombre  "semola")
    )

    ([sopa] of Categoria
         (nombre  "sopa")
    )

    ([sopa_avgolemono] of Plato
         (compuestoPor  [arroz] [caldo_de_verduras] [huevos] [limon] [pollo])
         (disponibleEn  [invierno])
         (esCategoria  [sopa])
         (esPreparacion  [cocido_y_batido])
         (incompatibleConBebida  [sauvignon_blanc] [tinto_de_verano] [txakoli])
         (incompatibleConPlato  [gazpacho_andaluz] [salmorejo_cordobes] [soupe_a_loignon])
         (originarioDe  [grecia])
         (dificultad  3)
         (nombre  "sopa_avgolemono")
         (precio  4.5)
    )

    ([soupe_a_loignon] of Plato
         (compuestoPor  [baguette] [caldo_de_carne] [cebolla] [laurel] [mantequilla] [pimienta] [queso_gruyere] [sal] [tomillo] [vino_blanco])
         (disponibleEn  [invierno] [otono])
         (esCategoria  [sopa])
         (esPreparacion  [rehogado_y_gratinado])
         (incompatibleConBebida  [sprite] [tinto_de_verano] [7up])
         (incompatibleConPlato  [gazpacho_andaluz] [minestrone] [salmorejo_cordobes])
         (originarioDe  [francia])
         (dificultad  4)
         (nombre  "soupe_a_loignon")
         (precio  5.5)
    )

    ([souvlaki] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [carne_de_cerdo] [cebolla] [limon] [oregano] [pita] [pollo] [tomate])
         (disponibleEn  [primavera] [verano])
         (esCategoria  [carne])
         (esPreparacion  [a_la_parrilla])
         (incompatibleConBebida  [barolo] [cabernet_sauvignon] [chardonnay])
         (incompatibleConPlato  [coq_au_vin] [kleftiko] [pollo_alla_cacciatora])
         (originarioDe  [grecia])
         (dificultad  3)
         (nombre  "souvlaki")
         (precio  6.0)
    )

    ([spaghetti_carbonara] of Plato
         (compuestoPor  [huevos] [panceta] [pasta] [pimienta_negra] [queso_parmesano])
         (disponibleEn  [otono] [primavera])
         (esCategoria  [pasta])
         (esPreparacion  [hervido])
         (incompatibleConBebida  [fanta_naranja] [pepsi] [refresco_de_cola])
         (incompatibleConPlato  [gnocchi_al_pomodoro] [pasta_al_pesto_genovese] [pasta_alla_norma])
         (originarioDe  [italia])
         (dificultad  5)
         (nombre  "spaghetti_carbonara")
         (precio  10.5)
    )

    ([spanakopita] of Plato
         (compuestoPor  [aceite_de_oliva] [cebolla] [eneldo] [espinacas] [masa_filo] [queso_feta])
         (disponibleEn  [primavera] [verano])
         (esCategoria  [vegetariano])
         (esPreparacion  [horno])
         (incompatibleConBebida  [cerveza_artesanal] [merlot] [zinfandel])
         (incompatibleConPlato  [empanada_gallega] [quiche_lorraine] [tiropita])
         (originarioDe  [grecia])
         (dificultad  5)
         (nombre  "spanakopita")
         (precio  5.5)
    )

    ([sprite] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "sprite")
         (precio  2.5)
    )

    ([steak_frites] of Plato
         (compuestoPor  [aceite_de_oliva] [carne_de_res] [patatas] [pimienta] [sal])
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (esCategoria  [carne])
         (esPreparacion  [asado_y_frito])
         (incompatibleConBebida  [agua_sin_gas] [cerveza_sin_alcohol] [limonada_casera])
         (incompatibleConPlato  [bistecca_alla_fiorentina] [kleftiko] [ossobuco_alla_milanese])
         (originarioDe  [francia])
         (dificultad  3)
         (nombre  "steak_frites")
         (precio  11.0)
    )

    ([tagliatelle_al_ragu] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [carne_picada] [cebolla] [pasta] [tomate])
         (disponibleEn  [invierno] [otono])
         (esCategoria  [pasta])
         (esPreparacion  [hervido])
         (incompatibleConBebida  [fanta_naranja] [pepsi] [sprite])
         (incompatibleConPlato  [lasagna_alla_bolognese] [moussaka] [pastitsio])
         (originarioDe  [italia])
         (dificultad  5)
         (nombre  "tagliatelle_al_ragu")
         (precio  11.0)
    )

    ([tarta_de_santiago] of Plato
         (compuestoPor  [almendra] [azucar] [canela] [huevos] [limon])
         (disponibleEn  [verano])
         (esCategoria  [postre])
         (esPreparacion  [horno])
         (incompatibleConBebida  [fanta_naranja] [pepsi] [refresco_de_cola])
         (incompatibleConPlato  [baklava] [cannoli_siciliani] [torta_caprese])
         (originarioDe  [espana])
         (dificultad  5)
         (nombre  "tarta_de_santiago")
         (precio  5.0)
    )

    ([tarte_tatin] of Plato
         (compuestoPor  [azucar] [canela] [mantequilla] [manzana] [masa_de_hojaldre])
         (disponibleEn  [otono])
         (esCategoria  [postre])
         (esPreparacion  [caramelizado_y_horneado])
         (incompatibleConBebida  [fanta_naranja] [pepsi] [refresco_de_cola])
         (incompatibleConPlato  [galaktoboureko] [loukoumades] [torrijas])
         (originarioDe  [francia])
         (dificultad  7)
         (nombre  "tarte_tatin")
         (precio  6.5)
    )

    ([ternera] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "ternera")
    )

    ([tinto_de_verano] of Casual
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "tinto_de_verano")
         (precio  3.0)
    )

    ([tinto_ribera_del_duero] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "tinto_ribera_del_duero")
         (precio  14.0)
    )

    ([tinto_rioja] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "tinto_rioja")
         (precio  12.5)
    )

    ([tiramisu] of Plato
         (compuestoPor  [azucar] [bizcocho] [cacao_en_polvo] [cafe] [huevos] [queso_mascarpone])
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (esCategoria  [dulce])
         (esPreparacion  [frio])
         (incompatibleConBebida  [cerveza_artesanal] [cerveza_rubia] [tinto_de_verano])
         (incompatibleConPlato  [cannoli_siciliani] [panna_cotta] [semifreddo_de_limon])
         (originarioDe  [italia])
         (dificultad  5)
         (nombre  "tiramisu")
         (precio  6.5)
    )

    ([tiropita] of Plato
         (compuestoPor  [aceite_de_oliva] [huevos] [masa_filo] [queso_feta])
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (esCategoria  [vegetariano])
         (esPreparacion  [horno])
         (incompatibleConBebida  [cabernet_sauvignon] [chianti] [merlot])
         (incompatibleConPlato  [empanada_gallega] [quiche_lorraine] [spanakopita])
         (originarioDe  [grecia])
         (dificultad  4)
         (nombre  "tiropita")
         (precio  5.0)
    )

    ([tomate] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "tomate")
    )

    ([tomillo] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "tomillo")
    )

    ([torrijas] of Plato
         (compuestoPor  [azucar] [canela] [huevos] [leche] [pan])
         (disponibleEn  [primavera])
         (esCategoria  [postre])
         (esPreparacion  [frito])
         (incompatibleConBebida  [chardonnay] [merlot] [sauvignon_blanc])
         (incompatibleConPlato  [galaktoboureko] [loukoumades] [tarte_tatin])
         (originarioDe  [espana])
         (dificultad  4)
         (nombre  "torrijas")
         (precio  3.0)
    )

    ([torta_caprese] of Plato
         (compuestoPor  [almendra] [azucar] [chocolate] [huevos] [mantequilla])
         (disponibleEn  [invierno] [otono])
         (esCategoria  [dulce])
         (esPreparacion  [horno])
         (incompatibleConBebida  [cerveza_sin_alcohol] [sprite] [7up])
         (incompatibleConPlato  [baklava] [cannoli_siciliani] [tarta_de_santiago])
         (originarioDe  [italia])
         (dificultad  6)
         (nombre  "torta_caprese")
         (precio  7.0)
    )

    ([tortilla_de_patatas] of Plato
         (compuestoPor  [aceite_de_oliva] [cebolla] [huevos])
         (disponibleEn  [invierno] [otono] [primavera] [verano])
         (esCategoria  [huevo])
         (esPreparacion  [frito])
         (incompatibleConBebida  [cabernet_sauvignon] [chianti] [zinfandel])
         (incompatibleConPlato  [frittata_di_zucchine] [huevos_rotos_con_jamon] [revuelto_de_setas])
         (originarioDe  [espana])
         (dificultad  4)
         (nombre  "tortilla_de_patatas")
         (precio  7.0)
    )

    ([trufa] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "trufa")
    )

    ([txakoli] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "txakoli")
         (precio  11.0)
    )

    ([tzatziki] of Plato
         (compuestoPor  [aceite_de_oliva] [ajo] [eneldo] [limon] [pepino] [yogur_griego])
         (disponibleEn  [verano])
         (esCategoria  [dip])
         (esPreparacion  [mezclado])
         (incompatibleConBebida  [batido_de_chocolate] [bebida_de_yogur] [chianti])
         (incompatibleConPlato  [assiette_de_fromages] [gazpacho_andaluz] [salmorejo_cordobes])
         (originarioDe  [grecia])
         (dificultad  2)
         (nombre  "tzatziki")
         (precio  3.0)
    )

    ([vainilla] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "vainilla")
    )

    ([vapor] of Preparacion
         (nombre  "vapor")
    )

    ([vegetariano] of Categoria
         (nombre  "vegetariano")
    )

    ([verano] of Temporada
         (nombre  "verano")
    )

    ([verdicchio] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "verdicchio")
         (precio  13.0)
    )

    ([vinagre_balsamico] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "vinagre_balsamico")
    )

    ([vino_blanco] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "vino_blanco")
    )

    ([vino_dulce] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "vino_dulce")
    )

    ([vino_tinto] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "vino_tinto")
    )

    ([vitello_tonnato] of Plato
         (compuestoPor  [aceite_de_oliva] [alcaparras] [atun] [huevos] [ternera])
         (disponibleEn  [verano])
         (esCategoria  [carne])
         (esPreparacion  [hervido])
         (incompatibleConBebida  [cerveza_sin_alcohol] [pepsi] [refresco_de_cola])
         (incompatibleConPlato  [bistecca_alla_fiorentina] [ossobuco_alla_milanese] [saltimbocca_alla_romana])
         (originarioDe  [italia])
         (dificultad  6)
         (nombre  "vitello_tonnato")
         (precio  14.0)
    )

    ([yogur_griego] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  FALSE)
         (nombre  "yogur_griego")
    )

    ([zanahoria] of Ingrediente
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "zanahoria")
    )

    ([zinfandel] of Vino
         (alcoholica  TRUE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "zinfandel")
         (precio  16.5)
    )

    ([7up] of Casual
         (alcoholica  FALSE)
         (glutenFree  TRUE)
         (lactosaFree  TRUE)
         (nombre  "7up")
         (precio  2.5)
    )

)

;; Para almacenar las preferencias y restricciones del usuario.
(defclass Preferencias
     (is-a USER)
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

(defmodule MAIN (export ?ALL))


(defmodule RicoRico_Filtrado
    (import MAIN ?ALL)
    (export ?ALL)
)

(deffunction MAIN::escribir_instancias ()
    (bind ?lista1 (find-all-instances ((?i Plato)) TRUE))
    (bind ?lista2 (find-all-instances ((?i Bebida)) TRUE))
    (printout t "Platos: " (length$ ?lista1) crlf)
    (do-for-all-instances ((?i Plato))
        TRUE
        (printout t "Nombre Plato: " (instance-name ?i) crlf)
    )
    (printout t "Bebidas: " (length$ ?lista2) crlf)
    (do-for-all-instances ((?i Bebida))
        TRUE
        (printout t "Nombre Bebida: " (instance-name ?i) crlf)
    )
)

(defrule MAIN::inicio 
    (declare (salience 20)) 
    => 
    (printout t "Bienvenido al sistema de filtrado." crlf)
    (make-instance prefs of Preferencias
          (num_comensales 101)
          (precio_min 1)
          (precio_max 3)
          (temporada verano)
          (alcoholica TRUE)
          (vino FALSE)
          (diferentesBebidas TRUE)
          (intolerancia_gluten FALSE)
          (intolerancia_lactosa TRUE)
     )
     (focus RicoRico_Filtrado)
)

(deffunction RicoRico_Filtrado::eliminar_platos_complejos ()

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
     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (do-for-all-instances ((?bebida Bebida))
          (if (not (eq (send ?bebida get-alcoholica) (send ?pref get-alcoholica))) then
               (send ?bebida delete)
          )   
     )
)

(deffunction RicoRico_Filtrado::eliminar_vinos ()
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

     (bind ?pref (nth$ 1 (find-instance ((?p Preferencias)) TRUE)))
     (bind ?gluten (send ?pref get-intolerancia_gluten))

     (if ?gluten then

          (bind ?lista1 (find-all-instances ((?l1 Plato)) TRUE))
          (foreach ?plato ?lista1
               (bind ?ingredienteList (send ?plato get-compuestoPor))
               (bind ?delete FALSE)

               (foreach ?ingrediente ?ingredienteList
                    (printout t ?ingrediente crlf)
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
     (declare (salience 10))
     =>
     (eliminar_bebidas_alcoholicas)
     (eliminar_bebidas_gluten)
     (eliminar_bebidas_lactosa)
     (eliminar_platos_complejos)
     (eliminar_platos_fuera_temporada)
     (eliminar_platos_gluten)
     (eliminar_platos_lactosa)
     (eliminar_vinos)
)

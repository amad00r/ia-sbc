;;; ---------------------------------------------------------
;;; ricorico.clp
;;; Translated by owl2clips
;;; Translated to CLIPS from ontology ricorico.ttl
;;; :Date 30/04/2025 19:21:47

(defclass Bebida "Clase para representar una bebida."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
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
    ;;; Atributo de tipo Booleano para indicar si una bebida casual es alcohólica o no.
    (slot alcoholica
        (type SYMBOL)
        (create-accessor read-write))
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
    ;;; Relación para indicar que bebidas son compatibles con un plato.
    (multislot compatibleConBebida
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Relación para indicar que platos son compatibles con un plato concreto.
    (multislot compatibleConPlato
        (type INSTANCE)
        (create-accessor read-write))
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
    ;;; Relación para indicar el país de origen de un plato.
    (slot originarioDe
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Atributo de tipo Integer para indicar la dificultad de realizar un plato. 
Su rango va de 1 a 10, siendo 1 el valor que indica menor dificultad y 10 el valor que indica la máxima dificultad.
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
         (nombre  "vinoMoc")
         (precio  10.5)
    )

    ([mocBebida2] of Casual
         (alcoholica  "true")
         (nombre  "cocacolaMoc")
         (precio  1.2)
    )

    ([mocCategoria] of Categoria
         (nombre  "VegetarianoMoc")
    )

    ([mocIngrediente1] of Ingrediente
         (disponibleEn  [mocTemporada] [mocTemporada2])
         (nombre  "ArrozMoc")
    )

    ([mocIngrediente2] of Ingrediente
         (disponibleEn  [mocTemporada])
         (nombre  "LechugaMoc")
    )

    ([mocMenu] of Menu
         (1rBebida  [mocBebida1])
         (1rPlato  [mocPlato1])
         (2oBebida  [mocBebida2])
         (2oPlato  [mocPlato2])
         (postre  [mocPostre])
    )

    ([mocOrigen] of Origen
         (nombre  "EspañaMoc")
    )

    ([mocPlato1] of Plato
         (compatibleConBebida  [mocBebida1] [mocBebida2])
         (compatibleConPlato  [mocPlato2] [mocPostre])
         (compuestoPor  [mocIngrediente1] [mocIngrediente2])
         (esCategoria  [mocCategoria])
         (esPreparacion  [mocPreparacion])
         (originarioDe  [mocOrigen])
         (dificultad  7)
         (nombre  "Plato1Moc")
         (precio  7.23)
    )

    ([mocPlato2] of Plato
         (compuestoPor  [mocIngrediente2])
         (esCategoria  [mocCategoria])
         (esPreparacion  [mocPreparacion])
         (originarioDe  [mocOrigen])
         (dificultad  1)
         (nombre  "Plato2Moc")
         (precio  2.56)
    )

    ([mocPostre] of Plato
         (compatibleConBebida  [mocBebida1])
         (compatibleConPlato  [mocPlato1])
         (compuestoPor  [mocIngrediente1])
         (esCategoria  [mocCategoria])
         (esPreparacion  [mocPreparacion])
         (originarioDe  [mocOrigen])
         (dificultad  4 "PostreMoc")
         (precio  8.2)
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

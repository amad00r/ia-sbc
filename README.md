# Protege 5 to CLIPS

## Requisitos

- **Python 3**: Asegúrate de tener instalado Python 3 en tu sistema.
- **owl2else**: Una herramienta para convertir ontologías OWL a CLIPS.

## Comando

Para convertir un archivo OWL a formato CLIPS, utiliza el siguiente comando:

```bash
owl2clips --input ricorico.ttl --output RicoRico_Ontology.clp --format turtle
```

## Ejecución RicoRico_Entrada.clp

Sigue estos pasos para ejecutar la aplicación RicoRico en CLIPS:
```clips
; 1. Limpia el entorno de CLIPS:
(clear)

; 2. Carga todos los archivos en este orden:
(load "./RicoRico_Ontology.clp")
(load "./RicoRico_MAIN.clp")
(load "./RicoRico_Entrada.clp")
(load "./RicoRico_Filtrado.clp")
(load "./RicoRico_Generador.clp")
(load "./RicoRico_Salida.clp")

; 3. Reinicia el entorno:
(reset)

; 4. Ejecuta las reglas:
(run)
```

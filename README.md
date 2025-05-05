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

Sigue estos pasos para ejecutar el archivo `RicoRico_Entrada.clp` en CLIPS:

1. Limpia el entorno de CLIPS:
    ```clips
    (clear)
    ```

2. Carga el archivo:
    ```clips
    (load "./RicoRico_Entrada.clp")
    ```

3. Reinicia el entorno:
    ```clips
    (reset)
    ```

4. Ejecuta las reglas:
    ```clips
    (run)
    ```
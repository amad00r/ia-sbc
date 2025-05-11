import sys

def convert_bool_in_file(filename):
    try:
        # Leer el contenido del archivo
        with open(filename, 'r') as file:
            content = file.read()
        
        # Reemplazar "true" por TRUE y "false" por FALSE
        updated_content = content.replace('"true"', 'TRUE').replace('"false"', 'FALSE')
        
        # Escribir los cambios en el mismo archivo
        with open(filename, 'w') as file:
            file.write(updated_content)
        
        print(f"Archivo '{filename}' procesado correctamente.")
    except FileNotFoundError:
        print(f"Error: El archivo '{filename}' no existe.")
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Uso: python boolConverter.py <nombre_del_archivo>")
    else:
        convert_bool_in_file(sys.argv[1])
import unicodedata
import json
import re


print("(definstances instances")
with open("ricorico.json", "r") as f:
	for obj in json.load(f):
		ident = re.sub(r'\s+', '_', unicodedata.normalize("NFKD", obj["id"]).encode("ASCII", "ignore").decode("utf-8").lower())
		print(f'\t([{ident}] of ' + obj["type"])
		if obj["type"] in ("Origen", "Temporada", "Preparacion", "Categoria"):
			print(f'\t\t(nombre "{ident}")')
		elif obj["type"] == "Vino":
			print(f'\t\t(nombre "{ident}")')
			print(f'\t\t(precio  {obj["precio"]})')
		elif obj["type"] == "Casual":
			print(f'\t\t(alcoholica  \"{str(obj["alcoholica"]).lower()}\")')
			print(f'\t\t(nombre "{ident}")')
			print(f'\t\t(precio  {obj["precio"]})')
		elif obj["type"] == "Ingrediente":
			print('\t\t(disponibleEn  ' + " ".join(f"[{str(e)}]" for e in obj["idsTemporadas"]) + ')')	           
			print(f'\t\t(nombre  \"{ident}\")')
		elif obj["type"] == "Plato":
			if obj["idsBebidasCompatibles"]: 
				print('\t\t(compatibleConBebida  ' + " ".join(f"[{str(e)}]" for e in obj["idsBebidasCompatibles"]) + ')')
			if obj["idsPlatosCompatibles"]:
				print('\t\t(compatibleConPlato  ' + " ".join(f"[{str(e)}]" for e in obj["idsPlatosCompatibles"]) + ')')
			print('\t\t(compuestoPor  ' + " ".join(f"[{str(e)}]" for e in obj["idsIngredientes"]) + ')')  
			print(f'\t\t(esCategoria  [{obj["idCategoria"]}])')
			#print(f'\t\t(esPreparacion [{obj["idPreparacion"]}])')
			print(f'\t\t(originarioDe  [{obj["idOrigen"]}])')
			print(f'\t\t(dificultad  {obj["dificultad"]})')
			print(f'\t\t(nombre  \"{ident}\")')
			print(f'\t\t(precio  {obj["precio"]})')
			

		print("\t)")
print(")")
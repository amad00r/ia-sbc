# print("@prefix : <http://example.org/ricorico#> .")
# print("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .")
# print("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .")
# print("@base <http://example.org/ricorico> .")
# print('<http://example.org/ricorico> rdf:type owl:Ontology ; rdfs:comment "ontologia ricorico amadeu pol juan" .')

import unicodedata
import json
import re

with open("ricorico-instances-v2.json", "r") as f:
	for obj in json.load(f):
		ident = re.sub(r'\s+', '_', unicodedata.normalize("NFKD", obj["id"]).encode("ASCII", "ignore").decode("utf-8").lower())
		print(f':{ident} rdf:type owl:NamedIndividual ,\n\t\t\t:{obj["type"]} ;')
		if obj["type"] in ("Origen", "Temporada", "Preparacion", "Categoria"):
			print(f'\t\t:nombre "{obj["id"]}" .')
		elif obj["type"] in ("Vino", "Casual"):
			print(f'\t\t:alcoholica "{str(obj["alcoholica"]).lower()}"^^xsd:boolean ;')
			print(f'\t\t:glutenFree "{str(obj["glutenFree"]).lower()}"^^xsd:boolean ;')
			print(f'\t\t:lactosaFree "{str(obj["lactosaFree"]).lower()}"^^xsd:boolean ;')
			print(f'\t\t:nombre "{ident}" ;')
			print(f'\t\t:precio "{obj["precio"]}"^^xsd:float .')
		elif obj["type"] == "Ingrediente":
			print('\t\t:disponibleEn ' + " , ".join(f":{str(e)}" for e in obj["idsTemporadas"]) + ' ;')	           
			print(f'\t\t:glutenFree "{str(obj["glutenFree"]).lower()}"^^xsd:boolean ;')
			print(f'\t\t:lactosaFree "{str(obj["lactosaFree"]).lower()}"^^xsd:boolean ;')
			print(f'\t\t:nombre "{ident}" .')
		elif obj["type"] == "Plato":
			print('\t\t:compuestoPor ' + " , ".join(f":{str(e)}" for e in obj["idsIngredientes"]) + ' ;')  
			print(f'\t\t:esCategoria :{obj["idCategoria"]} ;')
			print(f'\t\t:esPreparacion :{obj["idPreparacion"]} ;')
			if obj["idsBebidasIncompatibles"]:
				print('\t\t:incompatibleConBebida ' + " , ".join(f":{str(e)}" for e in obj["idsBebidasIncompatibles"]) + ' ;')
			if obj["idsPlatosIncompatibles"]:
				print('\t\t:incompatibleConPlato ' + " , ".join(f":{str(e)}" for e in obj["idsPlatosIncompatibles"]) + ' ;')
			print(f'\t\t:originarioDe :{obj["idOrigen"]} ;')
			print(f'\t\t:dificultad {obj["dificultad"]} ;')
			print(f'\t\t:nombre "{ident}" ;')
			print(f'\t\t:precio "{obj["precio"]}"^^xsd:float .')
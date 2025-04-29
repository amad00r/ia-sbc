# print("@prefix : <http://example.org/ricorico#> .")
# print("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .")
# print("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .")
# print("@base <http://example.org/ricorico> .")
# print('<http://example.org/ricorico> rdf:type owl:Ontology ; rdfs:comment "ontologia ricorico amadeu pol juan" .')

import unicodedata
import json
import re

with open("ricorico.json", "r") as f:
	for obj in json.load(f):
		if obj["type"] in ("Origen", "Temporada", "Preparacion"):
			ident = re.sub(r'\s+', '_', unicodedata.normalize("NFKD", obj["id"]).encode("ASCII", "ignore").decode("utf-8").lower())
			print(f':{ident} rdf:type owl:NamedIndividual , :{obj["type"]} ; :nombre "{obj["id"]}" .')

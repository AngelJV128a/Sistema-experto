import re
from pyswip import Prolog

def character_name_formater(personaje:str) -> str:
    personaje = personaje.strip()  # Removes leading and trailing spaces
    personaje = re.sub(r'\s+', ' ', personaje)  # Replaces multiple spaces with a single space
    personaje = personaje.replace(' ', '_')  # Replaces spaces with underscores 
    
    return personaje.lower()


def character_exists(personaje:str) -> bool:
    prolog = Prolog()
    prolog.consult("p4 (1).pl")
    
    # Check if the character exists in the Prolog database
    query = f"personaje({character_name_formater(personaje)}" + ",_" * 9 + ")."
    result = list(prolog.query(query))
    
    return len(result) > 0

print(character_name_formater("Bowser  JR"))

print("El personaje existe" if character_exists("Mega Man") else "El personaje no existe")
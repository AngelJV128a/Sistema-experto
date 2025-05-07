import re

def character_name_formater(personaje:str):
    personaje = personaje.strip()  # Removes leading and trailing spaces
    personaje = re.sub(r'\s+', ' ', personaje)  # Replaces multiple spaces with a single space
    personaje = personaje.replace(' ', '_')  # Replaces spaces with underscores 
    
    return personaje.lower()

print(character_name_formater("Bowser  JR"))
from flask import Flask, request, jsonify, render_template
from flask_cors import CORS  
from pyswip import Prolog
import os
from character_validation_operations import *

app = Flask(__name__)
CORS(app) 

@app.route('/')
def home():
    return render_template('index.html')


@app.route('/recomendar', methods=['POST'])
def recomendar():
    data = request.get_json()
    print(type(data))

    # Validar que vengan todos los parámetros
    expected_keys = ['Enemigo', 'Vel', 'Alc', 'Fue', 'Dif', 'Tip', 'Pes', 'Rec', 'Pro', 'Aer']
    if not all(key in data for key in expected_keys) or any(not valor for valor in data.values()):
        return jsonify({"recomendacion": "fallida", "motivo": "Faltan parámetros"}), 200

    try:
        prolog = Prolog()
        # Cargar archivo Prolog (ajusta el path si es necesario)
        prolog.consult("p4 (1).pl")

        if not character_exists(data['Enemigo']):
            return jsonify({"recomendacion": "fallida", "motivo": "El personaje enemigo no existe"}), 200

        # Ejecutar la cláusula
        consulta = f"recomendar_personaje({character_name_formater(data['Enemigo'])}, {data['Vel']}, {data['Alc']}, {data['Fue']}, {data['Dif']}, {data['Tip']}, {data['Pes']}, {data['Rec']}, {data['Pro']}, {data['Aer']},MejorPersonaje,Detalles,EstadisticasStr)"
        result = list(prolog.query(consulta))

        if result:
            return jsonify({"recomendacion": "exitosa",
                            "mejor_personaje":result[0]['MejorPersonaje'],
                           "detalles":result[0]['Detalles'], "Estadisticas":result[0]['EstadisticasStr']})
        else:
            return jsonify({"recomendacion": "fallida"})

    except Exception as e:
        return jsonify({"recomendacion": "fallida", "error": str(e)}), 500

if __name__ == '__main__':
    app.run(debug=True)

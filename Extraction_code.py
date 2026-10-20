#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug 19 14:12:45 2024

@author: statistics
"""

#pip install praat-parselmouth
#pip install -U scikit-learn

#pip install praatio==4.4.0  #VERSION DE PRAATIO que soporta tgio, esta version es adecuada para la obtencion de datos

import os
import pandas as pd
import parselmouth
from collections import defaultdict
from praatio import tgio

#FUNCTIONS

'''
An (auxiliary) function that FLATTENS a list that may contain sublists, returning a flat list without sublists.
'''
def aplanar_lista(lista):
    resultado = []
    for elemento in lista:
        if isinstance(elemento, list):
            resultado.extend(aplanar_lista(elemento))
        else:
            resultado.append(elemento)
    return resultado

'''
A function that finds the lowest level of analysis in the text grid given its location
The file_path parameter must have the following format: file_path="/content/audios/1MMFS"
'''
def last_tier(file_path):
  textgrid_path=file_path+ ".TextGrid"
  tg = tgio.openTextgrid(textgrid_path)
  tier_names = list(tg.tierDict.keys())
  if tier_names:
      last_tier = tier_names[-1]
      return last_tier
  else:
      return None

'''
Function for INTERVAL analysis.
The parameters sound, intensity, pitch, and formant are objects of that type (Pratt type), and start and stop are the boundaries of the interval of interest.
'''
def extract_interval_features(sound, intensity, pitch, formant, start, stop):

  #Intensity
  mean_intensity = parselmouth.praat.call(intensity, "Get mean", start, stop, 'energy')
  max_intensity = parselmouth.praat.call(intensity, "Get maximum", start, stop, "Parabolic")

  #Pitch
  mean_pitch = parselmouth.praat.call(pitch, "Get mean", start, stop, "Hertz")
  max_pitch = parselmouth.praat.call(pitch, "Get maximum", start, stop, "Hertz", "Parabolic")
  min_pitch = parselmouth.praat.call(pitch, "Get minimum", start, stop, "Hertz", "Parabolic")
  median_pitch = parselmouth.praat.call(pitch, "Get quantile", start, stop, 0.5, "Hertz")

  #Spectrum
  s1=sound.extract_part(from_time=start, to_time=stop)#Extract audio from a range
  sp1=s1.to_spectrum()
  center= parselmouth.praat.call(sp1, "Get centre of gravity",2)
  sd= parselmouth.praat.call(sp1, "Get standard deviation",2)
  sk= parselmouth.praat.call(sp1, "Get skewness",2)
  kur= parselmouth.praat.call(sp1, "Get kurtosis",2)

  #Formants
  formant_means = [parselmouth.praat.call(formant, "Get mean", i, start, stop, "Hertz") for i in range(1, 5)]
  formant_maxs = [parselmouth.praat.call(formant, "Get maximum", i, start, stop, "Hertz", "Parabolic") for i in range(1, 5)]
  formant_mins = [parselmouth.praat.call(formant, "Get minimum", i, start, stop, "Hertz", "Parabolic") for i in range(1, 5)]
  formant_medians = [parselmouth.praat.call(formant, "Get quantile", i, start, stop, "Hertz", 0.5) for i in range(1, 5)]

  return start, stop, stop-start, mean_intensity, max_intensity, mean_pitch, max_pitch, min_pitch, median_pitch, center, sd, sk, kur, formant_means, formant_maxs, formant_mins, formant_medians


'''
Function for observational analysis (file) with location
'''
def process_file(file_path):
    base_name = os.path.basename(file_path)  # Get the filename without the path
    name = os.path.splitext(base_name)[0]
    tg = tgio.openTextgrid(file_path + ".TextGrid")
    sound = parselmouth.Sound(file_path + ".wav")
    intensity = sound.to_intensity(minimum_pitch=75, time_step=None, subtract_mean=True)
# SELECT THE PITCH AND FORMANT TYPE BASED ON THE SPEAKER SAMPLE; THE DEFAULT IS “WOMEN”    pitch = sound.to_pitch(pitch_floor=100, pitch_ceiling=500)# Female
    #pitch = sound.to_pitch(pitch_floor=75, pitch_ceiling=300)# Male
    formant = parselmouth.praat.call(sound, "To Formant (burg)", 0.005, 5, 5500, 0.025, 50) #Female
    #formant = parselmouth.praat.call(sound, "To Formant (burg)", 0.005, 5, 5000, 0.025, 50) #Male

    interval_features = []

    word_tier = tg.tierDict[last_tier(file_path)]

    # interval analysis
    for start, stop, label in word_tier.entryList:
        interval_features.append([name, label]+ aplanar_lista(list(extract_interval_features(sound, intensity, pitch, formant, start, stop))))

    # Define the column names for the DataFrame
    column_names = ['name','Label','Start', 'Stop','Duration', 'Mean_Intensity', 'Max_Intensity',
                    'Mean_Pitch', 'Max_Pitch', 'Min_Pitch', 'Median_Pitch',
                    'Center_spectrum', 'SD_spectrum', 'SK_spectrum', 'KUR__spectrum',
                    'F1_Mean', 'F2_Mean', 'F3_Mean', 'F4_Mean',
                    'F1_Max', 'F2_Max', 'F3_Max', 'F4_Max',
                    'F1_Min', 'F2_Min', 'F3_Min', 'F4_Min',
                    'F1_Median', 'F2_Median', 'F3_Median', 'F4_Median']

    return pd.DataFrame(interval_features, columns=column_names)

'''
Function for analyzing a sample (file) based on its location (audio folder directory).
'''
def process_sample(directory):
    EXTENSIONS = {'.wav', '.TextGrid'}
   # Dictionary for Counting Files by Observation
    grouped_files = defaultdict(int)

    for f in os.listdir(directory):
        nombre, ext = os.path.splitext(os.path.join(directory, f))
        if ext in EXTENSIONS:
            grouped_files[nombre] += 1

    files = list(grouped_files.keys())  #List containing the base path (without the extension) of all observations. Example: /content/audios/1MMFS
                        #This path is in the input format for the “last_tier” and “process_file” functions
    general = []

    # Generate a CSV file for each observation (file)
    for file_path in files:
        df = process_file(file_path)
        general.append(df)
        df.to_csv(file_path + "_data.csv", index=False)

    # Generate a General CSV File
    df_general = general[0]  # This DataFrame will contain information on all
    for i in range(1, len(general)):
        df_general = pd.concat([df_general, general[i]])

    df_general.to_csv(os.path.join(directory, "General.csv"), index=False)

'''
Reset to the original files in the base folder (deletes the files created during extraction)
'''
def reset_archivos(path):
  EXTENSIONS = {'.wav', '.TextGrid'}
  if not os.path.isdir(path):
      print("La ruta proporcionada no es un directorio válido.")
      return

  archivos = os.listdir(path)
  for archivo in archivos:
      archivo_path = os.path.join(path, archivo)
      if os.path.isfile(archivo_path) and os.path.splitext(archivo)[1] not in EXTENSIONS:
          os.remove(archivo_path)
          print(f"Se eliminó el archivo: {archivo_path}")

#DATA EXTRACTION
directory='/Users/statistics/Desktop/Unificado' # test is the folder containing the initial data (samples)
process_sample(directory)
# -*- coding: utf-8 -*-
"""Function _find_filter_duplicates.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1HSjbA1ymNheDyBoI2k-_wRxVNMoOiMVs
"""
import pandas as pd
import numpy as np

def find_filter_duplicates(df, id_column, date_column, question_metadata):
#   if df[date_column].dtype != "datetime64[ns, UTC]":
#     return print("The function requires a date/time column with data in the format 'datetime64'.")
  
  survey_all = df.groupby(id_column).filter(lambda x: len(x)==1)
  survey_dupes = df.groupby(id_column).filter(lambda x: len(x)>1)
  
  ## Getting question MetaData for determining order of occurance in the survey
  # Survey Meta Data File (page, question_id, question_label, question_text, question_type, wave_1, wave_2, wave_3)
  #question_metadata= pd.read_csv(filepath_question_metadata, sep=',', error_bad_lines=False).reset_index().rename(columns={'index':'Position'})
  question_metadata= question_metadata.reset_index().rename(columns={'index':'Position'})

  ## Positionality of Questions Dictionary
  question_positions = question_metadata[['page']].drop_duplicates().reset_index(drop=True).reset_index().rename(columns={'index':'Position'})
  positions_dict = dict(zip(question_positions.page, question_positions.Position))

  # create object into which to put acceptable entries
  entries = pd.DataFrame()

  ## Group entries by Participant ID

  grouped_dupes = survey_dupes.groupby(id_column)

  for group_name, df_grouped in grouped_dupes:
    #print('\nCREATE TABLE {}('.format(group_name))
    
    if len(df_grouped[(df_grouped.dispcode == 31) | (df_grouped.dispcode == 32)])==1:
      #print(f"Old len is {len(df_grouped)}, new len is {len(df_grouped.apply(lambda row: row[df_grouped['dispcode'].isin([31, 32])]))}")

      entries = entries.append(df_grouped.apply(lambda row: row[df_grouped['dispcode'].isin([31, 32])]), ignore_index=True)
    elif len(df_grouped[(df_grouped.dispcode == 31) | (df_grouped.dispcode == 32)])>1:
      earliest = df_grouped[(df_grouped.dispcode == 31) | (df_grouped.dispcode == 32)]
      
      earliest = earliest.sort_values(date_column)
      
      entries = entries.append(earliest.head(1), ignore_index=True)
    else:
      df_grouped["lastpage_code"] = df_grouped['lastpage'].map(positions_dict)
      df_grouped_max = df_grouped[df_grouped.lastpage_code == df_grouped.lastpage_code.max()]
      if len(df_grouped_max)==1:
        entries = entries.append(df_grouped_max.drop('lastpage_code', axis=1), ignore_index=True)
      else:
        df_grouped_max = df_grouped_max.sort_values(date_column)
        entries = entries.append(df_grouped_max.drop('lastpage_code', axis=1).head(1), ignore_index=True)
  print(f"Out of {len(df)} entries in the survey:\n\t{len(survey_all)} Single Entry Participants\n\t{len(survey_dupes)} Duplicated entries amongst {len(set(survey_dupes.p_0002.to_list()))} Participants")
  ## Adding the completed entries back into main dataframe
  survey_all_add = survey_all.append(entries).reset_index()
  return survey_all_add

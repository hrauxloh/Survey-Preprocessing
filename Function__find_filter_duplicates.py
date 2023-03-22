{
  "nbformat": 4,
  "nbformat_minor": 0,
  "metadata": {
    "colab": {
      "provenance": [],
      "authorship_tag": "ABX9TyMkdfANzVs8iL1mpD5UD1+E"
    },
    "kernelspec": {
      "name": "python3",
      "display_name": "Python 3"
    },
    "language_info": {
      "name": "python"
    }
  },
  "cells": [
    {
      "cell_type": "code",
      "execution_count": 1,
      "metadata": {
        "id": "5nmwsOVv1N7K"
      },
      "outputs": [],
      "source": [
        "def find_filter_duplicates(df, id_column, date_column, filepath_question_metadata):\n",
        "  df[date_column] = pd.to_datetime(df[date_column], format='%Y-%m-%d %H:%M:%S')\n",
        "  if df[date_column].dtype != 'datetime64[ns, UTC]':\n",
        "    return print(\"The function requires a date/time column with data in the format 'datetime64'.\")\n",
        "  \n",
        "  survey_all = df.groupby(id_column).filter(lambda x: len(x)==1)\n",
        "  survey_dupes = df.groupby(id_column).filter(lambda x: len(x)>1)\n",
        "  \n",
        "  ## Getting question MetaData for determining order of occurance in the survey\n",
        "  # Survey Meta Data File (page, question_id, question_label, question_text, question_type, wave_1, wave_2, wave_3)\n",
        "  question_metadata= pd.read_csv(filepath_question_metadata, sep=',', error_bad_lines=False).reset_index().rename(columns={'index':'Position'})\n",
        "\n",
        "  ## Positionality of Questions Dictionary\n",
        "  question_positions = question_metadata[['page']].drop_duplicates().reset_index(drop=True).reset_index().rename(columns={'index':'Position'})\n",
        "  positions_dict = dict(zip(question_positions.page, question_positions.Position))\n",
        "\n",
        "  # create object into which to put acceptable entries\n",
        "  entries = pd.DataFrame()\n",
        "\n",
        "  ## Group entries by Participant ID\n",
        "\n",
        "  grouped_dupes = survey_dupes.groupby(id_column)\n",
        "\n",
        "  for group_name, df_grouped in grouped_dupes:\n",
        "    #print('\\nCREATE TABLE {}('.format(group_name))\n",
        "    \n",
        "    if len(df_grouped[(df_grouped.dispcode == 31) | (df_grouped.dispcode == 32)])==1:\n",
        "      #print(f\"Old len is {len(df_grouped)}, new len is {len(df_grouped.apply(lambda row: row[df_grouped['dispcode'].isin([31, 32])]))}\")\n",
        "\n",
        "      entries = entries.append(df_grouped.apply(lambda row: row[df_grouped['dispcode'].isin([31, 32])]), ignore_index=True)\n",
        "    elif len(df_grouped[(df_grouped.dispcode == 31) | (df_grouped.dispcode == 32)])>1:\n",
        "      earliest = df_grouped[(df_grouped.dispcode == 31) | (df_grouped.dispcode == 32)]\n",
        "      \n",
        "      earliest = earliest.sort_values(date_column)\n",
        "      \n",
        "      entries = entries.append(earliest.head(1), ignore_index=True)\n",
        "    else:\n",
        "      df_grouped[\"lastpage_code\"] = df_grouped['lastpage'].map(positions_dict)\n",
        "      df_grouped_max = df_grouped[df_grouped.lastpage_code == df_grouped.lastpage_code.max()]\n",
        "      if len(df_grouped_max)==1:\n",
        "        entries = entries.append(df_grouped_max.drop('lastpage_code', axis=1), ignore_index=True)\n",
        "      else:\n",
        "        df_grouped_max = df_grouped_max.sort_values(date_column)\n",
        "        entries = entries.append(df_grouped_max.drop('lastpage_code', axis=1).head(1), ignore_index=True)\n",
        "  print(f\"Out of {len(df)} entries in the survey:\\n\\t{len(survey_all)} Single Entry Participants\\n\\t{len(survey_dupes)} Duplicated entries amongst {len(set(survey_dupes.p_0002.to_list()))} Participants\")\n",
        "  ## Adding the completed entries back into main dataframe\n",
        "  survey_all_add = survey_all.append(entries).reset_index()\n",
        "  return survey_all_add"
      ]
    },
    {
      "cell_type": "code",
      "source": [],
      "metadata": {
        "id": "cCCkND_uVomA"
      },
      "execution_count": null,
      "outputs": []
    }
  ]
}
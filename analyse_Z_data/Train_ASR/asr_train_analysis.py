import os

def get_path(path):
    return os.path.split(path)[0]


def get_filename(path):
    return os.path.splitext(os.path.basename(path))[0]



def extract_turns(input):
    """
    Génère automatiquement un fichier contenant le nombre de tours par locuteur à partir du fichier de tours

    :param input:
    :return:
    """

    f = open(input,mode="r",encoding="utf-8")
    content = f.read()
    output_basename = get_filename(input)
    outputFile1 = open(output_basename+"_turns.csv",mode="w",encoding="utf-8")
    outputFile1.write("id_turn;start_time;end_time;speaker;episode;trans\n")
    lines = content.split("\n")

    turns_by_spk={}
    id_turn=0
    for line in lines:
        if len(line)>1:
            col = line.split("\t")
            episode, speaker, start_time, end_time = col[0].split("#")
            outputFile1.write(str(id_turn)+";"+start_time+";"+end_time+";"+speaker+";"+episode+";"+col[1]+"\n")
            id_turn+=1
            if speaker not in turns_by_spk.keys():
                turns_by_spk[speaker] = {}
                turns_by_spk[speaker]["count"]=1
                turns_by_spk[speaker]["length"]=float(end_time)-float(start_time)

            else:
                turns_by_spk[speaker]["count"] += 1
                turns_by_spk[speaker]["length"]+=float(end_time)-float(start_time)

    outputFile2 = open(output_basename+"_turns_by_spk.csv",mode="w",encoding="utf-8")
    outputFile2.write("speaker;nb_turn;total_length\n")

    for entry in turns_by_spk.keys():
        outputFile2.write(entry+";"+str(turns_by_spk[entry]["count"])+";"+str(turns_by_spk[entry]["length"])+"\n")

    f.close()
    outputFile1.close()
    outputFile2.close()


def gender_mapping(filename,gender_file):
    """
    Associe un genre à chaque locuteur dans le fichier de tours de parole ou dans le fichier de nombre de tours par locuteur

    :param filename: nom du fichiers de tours
    :param gender_file: nom du fichier de mapping locuteur-genre
    :return:
    """
    f = open(gender_file, mode="r", encoding="utf-8")
    genders = {}
    lines = f.read().split("\n")
    for line in lines:
        if len(line)>1:
            speaker,gender=line.split(" ")
            genders[speaker] = gender
    f.close()

    f = open(filename,mode="r",encoding="utf-8")
    outf_basename=get_filename(filename)
    outf = open(outf_basename+"_gendered.csv",mode="w",encoding="utf-8")
    # si on a un fichier de tours de parole par locuteur
    if "_turns_by_spk" in outf_basename:
        outf.write("name;gender;nb_turn;total_length\n")
        lines = f.read().split("\n")
        for line in lines[1:]:
            if len(line)>1:
                speaker, turn_count, total_length = line.split(";")
                if speaker in genders.keys():
                    gender=genders[speaker]
                else:
                    gender="NA"
                outf.write(speaker+";"+gender+";"+turn_count+";"+total_length+"\n")
    # si on a le fichier de tours
    else:
        outf.write("id_turn;start_time;end_time;speaker;gender;episode;trans\n")
        lines = f.read().split("\n")
        for line in lines[1:]:
            if len(line) > 1:
                id_turn,start_time,end_time,speaker,episode,trans = line.split(";")
                if speaker in genders.keys():
                    gender = genders[speaker]
                else:
                    gender = "NA"
                outf.write(id_turn+";"+start_time+";"+end_time+";"+speaker+";"+gender+";"+episode+";"+trans+"\n")
    f.close()
    outf.close()


def add_id_class(filename,classdict):

    f = open(filename, mode="r", encoding="utf-8")
    outf_basename = get_filename(filename)
    outf = open(outf_basename + "_with_class.csv", mode="w", encoding="utf-8")
    # si on a un fichier de tours de parole par locuteur
    if "_turns_by_spk" in outf_basename:
        outf.write("name;gender;spk_class;nb_turn;total_length\n")
        lines = f.read().split("\n")
        for line in lines[1:-1]:
            speaker, gender, turn_count, total_length = line.split(";")
            outf.write(speaker + ";" + gender + ";"+ classdict[(speaker,gender)] + ";" + turn_count + ";" + total_length +"\n")
    # si on a le fichier de tours
    else:
        outf.write("id_turn;start_time;end_time;speaker;gender;spk_class;episode;trans\n")
        lines = f.read().split("\n")
        for line in lines[1:-1]:
            id_turn, start_time, end_time, speaker, gender, episode, trans = line.split(";")
            outf.write(
                id_turn + ";" + start_time + ";" + end_time + ";" + speaker + ";" + gender + ";"+ classdict[(speaker,gender)] +";" + episode + ";" + trans + "\n")
    f.close()
    outf.close()


def main() :


    extract_turns("text.csv")
    gender_mapping("text_turns_by_spk.csv","spk-gender.txt")
    gender_mapping("text_turns.csv","spk-gender.txt")

    spk_class={}
    f=open("text_turns_by_spk_gendered.csv",mode="r",encoding="utf-8")
    content=f.read()
    lines=content.split("\n")
    for line in lines[1:-1]:
        speaker, gender, nb_turn, total_length = line.split(";")
        if int(nb_turn) < 75:
            if float(total_length) < 600:
                id_class = "1"
            else:
                id_class = "3"
        else:
            if float(total_length) < 600:
                id_class = "2"
            else:
                id_class = "4"
        spk_class[(speaker,gender)]=id_class

    add_id_class("text_turns_by_spk_gendered.csv",spk_class)
    add_id_class("text_turns_gendered.csv",spk_class)

if __name__ == '__main__':
    main()
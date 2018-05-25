speakers={}
f=open("speakers_class.csv",mode="r",encoding="utf-8")
content=f.read()
lines=content.split("\n")

for line in lines[1:-1]:
    spk,id_class=line.split(";")
    speakers[spk]=id_class
f.close()


f=open("turns_all_info.csv",mode="r",encoding="utf-8")
fo=open("turns_all_info_id_class.csv",mode="w",encoding="utf-8")
fo.write("id_turn;start_time;end_time;id_speaker;gender;id_episode;id_show;id_corpus;id_speaker_class\n")
content=f.read()
lines=content.split("\n")

for line in lines[1:-1]:
    id_turn, start_time, end_time, id_speaker, gender, id_episode, id_show, id_corpus = line.split(";")
    fo.write(line+";"+speakers[id_speaker]+"\n")

f.close()
fo.close()
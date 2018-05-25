def main :

    speakers={}

    f=open("turns_all_info.csv",mode="r",encoding="utf-8")
    content=f.read()
    lines=content.split("\n")

    for line in lines[1:-1]:
        id_turn,start_time,end_time,id_speaker,gender,id_episode,id_show,id_corpus=line.split(";")
        key=(id_speaker,gender)
        if key in speakers.keys():
           speakers[key]["count"]+=1
           length=float(end_time)-float(start_time)
           speakers[key]["length"]+=length
        else:
            speakers[key]={}
            speakers[key]["count"] = 1
            length = float(end_time) - float(start_time)
            speakers[key]["length"] = length

    f.close()


    fo=open("turn_count_length.csv",mode="w",encoding="utf-8")
    fo.write("id_speaker;gender;turn_count;total_length;spk_class\n")
    for (id,gender) in speakers.keys():
        count=speakers[(id,gender)]["count"]
        length=speakers[(id,gender)]["length"]
        if count < 75:
            if length < 600:
                spk_class="1"
            else:
                spk_class="3"
        else:
            if length<600:
                spk_class="2"
            else:
                spk_class="4"

        fo.write(id+";"+gender+";"+str(count)+";"+str(length)+";"+spk_class+"\n")


import xml.etree.ElementTree as ET
import sqlite3
import os
import re
import io


def create_connection(db_file):
    """
        Create a database connection to the SQLite database specified by db_file
        :param db_file: database file
        :return: Connection object or None
    """

    try:
        conn = sqlite3.connect(db_file)
        return conn
    except Exception as e:
        print(e)

    return None


def create_speaker(conn, speaker):
    """
        Create a new speaker into the speakers table
        :param conn:
        :param speaker:
        :return: speaker id
    """

    sql = ''' INSERT INTO speaker(name,gender,native)
              VALUES(?,?,?) '''
    cur = conn.cursor()
    cur.execute(sql, speaker)
    return cur.lastrowid


def create_turn(conn, turn):
    """
        Create a new speaker into the speakers table
        :param conn:
        :param speaker:
        :return: speaker id
    """
    sql = ''' INSERT INTO turn(start_time,end_time,id_speaker,id_episode,trans)
                  VALUES(?,?,?,?,?) '''
    cur = conn.cursor()
    cur.execute(sql, turn)
    return cur.lastrowid


def create_episode(conn, episode):
    """
        Create a new episode into the episodes table
        :param conn:
        :param episode:
        :return: episode id
    """
    sql = '''INSERT INTO episode(date, id_show, id_corpus, partition, path)
                  VALUES(?,?,?,?,?)'''
    cur = conn.cursor()
    cur.execute(sql, episode)
    return cur.lastrowid


def extract_data(filename, id_ep, conn):
    """
        Extract data from a trs file and update the data base with speaker and turns accordingly
        :param filename:
        :param id_ep:
        :param conn:
        :return:
    """

    try:
        tree = ET.parse(filename)
        root = tree.getroot()

        # creation of a speaker's dict to manage the local ids (specific speakers' id file-dependent)
        speakers = {}
        # creation of a iterable to identify the unknown/unnamed speakers
        uknw_id = 0

        names = []
        cur = conn.cursor()
        cur.execute("SELECT name FROM speaker")
        rows = cur.fetchall()
        i = 0
        for row in rows:
            names.append(row[0])
            i += 1

        # creation of speakers
        for spkr in root.iter("Speaker"):
            name = spkr.attrib["name"]
            if (("," in name) or ("sup+" in name) or ("Sup+" in name)):
                name = "multi_spk"
            elif (("spk" in name) or ("speaker" in name) or ("Inconnu" in name) or ("unknown" in name)):
                name = "spk_"+str(id_ep)+"_"+str(uknw_id)
                uknw_id += 1
            else :
                n = name.split(" ")
                name = n[0]
                if len(n) > 1:
                    for i in range(1, len(n)):
                        name += "_" + n[i].upper()

            if "type" in spkr.attrib:
                if spkr.attrib["type"] not in ("male", "female"):
                    gender = "NA"
                else:
                    gender = spkr.attrib["type"]
            else:
                gender = "NA"

            if "dialect" in spkr.attrib:
                native = spkr.attrib["dialect"]
            else:
                native = "NA"

            # speaker added only if doesn't already exists in the database
            if name not in names:
                new_speaker = (name, gender, native)
                create_speaker(conn, new_speaker)

            # update of the local id->name dictionary
            speakers[spkr.attrib['id']] = name

        # creation of turns
        for turn in root.iter("Turn"):

            if "speaker" in turn.attrib:
                if len(turn.attrib["speaker"]) > 5:
                    speaker_name = "multi_spk"
                else:
                    speaker_name = speakers[turn.attrib["speaker"]]
                start_time = turn.attrib["startTime"]
                end_time = turn.attrib["endTime"]

                cur = conn.cursor()
                cur.execute("SELECT id_speaker FROM speaker WHERE name=?", (speaker_name,))
                id_speaker = cur.fetchone()[0]
                id_episode = id_ep
                trans = ET.tostring(turn, "ISO-8859-1", method="text")
                trans = trans.decode("ISO-8859-1")
                trans = re.sub("\n", " ", trans)
                trans = re.sub("  ", " ", trans)

                new_turn = (start_time, end_time, id_speaker, id_episode, trans)
                create_turn(conn, new_turn)

    except Exception as e:
        print(e)
        print(filename)
        pass


def extract_date(filename, corpus):
    """
        Extract the date of a given episode from the filename. (The use of the corpus helps define
        the structure of the regex)
        :param filename:
        :param corpus:
        :return: date (string)
    """
    try:
        if corpus in ("ester1", "ester2"):
            date_match = re.match(r'^(\d\d\d\d)', filename)
        else:
            if "EST2BC-FRE-FR-FINTER-DEBATE" in filename:
                date_match = re.match(r'.*\_(\d\d\d\d)\d\d\d\d\_', filename)
            elif "EST2BC_FRE_FR" in filename:
                date_match = re.match(r'.*\_(\d\d\d\d)\d\d\d\d\_', filename)
            else:
                date_match = re.match(r'.*\_(\d\d\d\d)\-', filename)
        date = str(date_match.group(1))
        return date

    except Exception as e:
        print("Exception du try extract_date")
        print(e)
        date = "NA"
        return date


def extract_show(filename):
    """
        Return the show name of a given episode
        :param filename:
        :return: show name (string)

    """
    try:
        f = open("recap_data.csv", mode='r', encoding="utf-8")
        content = f.read()
        f.close()
        lines = content.split('\n')
        for line in lines:
            cols = line.split(';')
            if cols[0] == filename:
                return cols[3]
        return None

    except Exception as e:
        print("Exception du try extract_show")
        print(e)
        return None


def _get_filename(path):
    while '.' in path:
        path=os.path.splitext(os.path.basename(path))[0]
    return path

def getCorpusID(corpus, conn):
    """
    Return the corpus ID given its name. (Database query)
        :param corpus:
        :param conn:
        :return: id_corpus (integer)
    """
    cur = conn.cursor()
    cur.execute("SELECT id_corpus FROM corpus WHERE name=?", (corpus,))
    id_corpus = cur.fetchone()[0]
    return id_corpus


def getShowId(show, conn):
    """
        Return the show id given its name. (Database query)
        :param show:
        :param conn:
        :return: id_show (integer)
    """
    cur = conn.cursor()
    cur.execute("SELECT id_show FROM show WHERE name=?", (show,))
    id_show = cur.fetchone()[0]
    return id_show


def getEpisodeId(path, conn):
    """
        Return the show id given its name. (Database query)
        :param path:
        :param conn:
        :return: id_episode (integer)
    """
    cur = conn.cursor()
    cur.execute("SELECT id_episode FROM episode WHERE path=?", (path,))
    id_episode = cur.fetchone()[0]
    return id_episode


def main():
    # print("GO")
    database = "db/corpus_database.db" # path to the database

    # create a database connection
    conn = create_connection(database)
    with conn:
        dir = '/home/getalp/elloumi/These2016/lnedata/corpus-trans'
        # dir contains the name of the path in which the corpora folders are
        child_dirs = os.listdir(dir)
        for child_dir in child_dirs:
            if child_dir not in ("trs", "wav", "repere-ini"):
                corpus = child_dir

                curr_dir = dir+"/"+child_dir
                partitions = os.listdir(curr_dir)

                # walk through the different partitions of the corpus
                for partition in partitions:
                    if partition in ("dev", "dev0", "dev2", "test", "test0", "test1", "test2", "train"):
                        path = curr_dir+"/"+partition+"/trs/"
                        # walk through all the trs file to extract data about speakers and turns
                        for filename in os.listdir(path):
                            # as each file represent an episode of a show, we insert it into the db
                            if re.match(r'^.*\.trs$', filename) is not None:
                                #print(corpus + " " + partition + " " + filename)
                                date = extract_date(filename, corpus)
                                episode_path = path + filename
                                show = extract_show(filename)
                                new_episode = (date, getShowId(show, conn), getCorpusID(corpus, conn), partition, episode_path)
                                create_episode(conn, new_episode)
                                id_episode = getEpisodeId(episode_path, conn)
                                extract_data(path + "/" + filename, id_episode, conn)

        # Exception management : files with a not well-formed xml for the transcript (presence of &, removed here)
        excp_dir = '/home/getalp/garnerim/trs_parsing/not_well_formed_trs/repere/'
        corpus = "repere"
        partitions = os.listdir(excp_dir)

        # walk through the different partitions of the corpus
        for partition in partitions:
            if partition in ("dev", "dev0", "dev2", "test", "test0", "test1", "test2", "train"):
                real_path = excp_dir + partition + "/trs"
                # walk through all the trs file to extract data about speakers and turns
                for filename in os.listdir(real_path):
                    # as each file represent an episode of a show, we insert it into the db
                    if re.match(r'^.*\.trs$', filename) is not None:
                        print(corpus + " " + partition + " " + filename)
                        date = extract_date(filename, corpus)
                        episode_path = "/home/getalp/elloumi/These2016/lnedata/corpus-trans/repere/" + partition +"/trs/" + filename
                        show = extract_show(filename)
                        new_episode = (
                        date, getShowId(show, conn), getCorpusID(corpus, conn), partition, episode_path)
                        create_episode(conn, new_episode)
                        id_episode = getEpisodeId(episode_path, conn)
                        extract_data(real_path + "/" + filename, id_episode, conn)


if __name__ == '__main__':
    main()

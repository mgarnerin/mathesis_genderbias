import re

f = open("text",mode="r",encoding="utf-8")
o = open("text.csv",mode="w",encoding="utf-8")

content=f.read()
new_content=re.sub(r'(\d+#\d+\.\d+)( )',"\g<1>\t",content)

o.write(new_content)

f.close()
o.close()

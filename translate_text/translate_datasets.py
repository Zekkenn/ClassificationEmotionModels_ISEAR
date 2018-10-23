import snippets
import xml.etree.ElementTree as ET

def translate_sit_xml(xmlpath):
    tree = ET.parse('items.xml')  
    root = tree.getroot()

    # Change sentences
    for elem in root.iter('instance'):
        elem.text = translate_text(elem.text)


if __name__ == "__main__":
    bot_ui()

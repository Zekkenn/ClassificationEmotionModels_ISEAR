"""
This Application translate ISEAR & SemEval 2007: 14 Task
    From they original language (English) to Spanish with
Google Cloud Translate API

For more information, the documentation at
https://cloud.google.com/translate/docs.
"""

from google.cloud import translate
from tempfile import NamedTemporaryFile

import xml.etree.ElementTree as ET
import shutil
import csv
import six

SEMEVAL_PATH_IN = r'C:\Users\Esteban\Documents\Google Cloud Projects\python-docs-samples\translate\cloud-client\SemEval_14\AffectiveText.test\affectivetext_test.xml'
SEMEVAL_PATH_OUT = r'C:\Users\Esteban\Documents\Google Cloud Projects\python-docs-samples\translate\cloud-client\SemEval_14\AffectiveText.test.spanish\affectivetext_test.xml'

ISEAR_PATH_IN = r'C:\Users\Esteban\Documents\Google Cloud Projects\python-docs-samples\translate\cloud-client\ISEAR\isear.csv'
ISEAR_PATH_OUT = r'C:\Users\Esteban\Documents\Google Cloud Projects\python-docs-samples\translate\cloud-client\ISEAR\isear_spanish.csv'

def translate_semEval(semEval_xml):
    with open(semEval_xml, 'r') as xml_file:
        tree = ET.parse(xml_file)
        root = tree.getroot()
        # Change sentences
        count = 1
        for elem in root.iter('instance'):
            elem.text = translate_text('es',elem.text)
            if count == 3:
                break
            count += 1
        tree.write(SEMEVAL_PATH_OUT)

def translate_isear(isear_csv):

    tempfile = NamedTemporaryFile(mode='w', encoding='utf-8', delete=False)
    
    with open(isear_csv, 'r') as isear, tempfile:
        isear_reader = csv.DictReader(isear, delimiter='|')
        isear_writer = csv.DictWriter(tempfile, fieldnames=isear_reader.fieldnames, delimiter='|', lineterminator='\n')
        isear_writer.writeheader()

        for row in isear_reader:
            row['SIT'] = translate_text('es',row['SIT'])            
            new_row = {key: value for key, value in row.items()
                       if key in isear_writer.fieldnames}
            isear_writer.writerow(new_row)
            
    shutil.move(tempfile.name, ISEAR_PATH_OUT)

def translate_text(target, text):
    """Translates text into the target language.

    Target must be an ISO 639-1 language code.
    See https://g.co/cloud/translate/v2/translate-reference#supported_languages
    """
    translate_client = translate.Client()

    if isinstance(text, six.binary_type):
        text = text.decode('utf-8')

    # Text can also be a sequence of strings, in which case this method
    # will return a sequence of results for each text.
    result = translate_client.translate(
        text, target_language=target)

    return(result['translatedText'])


if __name__ == "__main__":
    translate_semEval(SEMEVAL_PATH_IN)
    # translate_isear(ISEAR_PATH_IN)

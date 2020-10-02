import csv
import io
import logging

import re 
import chardet

from typing import List, Dict, Any, Union, Tuple, Optional

import settings


logger = logging.getLogger(__name__)


def _clean(value: Any) -> Any:
    if value == '' or value == ' ':
        return None

    try:
       value =  _parse_to_number(value)

    except:
        value = re.sub('\s{2,}', ' ', str(value).strip())

    return value


def _parse_to_number(value: str) -> Union[int, float]:
    try:
        value = int(value)

    except ValueError:
        value = float(value)
    
    return value


def _get_delimiter(cell_content: str) -> str:
    fake_body = io.StringIO(cell_content)

    sniffer = csv.Sniffer()
    csv_dialect = sniffer.sniff(fake_body.readline())

    return csv_dialect.delimiter


def _set_encoding(row_value: str) -> str:
    def set_latin(row_value):
        try:
            row = row_value.encode('latin-1').decode('utf-8')

        except UnicodeEncodeError:
            row = row_value.encode('cp1252').decode('utf-8')

        return row

    try:
        row = set_latin(row_value=row_value)

    except:
        row = row_value.encode('utf-8').decode('utf-8')

    return row


def _set_encoding_dict(records: Dict[str, str])-> Dict[str, Any]:
    if isinstance(records, dict):
        for key, value in records.items():
            if isinstance(value, str):
                records[key] = _set_encoding(records[key])

    return records


def _strip_out_headers(body: str,
                       newline_delimiter: str,
                       delimiter: str,
                       record_schema: Optional[Dict[str, Any]]=None) -> Tuple[List[str], str]:
    header_row = body.split(newline_delimiter)[0]

    read_from = io.StringIO(header_row, newline_delimiter)

    reader = csv.DictReader(
        read_from,
        delimiter=delimiter,
        fieldnames=None,
        dialect=csv.excel,
    )

    fieldnames = [
        _set_encoding(colname.strip())
        for colname in reader.fieldnames 
    ]

    return fieldnames, newline_delimiter.join(
        body.split(newline_delimiter)[1:]
    )


def _sniff_newline(body: str) -> str:
    if '\r\n' in body:
        return '\r\n'
    elif '\r' in body:
        return '\r'
    else:
        return '\n'


def _predict_encoding(body: bytes) -> Union[str, bool]:
    results: Dict[str, Any] = chardet.detect(body)
    if results.get('confidence') < 0.5:

        return False

    return results.get('encoding')


def parse_csv(body: bytes) -> List[Dict[str, Any]]:

    if isinstance(body, bytes):
        encoding = _predict_encoding(body=body[1:2000])
        body = body.decode(encoding)

    sample_data = body[1:250]
    delimiter = _get_delimiter(
        sample_data
    )
    
    newline_delimiter = _sniff_newline(body=body[1:2000])

    headers, raw_body = _strip_out_headers(
        body=body,
        newline_delimiter=newline_delimiter,
        delimiter=delimiter
    )

    read_body = io.StringIO(raw_body, newline_delimiter)
    reader = csv.reader(
        read_body,
        delimiter=delimiter,
        dialect=csv.excel,
    )

    formatted_rows = [
        dict(
            zip(headers, _set_encoding_dict(unformatted))
        ) for unformatted in reader   
    ]

    formatted_rows = [
        {
            key: _clean(value) for key, value in formatted.items()
        }
        for formatted in formatted_rows
    ]
    logger.info('Validation and parsing successful')

    return formatted_rows
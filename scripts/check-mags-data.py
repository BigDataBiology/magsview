#!/usr/bin/env python3
"""Check that src/Data/Blobs/MAGs.elm and static/genome-data/*.json are in sync.

Checks:
  - the embedded CSV has the columns that src/LoadData.elm decodes, and every
    value parses with the type the decoder expects
  - MAG IDs are unique
  - every MAG has a JSON file, and every JSON file belongs to a MAG
  - every JSON file has the structure that src/Pages/Genome/Genome_.elm decodes
  - the number of 16S sequences in the JSON matches `#16s_rrna`

Exits with status 1 if any check fails.
"""
import argparse
import csv
import io
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent

# Same order as src/LoadData.elm; `str` columns are not checked for type
COLUMNS = {
    'id': str,
    'samples_id': str,
    'taxonomy': str,
    'completeness': float,
    'contamination': float,
    'genome_size': int,
    '#16s_rrna': int,
    '#5s_rrna': int,
    '#23s_rrna': int,
    '#trna': int,
    'nr_contigs': int,
    'nr_genes': int,
    'is_representative': 'bool',
    'binning_tool': str,
    'assembly_method': str,
    'comment': str,
    'file_size': int,
    'file_sha256': str,
}

JSON_FIELDS = {
    '16S': {'Seq': str, 'OTU': str},
    'ARGs': {
        'Sequence': str,
        'ArgName': str,
        'Cut_Off': str,
        'Identity': (int, float),
        'Coverage': (int, float),
        'Drug Class': str,
        'ARO': str,
        'InResfinder': bool,
    },
}

BOOL_VALUES = {'true', 'false', 'yes', 'no', 'y', 'n', 't', 'f', '1', '0'}


def read_blob(path):
    m = re.search(r'magsBlob\s*=\s*String\.trim\s*"""(.*?)"""', path.read_text(), re.DOTALL)
    if m is None:
        raise ValueError(f'Could not find magsBlob in {path}')
    return list(csv.reader(io.StringIO(m.group(1).strip())))


def check_value(value, ty):
    if ty is int:
        int(value)
    elif ty is float:
        float(value)
    elif ty == 'bool':
        if value.lower() not in BOOL_VALUES:
            raise ValueError(f'not a boolean: {value!r}')


def check_blob(rows, errors):
    header, body = rows[0], rows[1:]
    if header != list(COLUMNS):
        errors.append(f'MAGs.elm: header {header} does not match expected {list(COLUMNS)}')
        return {}
    mags = {}
    for lineno, row in enumerate(body, start=2):
        if len(row) != len(header):
            errors.append(f'MAGs.elm line {lineno}: expected {len(header)} fields, got {len(row)}')
            continue
        rec = dict(zip(header, row))
        for col, ty in COLUMNS.items():
            try:
                check_value(rec[col], ty)
            except ValueError:
                errors.append(f'MAGs.elm line {lineno} ({rec["id"]}): bad value for {col}: {rec[col]!r}')
        if rec['id'] in mags:
            errors.append(f'MAGs.elm line {lineno}: duplicate ID {rec["id"]}')
        mags[rec['id']] = rec
    return mags


def check_json(path, rec, errors):
    try:
        data = json.loads(path.read_text())
    except json.JSONDecodeError as e:
        errors.append(f'{path.name}: invalid JSON ({e})')
        return
    if not isinstance(data, dict) or set(data) != set(JSON_FIELDS):
        keys = sorted(data) if isinstance(data, dict) else type(data).__name__
        errors.append(f'{path.name}: expected keys {sorted(JSON_FIELDS)}, got {keys}')
        return
    for key, fields in JSON_FIELDS.items():
        if not isinstance(data[key], list):
            errors.append(f'{path.name}: {key} is not a list')
            continue
        for i, entry in enumerate(data[key]):
            for field, ty in fields.items():
                if not isinstance(entry, dict) or field not in entry:
                    errors.append(f'{path.name}: {key}[{i}] is missing {field!r}')
                elif not isinstance(entry[field], ty) or (ty is not bool and isinstance(entry[field], bool)):
                    errors.append(f'{path.name}: {key}[{i}].{field} has wrong type ({type(entry[field]).__name__})')
    if isinstance(data['16S'], list) and len(data['16S']) != int(rec['#16s_rrna']):
        errors.append(f'{path.name}: {len(data["16S"])} 16S sequences, but #16s_rrna is {rec["#16s_rrna"]}')


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--blob', type=Path, default=ROOT / 'src' / 'Data' / 'Blobs' / 'MAGs.elm')
    parser.add_argument('--json-dir', type=Path, default=ROOT / 'static' / 'genome-data')
    args = parser.parse_args()

    errors = []
    mags = check_blob(read_blob(args.blob), errors)
    json_files = {p.stem: p for p in args.json_dir.glob('*.json')}

    for mag_id in sorted(mags.keys() - json_files.keys()):
        errors.append(f'{mag_id}: in MAGs.elm but no {mag_id}.json in {args.json_dir}')
    for mag_id in sorted(json_files.keys() - mags.keys()):
        errors.append(f'{mag_id}.json: no matching MAG in MAGs.elm')
    for mag_id in sorted(mags.keys() & json_files.keys()):
        check_json(json_files[mag_id], mags[mag_id], errors)

    for e in errors:
        print(e, file=sys.stderr)
    if errors:
        print(f'FAILED: {len(errors)} problem(s)', file=sys.stderr)
        sys.exit(1)
    print(f'OK: {len(mags)} MAGs, {len(json_files)} JSON files')


if __name__ == '__main__':
    main()

#!/usr/bin/env python3
"""Generate src/Data/Blobs/MAGs.elm from the MAG catalogue table.

The input is the catalogue CSV (one row per MAG, columns such as `Bin ID`,
`Classification`, `16S rRNA`, `16S partial`, ...). The output is an Elm module
embedding the CSV that `LoadData.loadData` decodes.

Usage:
    scripts/generate-mags-blob.py             # (re)write the Elm module from data/magsdata.csv
    scripts/generate-mags-blob.py --check     # fail if it is out of date
    scripts/generate-mags-blob.py other.csv   # use a different catalogue
"""
import argparse
import csv
import io
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DEFAULT_SOURCE = ROOT / 'data' / 'magsdata.csv'
DEFAULT_OUTPUT = ROOT / 'src' / 'Data' / 'Blobs' / 'MAGs.elm'

# Must match the field names used by the decoder in src/LoadData.elm
COLUMNS = [
    'id',
    'samples_id',
    'taxonomy',
    'completeness',
    'contamination',
    'genome_size',
    '#16s_rrna',
    '#5s_rrna',
    '#23s_rrna',
    '#trna',
    'nr_contigs',
    'nr_genes',
    'is_representative',
    'binning_tool',
    'assembly_method',
    'comment',
    'file_size',
    'file_sha256',
]

ASSEMBLY_METHOD = 'Flye with Polypolish'
BINNING_TOOL = 'SemiBin2'


def count(value):
    '''Counts are stored as floats (e.g., "6.0"); empty means none were found'''
    return int(float(value)) if value else 0


def rrna_total(row, gene):
    '''Complete plus partial copies'''
    return count(row[f'{gene} rRNA']) + count(row[f'{gene} partial'])


def convert_row(row):
    mag_id = row['Bin ID'].removesuffix('.fna.gz')
    if row['Representative'] not in ('Yes', 'No'):
        raise ValueError(f'{mag_id}: unexpected Representative value {row["Representative"]!r}')
    return {
        'id': mag_id,
        'samples_id': row['Sample'],
        'taxonomy': row['Classification'],
        'completeness': repr(float(row['Completeness'])),
        'contamination': repr(float(row['Contamination'])),
        'genome_size': int(row['Genome Size']),
        '#16s_rrna': rrna_total(row, '16S'),
        '#5s_rrna': rrna_total(row, '5S'),
        '#23s_rrna': rrna_total(row, '23S'),
        '#trna': int(row['Unique tRNAs']),
        'nr_contigs': int(row['Nr contigs']),
        # Not yet available: placeholders
        'nr_genes': 0,
        'is_representative': str(row['Representative'] == 'Yes'),
        'binning_tool': BINNING_TOOL,
        'assembly_method': ASSEMBLY_METHOD,
        'comment': '',
        'file_size': 0,
        'file_sha256': 'TODO',
    }


def build_csv(source):
    with open(source, newline='') as f:
        rows = [convert_row(r) for r in csv.DictReader(f)]
    ids = [r['id'] for r in rows]
    if len(set(ids)) != len(ids):
        raise ValueError('Duplicate MAG IDs in input')
    out = io.StringIO()
    writer = csv.DictWriter(out, fieldnames=COLUMNS, lineterminator='\n')
    writer.writeheader()
    writer.writerows(rows)
    return out.getvalue(), len(rows)


def build_elm(csv_text):
    if '"""' in csv_text or '\\' in csv_text:
        raise ValueError('CSV contains characters that cannot be embedded in an Elm multiline string')
    return ('module Data.Blobs.MAGs exposing (magsBlob)\n'
            '\n'
            'magsBlob : String\n'
            'magsBlob = String.trim """\n'
            f'{csv_text}'
            '"""\n')


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('source', nargs='?', default=DEFAULT_SOURCE, type=Path,
                        help=f'MAG catalogue CSV (default: {DEFAULT_SOURCE})')
    parser.add_argument('-o', '--output', default=DEFAULT_OUTPUT, type=Path,
                        help=f'Elm module to write (default: {DEFAULT_OUTPUT})')
    parser.add_argument('--check', action='store_true',
                        help='Do not write; exit with an error if the output is not up to date')
    args = parser.parse_args()

    csv_text, n = build_csv(args.source)
    elm = build_elm(csv_text)
    if args.check:
        current = args.output.read_text() if args.output.exists() else None
        if current != elm:
            print(f'{args.output} is out of date with {args.source}; '
                  f'rerun without --check to regenerate', file=sys.stderr)
            sys.exit(1)
        print(f'{args.output} is up to date ({n} MAGs)')
    else:
        args.output.write_text(elm)
        print(f'Wrote {args.output} ({n} MAGs)')


if __name__ == '__main__':
    main()

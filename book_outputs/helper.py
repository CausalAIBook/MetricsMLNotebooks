"""Helper for make_book_outputs.sh: reads book_outputs/index.csv and turns notebooks into scripts.

Commands (chapters are numbers; none means all chapters in the index):
  notebooks [--missing] [CHAPTER ...]  notebooks that produce the book items (with --missing: only those
                                      with at least one output file missing)
  expected [CHAPTER ...]               output files the index promises, relative to book_outputs/
  script NOTEBOOK OUTFILE              write NOTEBOOK's code cells to OUTFILE without the install commands
  installs NOTEBOOK ...                print the install commands of the notebooks (R first, then pip),
                                      one per line, prefixed with "R " or "PIP "; R packages are only
                                      installed if missing
"""
import csv
import json
import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, 'book_outputs')
INDEX = os.path.join(OUT, 'index.csv')

R_INSTALL = re.compile(r'^\s*(install\.packages|remotes::install_\w+|devtools::install_\w+|BiocManager::install)\s*\(')
PY_INSTALL = re.compile(r'^\s*[!%]\s*pip3?\s+install\b')


def index_rows(chapters):
    with open(INDEX, newline='') as f:
        rows = list(csv.DictReader(f))
    if chapters:
        wanted = {int(c) for c in chapters}
        rows = [r for r in rows if int(r['chapter']) in wanted]
    return rows


def unique(seq):
    seen = []
    for s in seq:
        if s not in seen:
            seen.append(s)
    return seen


def code_cells(path):
    with open(path) as f:
        nb = json.load(f)
    return [''.join(c['source']) for c in nb['cells'] if c['cell_type'] == 'code']


def paren_balance(line):
    """Parentheses opened minus closed, ignoring strings and # comments (good enough for install calls)."""
    depth, quote = 0, None
    for ch in line:
        if quote:
            if ch == quote:
                quote = None
        elif ch in '"\'':
            quote = ch
        elif ch == '#':
            break
        elif ch == '(':
            depth += 1
        elif ch == ')':
            depth -= 1
    return depth


def strip_comment(line):
    """Drop a trailing # comment that is not inside a string."""
    quote = None
    for i, ch in enumerate(line):
        if quote:
            if ch == quote:
                quote = None
        elif ch in '"\'':
            quote = ch
        elif ch == '#':
            return line[:i].rstrip()
    return line.rstrip()


def r_setup_command(cmd):
    """install.packages("pkg") -> install only if missing; anything else unchanged."""
    m = re.fullmatch(r'install\.packages\(\s*"([\w.]+)"\s*\)', cmd)
    return f'if (!requireNamespace("{m.group(1)}", quietly = TRUE)) {cmd}' if m else cmd


def split_installs(code, is_r):
    """Return (code without install commands, list of install commands)."""
    keep, installs, current, depth = [], [], [], 0
    for line in code.split('\n'):
        if current:                      # continuation of a multi-line install.packages(...)
            current.append(line)
            depth += paren_balance(line)
            if depth <= 0:
                installs.append('\n'.join(current))
                current = []
        elif is_r and R_INSTALL.match(line):
            depth = paren_balance(line)
            if depth <= 0:
                installs.append(strip_comment(line).strip())
            else:
                current = [line]
        elif not is_r and PY_INSTALL.match(line):
            installs.append(strip_comment(line).strip())
        else:
            keep.append(line)
    return '\n'.join(keep), installs


def is_r_notebook(path):
    return path.endswith('.irnb') or 'ir' == json.load(open(path)).get('metadata', {}).get('kernelspec', {}).get('name')


def main(argv):
    cmd, args = argv[0], argv[1:]
    if cmd == 'notebooks':
        missing = '--missing' in args
        rows = index_rows([a for a in args if a != '--missing'])
        if missing:
            rows = [r for r in rows if not all(os.path.exists(os.path.join(OUT, f)) for f in r['file'].split(';'))]
        print('\n'.join(unique(r['notebook'] for r in rows)))
    elif cmd == 'expected':
        print('\n'.join(f for r in index_rows(args) for f in r['file'].split(';')))
    elif cmd == 'script':
        nb, outfile = args
        is_r = is_r_notebook(os.path.join(ROOT, nb))
        cells = [split_installs(c, is_r)[0] for c in code_cells(os.path.join(ROOT, nb))]
        with open(outfile, 'w') as f:
            f.write(f'# Generated from {nb} by book_outputs/helper.py (install commands removed)\n\n')
            f.write('\n\n'.join(cells) + '\n')
    elif cmd == 'installs':
        r_cmds, py_cmds = [], []
        for nb in args:
            is_r = is_r_notebook(os.path.join(ROOT, nb))
            for c in code_cells(os.path.join(ROOT, nb)):
                (r_cmds if is_r else py_cmds).extend(split_installs(c, is_r)[1])
        for c in unique(r_cmds):
            print('R ' + r_setup_command(c.replace('\n', ' ')))
        for c in unique(py_cmds):
            print('PIP ' + re.sub(r'^[!%]\s*pip3?\s+install\s+', '', c))
    else:
        sys.exit(__doc__)


if __name__ == '__main__':
    main(sys.argv[1:] or ['help'])

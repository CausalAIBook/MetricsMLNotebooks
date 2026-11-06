#!/usr/bin/env python3
"""Regenerate the numbers, tables and figures of the book from the notebooks.

Same as make_book_outputs.sh, but also runs on Windows (python make_book_outputs.py ...).
Only the notebooks listed in book_outputs/index.csv are run; outputs go to book_outputs/chNN/. See README.md.
"""
import argparse
import glob
import os
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import time
from concurrent.futures import ThreadPoolExecutor, as_completed

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), 'book_outputs'))
from helper import (OUT, ROOT, code_cells, index_rows, is_r_notebook, r_setup_command,  # noqa: E402
                    split_installs, unique)

LOGS = os.path.join(OUT, 'logs')
CRAN = 'https://cloud.r-project.org'

EPILOG = """\
environment:
  RSCRIPT      R interpreter for .irnb notebooks (default: Rscript on the PATH; on Windows also
               the newest R in "C:\\Program Files\\R")

Notebooks run as scripts (like the repo's CI), in a temporary directory, without their install commands.
Python notebooks run with IPython from the Python that runs this script.
Logs: book_outputs/logs/<notebook>.log
"""


def output_files(row):
    return row['file'].split(';')


def find_rscript():
    """RSCRIPT, else Rscript on the PATH, else (Windows) the newest R in the default install folder."""
    if os.environ.get('RSCRIPT'):
        return os.environ['RSCRIPT']
    found = shutil.which('Rscript')
    if found:
        return found
    if os.name == 'nt':
        base = os.environ.get('ProgramFiles', r'C:\Program Files')
        candidates = glob.glob(os.path.join(base, 'R', 'R-*', 'bin', 'Rscript.exe'))
        if candidates:
            return max(candidates, key=lambda p: [int(n) for n in re.findall(r'\d+', p.split(os.sep)[-3])])
    sys.exit('Rscript not found: install R (https://cran.r-project.org) or set RSCRIPT to the path of Rscript.')


def write_script(nb, path):
    """Write the notebook's code cells to path without the install commands; return True for R notebooks."""
    nb_path = os.path.join(ROOT, nb)
    is_r = is_r_notebook(nb_path)
    cells = [split_installs(c, is_r)[0] for c in code_cells(nb_path)]
    with open(path, 'w', encoding='utf-8', newline='\n') as f:
        f.write(f'# Generated from {nb} by make_book_outputs.py (install commands removed)\n\n')
        f.write('\n\n'.join(cells) + '\n')
    return is_r


def setup(notebooks, rscript):
    """Install the packages the notebooks install (R packages only if missing), then the pip packages."""
    r_cmds, pip_cmds = [], []
    for nb in notebooks:
        nb_path = os.path.join(ROOT, nb)
        is_r = is_r_notebook(nb_path)
        for c in code_cells(nb_path):
            (r_cmds if is_r else pip_cmds).extend(split_installs(c, is_r)[1])
    r_cmds = [r_setup_command(c.replace('\n', ' ')) for c in unique(r_cmds)]
    status = 0
    if r_cmds:
        print('Installing R packages:')
        print('\n'.join('  ' + c for c in r_cmds), flush=True)
        fd, r_setup = tempfile.mkstemp(suffix='.R', prefix='book_outputs_setup_')
        with os.fdopen(fd, 'w', encoding='utf-8', newline='\n') as f:
            f.write(f'options(repos = c(CRAN = "{CRAN}"))\n' + '\n'.join(r_cmds) + '\n')
        status = subprocess.run([rscript, r_setup]).returncode
        os.remove(r_setup)
    for c in unique(pip_cmds):
        pkgs = re.sub(r'^[!%]\s*pip3?\s+install\s+', '', c)
        print(f'pip install {pkgs}', flush=True)
        status = subprocess.run([sys.executable, '-m', 'pip', 'install'] + shlex.split(pkgs)).returncode or status
    return status


def run_notebook(nb, rscript):
    """Run one notebook as a script in a temporary directory; return (notebook, log name, exit status, seconds)."""
    name = os.path.splitext(os.path.basename(nb))[0]
    work = tempfile.mkdtemp(prefix='book_outputs_')
    start = time.time()
    try:
        script = os.path.join(work, name + '.script')
        is_r = write_script(nb, script)
        cmd = [rscript, script] if is_r else [sys.executable, '-m', 'IPython', script]
        env = dict(os.environ, BOOK_OUTPUT_DIR=OUT, MPLBACKEND='Agg')
        with open(os.path.join(LOGS, name + '.log'), 'wb') as log:
            status = subprocess.run(cmd, cwd=work, env=env, stdout=log, stderr=subprocess.STDOUT).returncode
    finally:
        shutil.rmtree(work, ignore_errors=True)
    return nb, name, status, time.time() - start


def main():
    parser = argparse.ArgumentParser(
        description='Regenerate the numbers, tables and figures of the book from the notebooks.',
        epilog=EPILOG, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('chapters', nargs='*', type=int, metavar='CHAPTER',
                        help='chapter numbers to build, e.g. "2 4" (default: all chapters in the index)')
    parser.add_argument('--list', action='store_true', help='only show which notebooks would run')
    parser.add_argument('--missing', action='store_true',
                        help='only run notebooks with at least one output file missing')
    parser.add_argument('--setup', action='store_true',
                        help='install the R and Python packages the selected notebooks need, then exit')
    parser.add_argument('-j', '--jobs', type=int, default=1, metavar='N',
                        help='run up to N notebooks in parallel (default 1)')
    args = parser.parse_args()

    rows = index_rows(args.chapters)
    if args.missing:
        rows = [r for r in rows if not all(os.path.exists(os.path.join(OUT, f)) for f in output_files(r))]
    notebooks = unique(r['notebook'] for r in rows)
    if not notebooks:
        print('Nothing to run.')
        return 0
    if args.list:
        print('\n'.join(notebooks))
        return 0

    rscript = find_rscript()
    if args.setup:
        return setup(notebooks, rscript)

    os.makedirs(LOGS, exist_ok=True)
    print(f'Running {len(notebooks)} notebook(s) with {args.jobs} parallel job(s); outputs in book_outputs/',
          flush=True)
    failed = False
    with ThreadPoolExecutor(max_workers=max(1, args.jobs)) as pool:
        for job in as_completed([pool.submit(run_notebook, nb, rscript) for nb in notebooks]):
            nb, name, status, secs = job.result()
            mins, secs = divmod(int(secs), 60)
            if status == 0:
                print(f'  ok      {nb:<55} {mins:3d}m{secs:02d}s', flush=True)
            else:
                print(f'  FAILED  {nb:<55} {mins:3d}m{secs:02d}s  (see book_outputs/logs/{name}.log)', flush=True)
                failed = True

    expected = [f for r in index_rows(args.chapters) for f in output_files(r)]
    absent = [f for f in expected if not os.path.exists(os.path.join(OUT, f))]
    if absent:
        print('Missing outputs:')
        print('\n'.join(f'  book_outputs/{f}' for f in absent))
        failed = True
    if not failed:
        print('All outputs written.')
    return 1 if failed else 0


if __name__ == '__main__':
    sys.exit(main())

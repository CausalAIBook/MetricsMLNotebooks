# Book outputs for Applied Causal ML

This branch regenerates the numbers, tables and figures of the book from the notebooks. Running `make_book_outputs.sh` executes only the notebooks that produce something used in the book and writes the results to one folder per chapter.

Currently covered: **Chapters 1–4**.

## Running

```bash
./make_book_outputs.sh --setup        # once: install the R (and Python) packages the notebooks need
./make_book_outputs.sh                # all chapters
./make_book_outputs.sh 2 4            # only Chapters 2 and 4
./make_book_outputs.sh -j 4           # run up to 4 notebooks in parallel
./make_book_outputs.sh --missing      # only notebooks whose outputs are missing
./make_book_outputs.sh --list 3       # show which notebooks Chapter 3 would run
```

Each notebook runs as a script in a temporary directory. Its install commands are skipped (that is what `--setup` is for), and its log goes to `book_outputs/logs/`.

## Layout

```
book_outputs/
  index.csv            book item -> output file -> notebook (also tells the script what to run)
  ch01/table_1_1.csv   one file per table
  ch02/numbers.csv     numbers quoted in the text of a chapter
  ch04/figure_4_1.png  figures as PNG (300 dpi) and PDF
```

## Not generated 

| Chapter | Items | Why |
|---|---|---|
| 0 | Figures 0.1–0.5 | introduction examples; no code in this repository |
| 1 | Figures 1.1, 1.2, 1.4 | portraits and illustrations |
| 1 | Figure 1.3 | illustration of polynomial approximation; no code in this repository |
| 2 | Figure 2.1, Table 2.1 | photo; FDA briefing data |
| 2 | Figures 2.2, 2.3 | causal diagrams |
| 3 | Figures 3.1, 3.2, 3.6 | illustrations |
| 3 | Figures 3.3–3.5 | coefficient plots for Example 3.1.1; no code in this repository |

## Adding a chapter

1. Add a save cell after each cell that produces a book item (same style as the existing ones).
2. Add one row per item to `book_outputs/index.csv`; separate several files of one item with `;`.
3. Run `./make_book_outputs.sh <chapter>` and compare the files with the book.

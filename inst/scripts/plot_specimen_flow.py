#!/usr/bin/env python3
"""
Render the AXIS specimen-flow Sankey from an exported flow CSV.

Input is the CSV produced by "Download flow data (CSV)" on the Inventory tab,
one row per isolate with five columns:

    flow_site, flow_study, flow_parent, flow_mdro, flow_species

Usage:
    python3 plot_specimen_flow.py FLOW.csv --out figure --title "REACT - Rush/RML"
    python3 plot_specimen_flow.py FLOW.csv --site "RML Specialty Hospital"

Writes <out>.png, <out>.svg and <out>.pdf at print resolution.

Node labels are single-line - "Enterococcus faecium (54)" rather than the name
with the count stacked beneath it - because two-line labels collide wherever
several thin flows land next to each other in the same column.
"""
import argparse, csv, collections, sys

try:
    import plotly.graph_objects as go
except ImportError:
    sys.exit("plotly is required:  pip install plotly kaleido")

# Validated categorical palette (data-viz reference instance, light mode).
PALETTE = ["#2a78d6", "#eb6834", "#1baf7a", "#eda100",
           "#e87ba4", "#008300", "#4a3aa7", "#e34948"]
INK, INK_MUTED, SURFACE = "#0b0b0b", "#52514e", "#fcfcfb"

COLUMNS = ["flow_site", "flow_study", "flow_parent", "flow_mdro", "flow_species"]


def load(path, site=None, study=None):
    with open(path, newline="", encoding="utf-8-sig") as fh:
        rows = list(csv.DictReader(fh))
    if not rows:
        sys.exit(f"{path} has no rows")
    missing = [c for c in COLUMNS if c not in rows[0]]
    if missing:
        sys.exit("CSV is missing columns: " + ", ".join(missing))
    if site:
        rows = [r for r in rows if r["flow_site"] == site]
    if study:
        rows = [r for r in rows if r["flow_study"] == study]
    if not rows:
        sys.exit("No rows left after filtering")
    return rows


def build(rows):
    """Nodes keep a column prefix so the same word in two columns stays two nodes."""
    order, index = [], {}
    def node(col, label):
        key = f"{col}␟{label}"
        if key not in index:
            index[key] = len(order)
            order.append((col, label or "(not recorded)"))
        return index[key]

    edges = collections.Counter()
    for r in rows:
        vals = [r[c].strip() or "(not recorded)" for c in COLUMNS]
        for i in range(len(COLUMNS) - 1):
            edges[(node(COLUMNS[i], vals[i]), node(COLUMNS[i + 1], vals[i + 1]))] += 1

    totals = collections.Counter()
    for (s, t), v in edges.items():
        totals[s] += v
    # A terminal node's total is what flows into it.
    for (s, t), v in edges.items():
        if t not in totals or all(a != t for a, _ in edges):
            totals[t] += v
    incoming = collections.Counter()
    for (s, t), v in edges.items():
        incoming[t] += v
    for n, (_, _) in enumerate(order):
        if n not in totals or totals[n] == 0:
            totals[n] = incoming[n]

    labels = [f"{lab} ({totals[i]})" for i, (_, lab) in enumerate(order)]
    col_of = [COLUMNS.index(c) for c, _ in order]
    colors = [PALETTE[i % len(PALETTE)] for i in range(len(order))]
    return order, labels, col_of, colors, edges


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("csv")
    ap.add_argument("--out", default="specimen_flow")
    ap.add_argument("--site")
    ap.add_argument("--study")
    ap.add_argument("--title", default="")
    ap.add_argument("--width", type=int, default=1500)
    ap.add_argument("--height", type=int, default=680)
    a = ap.parse_args()

    rows = load(a.csv, a.site, a.study)
    order, labels, col_of, colors, edges = build(rows)

    src = [s for (s, _) in edges]
    tgt = [t for (_, t) in edges]
    val = list(edges.values())
    # Ribbons take the source node's hue at low opacity.
    link_colors = [colors[s] + "8c" for s in src]

    fig = go.Figure(go.Sankey(
        arrangement="snap",
        node=dict(
            label=labels,
            pad=26,                 # vertical breathing room between nodes
            thickness=13,
            line=dict(width=0),
            color=colors,
            hovertemplate="%{label}<extra></extra>",
        ),
        link=dict(source=src, target=tgt, value=val, color=link_colors,
                  hovertemplate="%{source.label} → %{target.label}<br>"
                                "%{value} isolates<extra></extra>"),
        textfont=dict(color=INK, size=13, family="Arial"),
    ))
    fig.update_layout(
        title=dict(text=a.title, font=dict(size=15, color=INK), x=0.01, y=0.97)
             if a.title else None,
        font=dict(family="Arial", color=INK_MUTED),
        paper_bgcolor=SURFACE, plot_bgcolor=SURFACE,
        # Plotly draws the final column's labels to the LEFT of its nodes, so a
        # wide right margin only squeezes the columns together and makes the
        # interior labels collide with the column to their right.
        margin=dict(l=14, r=24, t=46 if a.title else 14, b=14),
        width=a.width, height=a.height,
    )

    for ext in ("png", "svg", "pdf"):
        path = f"{a.out}.{ext}"
        fig.write_image(path, scale=3 if ext == "png" else 1)
        print("wrote", path)
    print(f"{len(rows)} isolates, {len(order)} nodes, {len(edges)} flows")


if __name__ == "__main__":
    main()

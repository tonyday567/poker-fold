#!/usr/bin/env python3
"""Parse results2.md and render an SVG trace of the coordinate-ascent path."""

import re
import sys
from pathlib import Path


def parse_thresholds(text):
    """Extract the six (t0,t1,t2,t3) tuples from a Strategy3 code block or a strategy file."""
    # Strategy-file format: lines like "C0: 0.1, 0.2, 0.3, 0.4"
    file_lines = []
    for line in text.splitlines():
        m = re.match(r"C\d+:\s*([0-9.eE\-+,\s]+)", line)
        if m:
            vals = [float(x.strip()) for x in m.group(1).split(",") if x.strip()]
            if len(vals) == 4:
                file_lines.append(tuple(vals))
    if len(file_lines) == 6:
        return file_lines

    # Code-block format: "Strategy3 [ (t0,t1,t2,t3) -- C0, ... ]"
    lines = [line for line in text.splitlines() if "Strategy3" not in line]
    block = re.sub(r"--[^\n]*", "", "\n".join(lines))
    nums = list(map(float, re.findall(r"[-+]?[0-9]*\.?[0-9]+(?:[eE][-+]?[0-9]+)?", block)))
    if len(nums) != 24:
        return None
    return [tuple(nums[i : i + 4]) for i in range(0, 24, 4)]


def sb_width(ts):
    """SB opening width: fraction of hands not folded in C0."""
    return 1.0 - ts[0][0]


def bb_aggression(ts):
    """Average of BB aggressive/defensive actions across C1, C2, C3.

    C1 has no fold (fold/call both map to a call), so the stored tuple is
    (call_start, call_start, raise_ai, 1.0).  The aggressive bands are the
    raise-3bb band (raise_ai - call_start) and the all-in band (1 - raise_ai).
    """
    c1_raise_allin = (ts[1][2] - ts[1][1]) + (ts[1][3] - ts[1][2])
    c2_allin = ts[2][3] - ts[2][1]
    c3_call = ts[3][1] - ts[3][0]
    return (c1_raise_allin + c2_allin + c3_call) / 3.0


def parse_steps(path):
    text = Path(path).read_text()
    # Each step has a header, a code block, then an EV line.
    pattern = re.compile(
        r"## Step (\d+).*?\n```haskell\n(.*?)\n```.*?"
        r"\n- \*\*EV \(candidate vs candidate\):\*\*\s*([-+eE0-9.]+)",
        re.DOTALL,
    )
    steps = []
    for m in pattern.finditer(text):
        step = int(m.group(1))
        ts = parse_thresholds(m.group(2))
        ev = float(m.group(3))
        if ts is None:
            continue
        steps.append((step, ev, ts))
    return steps


PATH_COLORS = ["#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b"]


def make_svg(path_groups, out_path, marker_files=None):
    if not path_groups:
        print("No paths found")
        return

    marker_files = marker_files or {}
    markers = {}
    for label, path in marker_files.items():
        with Path(path).open() as f:
            block = f.read()
        ts = parse_thresholds(block)
        if ts is not None:
            markers[label] = ts

    all_points = [(sb_width(ts), bb_aggression(ts), ev) for _, steps in path_groups for _, ev, ts in steps]
    evs = [ev for _, _, ev in all_points]
    xs = [x for x, _, _ in all_points]
    ys = [y for _, y, _ in all_points]

    margin = 60
    width = 900
    height = 700
    plot_w = width - 2 * margin
    plot_h = height - 2 * margin

    min_x, max_x = min(xs), max(xs)
    min_y, max_y = min(ys), max(ys)
    min_x = max(0.0, min_x - 0.05)
    max_x = min(1.0, max_x + 0.05)
    min_y = max(0.0, min_y - 0.05)
    max_y = min(1.0, max_y + 0.05)

    for ts in markers.values():
        x = sb_width(ts)
        y = bb_aggression(ts)
        min_x, max_x = min(min_x, x), max(max_x, x)
        min_y, max_y = min(min_y, y), max(max_y, y)

    def sx(x):
        return margin + (x - min_x) / (max_x - min_x) * plot_w

    def sy(y):
        return height - margin - (y - min_y) / (max_y - min_y) * plot_h

    ev_min, ev_max = min(evs), max(evs)

    def color(ev):
        if ev_max == ev_min:
            t = 0.5
        else:
            t = (ev - ev_min) / (ev_max - ev_min)
        r = int(255 * (1 - t))
        b = int(255 * t)
        return f"#{r:02x}00{b:02x}"

    lines = []
    lines.append(f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}">')
    lines.append('<rect width="100%" height="100%" fill="white"/>')

    lines.append(
        f'<text x="{width/2}" y="{30}" text-anchor="middle" font-size="18" font-weight="bold">Coordinate-ascent paths with local optima marked</text>'
    )

    ticks = [0.0, 0.2, 0.4, 0.6, 0.8, 1.0]
    for t in ticks:
        tx = sx(t)
        ty = sy(t)
        lines.append(f'<line x1="{tx:.2f}" y1="{margin}" x2="{tx:.2f}" y2="{height-margin}" stroke="#e0e0e0" stroke-width="1"/>')
        lines.append(f'<line x1="{margin}" y1="{ty:.2f}" x2="{width-margin}" y2="{ty:.2f}" stroke="#e0e0e0" stroke-width="1"/>')
        lines.append(f'<line x1="{tx:.2f}" y1="{height-margin}" x2="{tx:.2f}" y2="{height-margin+5}" stroke="black"/>')
        lines.append(f'<text x="{tx:.2f}" y="{height-margin+20}" text-anchor="middle" font-size="11">{t:.1f}</text>')
        lines.append(f'<line x1="{margin-5}" y1="{ty:.2f}" x2="{margin}" y2="{ty:.2f}" stroke="black"/>')
        lines.append(f'<text x="{margin-10}" y="{ty:.2f}" dy="4" text-anchor="end" font-size="11">{t:.1f}</text>')

    lines.append(
        f'<line x1="{margin}" y1="{height-margin}" x2="{width-margin}" y2="{height-margin}" stroke="black" stroke-width="2"/>'
    )
    lines.append(
        f'<line x1="{margin}" y1="{margin}" x2="{margin}" y2="{height-margin}" stroke="black" stroke-width="2"/>'
    )

    lines.append(
        f'<text x="{width/2}" y="{height-10}" text-anchor="middle" font-size="14">SB opening width (1 - C0_t0)</text>'
    )
    lines.append(
        f'<text x="{20}" y="{height/2}" text-anchor="middle" font-size="14" transform="rotate(-90 {20},{height/2})">BB aggression index</text>'
    )

    lines.append('<defs>')
    lines.append('  <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto" markerUnits="strokeWidth">')
    lines.append('    <path d="M0,0 L0,6 L9,3 z" fill="#666"/>')
    lines.append('  </marker>')
    lines.append('</defs>')

    for gi, (label, steps) in enumerate(path_groups):
        stroke = PATH_COLORS[gi % len(PATH_COLORS)]
        pts = " ".join(f"{sx(sb_width(ts)):.2f},{sy(bb_aggression(ts)):.2f}" for _, _, ts in steps)
        lines.append(f'<polyline points="{pts}" fill="none" stroke="{stroke}" stroke-width="1.5" marker-mid="url(#arrow)"/>')

        for i, (step, ev, ts) in enumerate(steps):
            x, y = sx(sb_width(ts)), sy(bb_aggression(ts))
            if i % 5 == 0 or i == len(steps) - 1:
                r = 5
                lines.append(
                    f'<circle cx="{x:.2f}" cy="{y:.2f}" r="{r}" fill="{color(ev)}" stroke="{stroke}" stroke-width="1.5">'
                    f'<title>{label} step {step}\nEV: {ev:.5f}\nSB width: {sb_width(ts):.3f}\nBB agg: {bb_aggression(ts):.3f}</title>'
                    f'</circle>'
                )
            else:
                r = 2.5
                lines.append(
                    f'<circle cx="{x:.2f}" cy="{y:.2f}" r="{r}" fill="{color(ev)}" stroke="{stroke}" stroke-width="0.5">'
                    f'<title>{label} step {step}\nEV: {ev:.5f}\nSB width: {sb_width(ts):.3f}\nBB agg: {bb_aggression(ts):.3f}</title>'
                    f'</circle>'
                )

        if steps:
            sx0, sy0 = sx(sb_width(steps[0][2])), sy(bb_aggression(steps[0][2]))
            sx1, sy1 = sx(sb_width(steps[-1][2])), sy(bb_aggression(steps[-1][2]))
            lines.append(
                f'<text x="{sx0:.2f}" y="{sy0:.2f}" dx="-30" dy="-8" font-size="10" fill="{stroke}" font-weight="bold">{label} start</text>'
            )
            lines.append(
                f'<text x="{sx1:.2f}" y="{sy1:.2f}" dx="10" dy="8" font-size="10" fill="{stroke}" font-weight="bold">{label} end</text>'
            )

    marker_colors = {"S0": "green", "S1": "orange", "S2": "purple"}
    for label, ts in markers.items():
        x, y = sx(sb_width(ts)), sy(bb_aggression(ts))
        col = marker_colors.get(label, "black")
        lines.append(f'<circle cx="{x:.2f}" cy="{y:.2f}" r="7" fill="{col}" stroke="black" stroke-width="2"/>')
        dx = 12 if x < width * 0.7 else -12
        anchor = "start" if x < width * 0.7 else "end"
        lines.append(
            f'<text x="{x:.2f}" y="{y:.2f}" dx="{dx}" dy="4" text-anchor="{anchor}" font-size="12" font-weight="bold" fill="{col}">{label}</text>'
        )

    # Path legend
    legend_y = margin - 60
    for gi, (label, _) in enumerate(path_groups):
        stroke = PATH_COLORS[gi % len(PATH_COLORS)]
        lines.append(f'<line x1="{margin}" y1="{legend_y}" x2="{margin+20}" y2="{legend_y}" stroke="{stroke}" stroke-width="2"/>')
        lines.append(f'<text x="{margin+25}" y="{legend_y}" dy="4" font-size="11" fill="black">{label}</text>')
        legend_y += 16

    # EV color scale
    for k in range(11):
        t = k / 10.0
        ev = ev_min + t * (ev_max - ev_min)
        cx = width - margin - 160 + t * 120
        cy = margin - 30
        col = color(ev)
        lines.append(f'<rect x="{cx:.2f}" y="{cy}" width="12" height="12" fill="{col}" stroke="none"/>')
    lines.append(
        f'<text x="{width-margin-160}" y="{margin-40}" font-size="11" fill="black">EV (SB): {ev_min:.3f}</text>'
    )
    lines.append(
        f'<text x="{width-margin-40}" y="{margin-40}" font-size="11" fill="black" text-anchor="end">{ev_max:.3f}</text>'
    )

    lines.append("</svg>")
    Path(out_path).write_text("\n".join(lines))
    total = sum(len(steps) for _, steps in path_groups)
    print(f"Wrote {out_path} with {total} points across {len(path_groups)} path(s)")


ALWAYS_CALL = [(0.0, 1.0, 1.0, 1.0)] * 6


def main():
    in_file = sys.argv[1] if len(sys.argv) > 1 else "results2.md"
    out_file = sys.argv[2] if len(sys.argv) > 2 else "optimization-trace.svg"
    steps = parse_steps(in_file)

    if "always-call" in out_file:
        # Isolate the last always-call run: the final block of steps where
        # step numbering reset to 1 and the SB never folds.
        starts = [i for i, (step, _, ts) in enumerate(steps) if step == 1 and abs(ts[0][0]) < 1e-12]
        if starts:
            steps = steps[starts[-1]:]

    make_svg(steps, out_file)


def add_marker(lines, x, y, label, color, width, height, margin, min_x, max_x, min_y, max_y):
    sx = margin + (x - min_x) / (max_x - min_x) * (width - 2 * margin)
    sy = height - margin - (y - min_y) / (max_y - min_y) * (height - 2 * margin)
    lines.append(f'<circle cx="{sx:.2f}" cy="{sy:.2f}" r="6" fill="{color}" stroke="black" stroke-width="2"/>')
    dx = 12 if sx < width * 0.7 else -12
    anchor = "start" if sx < width * 0.7 else "end"
    lines.append(f'<text x="{sx:.2f}" y="{sy:.2f}" dx="{dx}" dy="4" text-anchor="{anchor}" font-size="12" font-weight="bold" fill="{color}">{label}</text>')


def main():
    args = sys.argv[1:]
    out_file = "optimization-trace.svg"
    if args and not args[0].startswith(("path:", "marker:")):
        out_file = args[0]

    path_groups = []
    marker_files = {}
    for arg in args:
        if arg.startswith("path:"):
            rest = arg[len("path:"):]
            if "=" not in rest:
                continue
            label, path = rest.split("=", 1)
            steps = parse_steps(path)
            if "always-call" in label.lower():
                starts = [i for i, (step, _, ts) in enumerate(steps) if step == 1 and abs(ts[0][0]) < 1e-12]
                if starts:
                    steps = steps[starts[-1]:]
            path_groups.append((label, steps))
        elif arg.startswith("marker:"):
            rest = arg[len("marker:"):]
            if "=" not in rest:
                continue
            label, path = rest.split("=", 1)
            marker_files[label] = path
        elif "=" in arg:
            # Backwards compatibility: marker files passed as LABEL=file.strat
            label, path = arg.split("=", 1)
            marker_files[label] = path

    if not path_groups:
        in_file = args[0] if args else "results2.md"
        steps = parse_steps(in_file)
        if "always-call" in out_file:
            starts = [i for i, (step, _, ts) in enumerate(steps) if step == 1 and abs(ts[0][0]) < 1e-12]
            if starts:
                steps = steps[starts[-1]:]
        path_groups.append(("path", steps))

    make_svg(path_groups, out_file, marker_files)


if __name__ == "__main__":
    main()

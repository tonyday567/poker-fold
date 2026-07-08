#!/usr/bin/env python3
"""Greedy coordinate-ascent continuation for 3bb/all-in poker strategy.

Starts from the candidate in app/opt3bb.hs, runs the executable to obtain
self-play EV and a finite-difference gradient table, then repeatedly moves
one threshold at a time along the signed gradient until no coordinate in a
sweep improves EV.

Each accepted step is appended to results.md.
"""

import os
import re
import subprocess
import sys

ROOT = "/Users/tonyday567/haskell/poker-fold"
SOURCE = os.path.join(ROOT, "app", "opt3bb.hs")
RESULTS = os.path.join(ROOT, "results.md")
EXE = os.path.join(
    ROOT,
    "dist-newstyle",
    "build",
    "aarch64-osx",
    "ghc-9.14.1",
    "poker-fold-0.1.1.0",
    "x",
    "poker-fold-opt3bb",
    "build",
    "poker-fold-opt3bb",
    "poker-fold-opt3bb",
)

MAX_STEPS = 50
EPS = 1e-12

# Starting candidate given by the user.
START_CANDIDATE = [
    [0.46, 0.70, 0.90, 1.00],  # C0
    [0.00, 0.49, 0.69, 1.00],  # C1
    [0.50, 0.73, 1.00, 1.00],  # C2
    [0.55, 1.00, 1.00, 1.00],  # C3
    [0.20, 0.50, 1.00, 1.00],  # C4
    [0.20, 1.00, 1.00, 1.00],  # C5
]

CTX_LABELS = [
    "C0 : SB first action",
    "C1 : BB after SB call",
    "C2 : BB facing SB raise 3bb",
    "C3 : BB facing SB all-in",
    "C4 : SB after calling, facing BB raise 3bb",
    "C5 : SB after calling, facing BB all-in",
]


def fmt_num(x: float) -> str:
    """Format a threshold for human-readable output.

    Keep two decimals when that is exact; otherwise use three with trailing
    zeros dropped.
    """
    if abs(round(x, 2) - x) < 1e-9:
        return f"{x:.2f}"
    s = f"{x:.3f}"
    if "." in s:
        s = s.rstrip("0").rstrip(".")
    return s


def parse_run(text: str):
    """Return (ev, compact_line, gradients) from the executable output."""
    ev = None
    compact = None
    grads = [None] * 6
    for line in text.splitlines():
        m = re.search(r"candidate vs candidate:\s*([-\deE.]+)", line)
        if m:
            ev = float(m.group(1))
        if line.strip().startswith("C0:"):
            compact = line.strip()
        m2 = re.match(
            r"\s*C(\d):\s+([-\d.eE]+)\s+([-\d.eE]+)\s+([-\d.eE]+)\s+([-\d.eE]+)",
            line,
        )
        if m2:
            i = int(m2.group(1))
            grads[i] = [float(m2.group(k)) for k in range(2, 6)]
    if ev is None or compact is None or any(g is None for g in grads):
        raise RuntimeError(f"Failed to parse executable output:\n{text}")
    return ev, compact, grads


def run_eval():
    """Build and run the executable; return parsed output."""
    build = subprocess.run(
        ["cabal", "build", "poker-fold:exe:poker-fold-opt3bb"],
        cwd=ROOT,
        capture_output=True,
        text=True,
        timeout=120,
    )
    if build.returncode != 0:
        raise RuntimeError(f"cabal build failed:\n{build.stderr}\n{build.stdout}")
    proc = subprocess.run(
        [EXE],
        cwd=ROOT,
        capture_output=True,
        text=True,
        timeout=120,
    )
    if proc.returncode != 0:
        raise RuntimeError(f"executable failed:\n{proc.stderr}\n{proc.stdout}")
    return parse_run(proc.stdout)


def write_source(candidate):
    """Rewrite only the six threshold tuple lines in opt3bb.hs."""
    with open(SOURCE, "r") as f:
        lines = f.readlines()

    tuple_pat = re.compile(
        r"(\(\s*)[-\d.]+(\s*,\s*)[-\d.]+(\s*,\s*)[-\d.]+(\s*,\s*)[-\d.]+(\s*\))"
    )

    for i, label in enumerate(CTX_LABELS):
        for idx, line in enumerate(lines):
            if label in line:
                v = candidate[i]
                lines[idx] = tuple_pat.sub(
                    lambda m: (
                        f"{m.group(1)}{fmt_num(v[0])}{m.group(2)}"
                        f"{fmt_num(v[1])}{m.group(3)}{fmt_num(v[2])}"
                        f"{m.group(4)}{fmt_num(v[3])}{m.group(5)}"
                    ),
                    line,
                )
                break
        else:
            raise RuntimeError(f"Could not find tuple line for {label}")

    with open(SOURCE, "w") as f:
        f.writelines(lines)


def feasible(candidate, i, j, delta):
    """Check whether moving coordinate (i,j) by delta keeps the tuple valid."""
    new_val = candidate[i][j] + delta
    if not (0.0 <= new_val <= 1.0):
        return False
    tup = list(candidate[i])
    tup[j] = new_val
    if any(tup[k] > tup[k + 1] + 1e-12 for k in range(3)):
        return False
    # C4/C5 are offsets above C0_t0.
    if i in (4, 5):
        c0t0 = candidate[0][0]
        if new_val < 0.0:
            return False
        if c0t0 + new_val > 1.0 + 1e-12:
            return False
    return True


def format_frequencies(candidate):
    """Reproduce the compact frequency bullet list used in results.md."""
    c = candidate
    lines = []
    # C0: fold / call / raise 3bb / all-in
    pcts = [round(c[0][0] * 100), round((c[0][1] - c[0][0]) * 100),
            round((c[0][2] - c[0][1]) * 100), round((c[0][3] - c[0][2]) * 100)]
    lines.append(f"  - C0: {pcts[0]}% fold / {pcts[1]}% call / {pcts[2]}% raise 3bb / {pcts[3]}% all-in")
    # C1: call / raise 3bb / all-in
    pcts = [round(c[1][1] * 100), round((c[1][2] - c[1][1]) * 100),
            round((c[1][3] - c[1][2]) * 100)]
    lines.append(f"  - C1: {pcts[0]}% call / {pcts[1]}% raise 3bb / {pcts[2]}% all-in")
    # C2: fold / call / all-in
    pcts = [round(c[2][0] * 100), round((c[2][1] - c[2][0]) * 100),
            round((1.0 - c[2][1]) * 100)]
    lines.append(f"  - C2: {pcts[0]}% fold / {pcts[1]}% call / {pcts[2]}% all-in")
    # C3: fold / call
    pcts = [round(c[3][0] * 100), round((1.0 - c[3][0]) * 100)]
    lines.append(f"  - C3: {pcts[0]}% fold / {pcts[1]}% call")
    # C4: fold / call / all-in (within C0 call band)
    pcts = [round(c[4][0] * 100), round((c[4][1] - c[4][0]) * 100),
            round((1.0 - c[4][1]) * 100)]
    lines.append(f"  - C4: {pcts[0]}% fold / {pcts[1]}% call / {pcts[2]}% all-in (within C0 call band)")
    # C5: fold / call (within C0 call band)
    pcts = [round(c[5][0] * 100), round((1.0 - c[5][0]) * 100)]
    lines.append(f"  - C5: {pcts[0]}% fold / {pcts[1]}% call (within C0 call band)")
    return "\n".join(lines)


def format_literal(candidate):
    """Return the Strategy3 literal block used in results.md."""
    lines = ["```haskell", "Strategy3", "  ["]
    for i, tup in enumerate(candidate):
        comma = "," if i < 5 else ""
        vals = ", ".join(fmt_num(v) for v in tup)
        lines.append(f"    ({vals}){comma} -- {CTX_LABELS[i]}")
    lines.append("  ]")
    lines.append("```")
    return "\n".join(lines)


def format_grad_table(grads):
    """Return the markdown gradient table used in results.md."""
    rows = ["|     | t0      | t1      | t2      | t3  |",
            "|-----|---------|---------|---------|-----|"]
    for i in range(6):
        vals = " | ".join(f"{grads[i][j]:.4f}" for j in range(4))
        rows.append(f"| C{i}  | {vals} |")
    # locate highest magnitude gradient
    max_val = max((abs(grads[i][j]), i, j) for j in range(4) for i in range(6))
    _, hi, hj = max_val
    rows.append("")
    rows.append(f"Highest magnitude gradient: **C{hi}_t{hj} = {grads[hi][hj]:.4f}**.")
    return "\n".join(rows)


def append_section(step_no, coord, old_val, step_size, new_val, ev, candidate, grads):
    """Append one accepted step to results.md."""
    i, j = coord
    sign = "+" if step_size > 0 else "-"
    pct = f"{abs(step_size) * 100:g}%"
    heading = f"## Step {step_no} — C{i}_t{j}: {fmt_num(old_val)} → {fmt_num(new_val)} ({sign}{pct})"
    body = (
        f"\n{heading}\n\n"
        f"{format_literal(candidate)}\n\n"
        f"- **EV (candidate vs candidate):** `{ev}`\n"
        f"- **Frequencies:**\n"
        f"{format_frequencies(candidate)}\n\n"
        f"### Finite-difference gradients (±1% quantile)\n\n"
        f"{format_grad_table(grads)}\n\n"
    )
    with open(RESULTS, "a") as f:
        f.write(body)


def append_summary(final_ev, total_new_steps):
    body = (
        f"## Final summary\n\n"
        f"- **Final EV (candidate vs candidate):** `{final_ev}`\n"
        f"- **Accepted steps in this run:** {total_new_steps}\n"
        f"- **Cumulative accepted steps in results.md:** {13 + total_new_steps}\n\n"
    )
    with open(RESULTS, "a") as f:
        f.write(body)


def main():
    candidate = [list(t) for t in START_CANDIDATE]
    write_source(candidate)

    ev, compact, grads = run_eval()
    print(f"Initial EV: {ev}")

    step_no = 13
    accepted = 0
    blocked = set()

    while accepted < MAX_STEPS:
        # Find active coordinate with largest |gradient|.
        active = [(i, j) for i in range(6) for j in range(4) if (i, j) not in blocked]
        if not active:
            print("Sweep complete with no accepted step.")
            break

        coord = max(active, key=lambda c: abs(grads[c[0]][c[1]]))
        i, j = coord
        g = grads[i][j]
        if abs(g) < 1e-15:
            print(f"All active gradients are zero; stopping.")
            break

        direction = 1.0 if g > 0 else -1.0
        print(f"Trying C{i}_t{j} (grad={g:.4f}, direction={direction:+.0f})")

        # Candidate step sizes in preference order.
        sizes = [s for s in (0.01, 0.005) if feasible(candidate, i, j, direction * s)]
        if not sizes:
            print(f"  No feasible step for C{i}_t{j}; blocking.")
            blocked.add(coord)
            continue

        accepted_this_coord = False
        for step_size in sizes:
            old_val = candidate[i][j]
            new_val = old_val + direction * step_size
            candidate[i][j] = new_val
            write_source(candidate)
            try:
                new_ev, new_compact, new_grads = run_eval()
            except Exception as e:
                print(f"  Run failed for step {step_size}: {e}")
                candidate[i][j] = old_val
                write_source(candidate)
                continue
            print(f"  step {step_size:+.3f}: EV {ev} -> {new_ev}")
            if new_ev > ev + EPS:
                step_no += 1
                accepted += 1
                append_section(step_no, coord, old_val, direction * step_size,
                               new_val, new_ev, candidate, new_grads)
                ev, grads = new_ev, new_grads
                blocked = set()
                accepted_this_coord = True
                print(f"  Accepted step {step_no}; new EV = {ev}")
                break
            else:
                # Revert and try next step size.
                candidate[i][j] = old_val
                write_source(candidate)

        if not accepted_this_coord:
            print(f"  No improvement for C{i}_t{j}; blocking.")
            blocked.add(coord)

    append_summary(ev, accepted)
    print(f"Finished. Accepted {accepted} new step(s). Final EV = {ev}")


if __name__ == "__main__":
    main()

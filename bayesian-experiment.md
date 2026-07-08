# Bayesian Belief Tracking in Preflop Poker

## Motivation

In heads-up poker, a key skill is identifying the opponent's playing style and
adapting. A professional typically needs ~8 hands to form a read. We model this
as Bayesian inference over a discrete set of "centroid" strategies, with a
Dirichlet-categorical observation model.

## Model

### Centroids

We define three opponent play styles by shifting the baseline strategy's
thresholds uniformly:

| Style    | δ     | Description |
|----------|-------|-------------|
| loose    | -0.25 | Calls and raises wider than baseline |
| baseline |  0.00 | Equilibrium-ish strategy |
| tight    | +0.25 | Folds more, rarely raises |

Thresholds are shifted by `t' = t + δ·(1-t)` for tightening (δ > 0) and
`t' = t·(1+δ)` for loosening (δ < 0), then clamped to [0,1] and re-sorted.

### Observation Model

For each centroid `c` and observed opponent action `(context, raw_action)`, the
likelihood `P(raw | c, context)` is computed by integrating over all 169 hand
classes weighted by combo frequency:

```
P(raw | c, ctx) = Σ_hc combo_weight(hc) · 1[action(c, ctx, hc) == raw] / Σ_hc combo_weight(hc)
```

The action `action(c, ctx, hc)` uses the same logic as the game simulation:
`intendedAction(thresholds, equity)` → `clampAction` to nearest legal action.

### Bayesian Update

```
P(c | obs) ∝ P(obs | c) · P(c)
```

With an epsilon floor (ε = 0.01) applied after each update to prevent
permanent lock-in: `P'(c) = (P(c) + ε) / (1 + K·ε)`.

## Experiment 1: Style Identification

SB plays a fixed baseline strategy. BB plays one of the three styles.
SB maintains a belief vector over BB's style, updated after each hand.

**Result (200 hands):**

| BB style  | Final belief              | SB profit/hand |
|-----------|---------------------------|----------------|
| loose     | loose=1.00                | +0.04          |
| baseline  | baseline=0.94 tight=0.06  | +0.13          |
| tight     | baseline=0.51 tight=0.49  | +0.14          |

Loose is correctly identified. Baseline is 94% correct. Tight is
ambiguous (51/49) — at δ=±0.25, tight and baseline differ by only ~6%
in C1 raise frequency, requiring more than 200 hands to distinguish.

## Experiment 2: Level-2 Deception

Can we trick a Level-1 thinker? SB plays a "decoy" style for N hands to
manipulate BB's belief, then switches to exploit BB's misadjustment.

**Setup:**
- SB centroids: loose (-0.25), baseline (0), tight (+0.25) on seat0Baseline
- BB centroids: same on seat1Baseline
- BB adapts: effective δ = -(w_loose·(-0.25) + w_base·0 + w_tight·0.25)
  (BB tightens if SB looks loose, loosens if SB looks tight)
- SB: plays tight for hands 1-20, then switches to loose for hands 21-100

**Result (100 hands):**

```
hand  phase    profit  BB-thinks-SB-is
1     decoy    -1.0    tight-SB=0.70
4     decoy    -2.0    base-SB=0.92
5     decoy    -0.5    tight-SB=0.98   ← BB buys the decoy
20    decoy     2.0    loose-SB=0.50   ← epsilon slowly erodes certainty
21    exploit   5.0    loose-SB=0.94   ← SB switches, BB detects
100   exploit   2.0    loose-SB=0.93   ← BB correctly re-identifies
```

| Phase        | Hands | SB profit | /hand |
|-------------|-------|-----------|-------|
| Decoy        | 1-20  | -4.0      | -0.20 |
| Exploit      | 21-100| +36.0     | +0.45 |
| **Total**    | 1-100 | +32.0     | +0.32 |

The epsilon floor (ε=0.01) is essential: without it, once BB's belief
reaches P(tight)=1.0, zero-weight hypotheses can never recover. With ε,
BB's certainty erodes from 0.98 to 0.50 by hand 20, allowing rapid
re-identification after the switch.

## Limitations

1. **Hand-class aggregation:** The threshold model maps equity→action at the
   hand-class level. There is no "trash your reputation with 72o" because all
   hands in a class behave identically. Individual hand deception requires
   per-hand strategies.

2. **Centroid design:** Uniform δ-shift of a single baseline produces
   centroids that are only weakly distinguishable. Real playing styles differ
   qualitatively (e.g., a maniac doesn't just call wider — they 3-bet polar).

3. **BB's counter-strategy:** BB adapts by shifting thresholds, not by
   computing a true best response. A proper Level-1 BB would run CFR or
   analytical best-response against its belief.

4. **Information rate:** 200 hands yields ~20-30 observations per BB context.
   With 3 centroids differing by 6-20% in action probabilities, this gives
   ~2-4 bits per context — enough to distinguish loose from baseline, but
   marginal for tight vs baseline.

## Code

- `app/bayesian-adapt.hs` — main executable
- `--opponent loose|baseline|tight` — Level 1 belief tracking
- `--deceive` — Level 2 deception experiment
- Build: `cabal build poker-fold:exe:poker-fold-bayesian`

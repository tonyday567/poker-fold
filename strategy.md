# Strategy State

## 1. The existing game

The current playable game is a **heads-up push-fold approximation**.

- 2 seats, 10bb stacks, 0.5bb / 1.0bb blinds.
- Each player can take only three actions: **Fold**, **Call**, **Raise 10** (which is all-in).
- There are 5 decision points per hand:

  | index | seat | situation                              |
  |------:|------|----------------------------------------|
  | 0     | 0 (SB/UTG) | first action preflop             |
  | 1     | 0          | UTG calls then acts again? (call %) |
  | 2     | 1 (BB)     | after UTG calls, BB re-raises       |
  | 3     | 0          | UTG calls the BB re-raise           |
  | 4     | 1          | BB calls a UTG open-raise           |

  (See `Poker.Range.ev2Ranges` for the exact parameter mapping.)

- A strategy is a vector of 5 real numbers
  `θ = [s0r, s0c, s1r, s2r, s3r]`
  fed to `rcf`.

### 1.1 Strategy representation: `rcf`

```haskell
rcf :: Range Double -> Double -> Double -> Double -> Range RawAction
rcf strength raisePct callPct = ...
```

- `strength` is a precomputed ordering of starting hands (e.g. `o2`, the win-rate of each starting hand against a random hand in a 2-player game).
- `raisePct` cuts off the top x% of hands for a raise.
- `callPct`  cuts off the top y% of hands for a call; everything below folds.

So each decision point is just a pair of thresholds on a monotone hand-strength ordering.

### 1.2 Evaluating a strategy

`ev2Ranges hvs strength sims θ` simulates `sims` hands with both players using the threshold policies derived from `θ`, then reports the EV of seat 0.

Known edge cases:

- `θ = [0,0,0,0,0]`  → always fold → EV ≈ -0.5 (lose the small blind).
- `θ = [1,_,1,_]`    → always raise, and if the opponent always folds, EV ≈ +1.0.
- Symmetric all-in strategies converge to roughly zero EV.

### 1.3 What is the "optimal strategy" here?

For this tiny game the optimal strategy is the best-response vector `θ*` that maximizes `ev2Ranges` against a fixed opponent `θ_opp`. Because the game is so constrained, the optimization is just a search over a 5-D threshold space. In practice the landscape is noisy from Monte Carlo variance, so gradient descent / finite differences need enough samples to be trustworthy.

The key takeaway: the existing strategy is **a pair of percentile thresholds on a hand-strength ordering**, evaluated by simulation.

---

## 2. Extending to multi-round / opponent-modeling

The natural next step is to keep the same percentile-threshold idea but let it vary by street and situation, then add a layer of **opponent-parameter inference**.

### 2.1 Multi-round game

Add flop, turn, and river betting rounds with the same discrete action abstraction (`Fold / Check / Call / Bet size / AllIn`) already in `Poker.Action`.

At each decision point the hero uses a policy of the same *shape* as `rcf`:

```
action = thresholdPolicy(board-strength, hand-strength, pot-odds, position)
```

The difference is that the hand-strength ordering is now **board-dependent equity** rather than preflop `o2`.

### 2.2 Enemy strategy guess

The opponent’s true strategy is some fixed parameter vector `θ_opp` (or a small mixture of such vectors). It determines, for every info set, which percentile thresholds they use.

Across repeated hands the hero:

1. Maintains a belief over `θ_opp`.
2. After each observed opponent action, updates that belief (Bayesian update).
3. Plays the best response to the posterior over `θ_opp`.

This is the upgrade: from “play a fixed optimal threshold vector” to “observe, infer the opponent’s threshold vector, then exploit it.”

### 2.3 First concrete experiment

To make this testable without solving full poker:

1. Fix a small family of opponent strategies, e.g. `tight`, `loose`, `call-station`, each represented by a few percentile parameters.
2. Randomly draw the opponent’s type at the start of a session.
3. Let the hero observe a few hands, update a posterior over the type, and then best-respond.
4. Measure whether the hero’s EV improves versus a baseline that ignores the opponent type.

This keeps the multi-round extension grounded in the existing percentile-threshold machinery while adding the missing piece: a guess about the enemy’s *future* actions.

## Baseline optimal strategies

Instead of a uniformly random opponent, we first found a baseline pair of
position-dependent strategies using alternating best-response random search.

- `Poker.Strategy.seat0Baseline` — discovered strategy for the small blind / first actor.
- `Poker.Strategy.seat1Baseline` — discovered strategy for the big blind.

These were found by repeatedly fixing one seat's strategy and random-searching
the other seat's best response. After a few iterations the mutual EV stabilised
around **+0.35 bb/hand for seat 0** (and therefore -0.35 bb/hand for seat 1).

Named edge cases for comparison (seat 0 EV vs self):

| strategy     | EV vs self |
|--------------|------------|
| alwaysFold   | 0.0        |
| alwaysCall   | 0.45       |
| alwaysAllIn  | 0.01       |
| discovered seat0Baseline | ~0.35 vs seat1Baseline |

The discovered baselines are not guaranteed to be exact Nash equilibrium, but
they are a sensible, non-random reference point for the adaptive experiment.

## Experiment result

Implemented in `src/Poker/Strategy.hs`, `src/Poker/SimpleBelief.hs` and
`app/adapt.hs`.

- Opponent uses `seat1Baseline`.
- Two belief systems run in parallel after every hand:
  1. A lightweight **action-frequency belief** (`SimpleBelief`) that does not
     use hole-card information. It updates an EMA of opponent action frequencies
     per context and recovers thresholds as cumulative probabilities.
  2. The **particle filter** kept as a check (it does use the revealed hole
     cards).
- The hero acts from the simple belief, adjusting C2 (facing a raise) and C0
  (opening) heuristically.
- Smoke test over 200 hands:
  - Cumulative hero profit: **+85.0 bb** (~+0.43 bb/hand).
  - Runtime ~1 second for 200 hands, with no artificial 20-hand update delay.
  - Effective sample size stays healthy (~950/1000) for the particle check.
  - The simple C2 threshold moves faster and more extremely than the particle
    posterior (it often collapses to 0, i.e. fold everything facing a raise),
    while the particle posterior settles around 0.30–0.33. This shows the simple
    updater is more reactive but less calibrated.

This confirms the simple updater is viable and much cheaper than the particle
approach. The particle filter remains a useful sanity check.

## Open questions

- Should the simple updater use a smaller learning rate / gain so it does not
  collapse to folding so quickly?
- Can we make the simple updater more calibrated by weighting observations by
  the strength of the hero's own hand (i.e. a tighter bound on opponent equity)?
- Should the hero also adapt its C0 open-raise thresholds, and with what gain?
- Can we tighten the alternating best-response loop to get closer to a true
  Nash equilibrium, or at least measure exploitability more accurately?

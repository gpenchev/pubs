# How the threat index is calculated — a plain explanation

---

## What we are trying to measure

We want to know how much military threat each country faces in each year.
Not how worried people are, not what newspapers say — but how close real
armed conflict actually is to each country's border, and how deadly that
conflict is.

---

## Step 1 — Start with a database of real conflict events

We use the UCDP Georeferenced Event Dataset. This is a global database
maintained by Uppsala University that records every organised armed
conflict event — battles, clashes, military operations — going back
decades. Each event has three pieces of information we need: where it
happened (GPS coordinates), when it happened (year), and how many people
were killed (best estimate of battle deaths).

We keep only state-based conflicts — wars and military clashes involving
at least one government army. Riots, protests, and criminal violence are
excluded because they do not represent the kind of military threat that
drives a government to increase its defence budget.

---

## Step 2 — Remove conflicts that cannot realistically reach EU territory

Not every conflict near Europe is an equal threat. A war in Libya is
physically separated from Italy by 140 kilometres of open sea. A war in
Ukraine is reachable by land from Poland with no significant water
crossing. A military force can cross the Strait of Gibraltar (14 km) but
cannot practically invade across the central Mediterranean.

We draw a straight line from each conflict event to the nearest point on
the EU's external land border. We then measure how much of that line
crosses open ocean. If the sea crossing is more than 50 kilometres, the
conflict is classified as sea-separated and removed from the primary
threat score. If the sea crossing is 50 kilometres or less, the conflict
stays in.

In practice this means:
- Ukraine, Balkans, Caucasus — included. Reachable by land.
- Libya, Algeria, Syria, Lebanon — excluded. Mediterranean crossing too wide.
- Events near the Turkish-Bulgarian border — included. Land-connected.

This filter is the most important design choice in the whole measure. It
is what makes our threat index different from simply counting fatalities
near Europe. It focuses on threats that could plausibly turn into a
territorial military problem for EU member states.

---

## Step 3 — Measure how far each conflict is from each country's border

For every conflict event that passed the filter, we measure its distance
to each country in our sample. Crucially, we measure from the nearest
point on the country's actual border — not from the capital city or the
geographic centre.

This matters for large countries. Poland's eastern border is much closer
to Ukraine than Warsaw is. If we measured from Warsaw, we would
underestimate how exposed Poland actually is. By measuring from the
border, we capture where the country is genuinely vulnerable.

All distances are computed in kilometres using a proper geographic
projection that preserves accurate distances across Europe.

---

## Step 4 — Closer conflicts count more, deadlier conflicts count more

We now combine distance and fatalities into a single number for each
country in each year. Two principles guide this:

**Deadlier conflicts count more, but with diminishing returns.**
A conflict that kills 10,000 people is more threatening than one that
kills 100, but it is not 100 times more threatening. We take the
logarithm of fatalities, which compresses very large numbers and prevents
a single catastrophic event from completely dominating the score.

**Closer conflicts count more, exponentially.**
A conflict 100 kilometres from your border is far more threatening than
one 1,000 kilometres away. We apply an exponential decay with a bandwidth
of 500 kilometres. This means a conflict at 500 km contributes about 37%
of what it would contribute if it were on the border. At 1,000 km it
contributes about 14%. Beyond 1,500 km the contribution is negligible.

For each country and each year, we add up the contributions of all
qualifying conflict events. The result is the raw threat score.

---

## Step 5 — Final transformation

The raw score is then put through a logarithm one more time. This is
because the scores are still quite skewed — most country-years have low
or zero threat, while a few country-years (Poland in 2022, Bulgaria in
the 1990s Balkans) have very high scores. The second log compression
makes the variable better behaved for use in regression models. Years
with zero conflict activity near a country's border simply get a score
of zero.

---

## What the score looks like in practice

A country like Estonia or Poland in 2022 gets a high score — Ukraine is
close, the war is large, many people are dying near their land border.

A country like Spain or Portugal in any year gets a near-zero score —
there are no state-based military conflicts reachable by land from their
borders during the study period. The North African conflicts they are
close to are excluded by the sea-crossing filter.

Greece is an interesting case — it gets a moderately high score because
the Balkans conflicts of the 1990s were reachable by land, and later
because of proximity to Turkey-adjacent instability. But it gets a lower
score than its geographic closeness to the Middle East might suggest,
because Syria and Lebanon are sea-separated.

---

## How the threat index is used in the models

The threat score enters the models as a single variable for each country
in each year. It is treated the same way as the fiscal variables — it is
one of several factors that might explain why a country spends more or
less on defence in a given year.

In the baseline models (M1 to M3) it competes directly with the fiscal
variables — debt, deficit, economic growth — to explain spending. The
question is simply: after controlling for a country's financial situation,
does more nearby conflict lead to more spending?

In M4 and M7, the threat score is interacted with historical periods to
test whether the effect was stronger or weaker in different eras. In
M8, a broader version of the same score — one that includes sea-separated
conflicts — replaces it to test whether the land filter was the right
choice. In M13, the threat score is replaced entirely by a media-based
geopolitical risk index to test whether our physical measure does a
better job than a perception-based one.

In all cases the threat index is doing one job: representing, as
precisely as possible, the objective military pressure each country faced
in each year, so that the models can tell us whether that pressure
translated into actual defence budget decisions.

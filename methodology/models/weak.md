# Weaknesses and how we address them — a plain explanation

---

## 1. We cannot prove causation, only association

**The problem**
Even in the cleanest model (M3), we are showing that when threat goes up
spending tends to go up too. But we cannot be certain that threat is
*causing* the spending increase. It is possible that something else —
say, a change in government, or a new NATO commitment — caused both the
threat perception and the spending increase at the same time. This is the
fundamental limitation of observational data: we watch things happen, we
do not run controlled experiments.

**How we partially address it**
We control for as many alternative explanations as we can — fiscal
position, political ideology, economic growth, election cycles. We also
use country and year fixed effects to remove the most obvious confounders.
This does not prove causation but it rules out many competing
explanations. The more things we control for and the finding still holds,
the more confident we can be.

---

## 2. The Regime 4 result is based on very little data

**The problem**
Our most exciting finding — that threat started significantly affecting
spending after 2022 — is based on only two years of data (2022 and 2023).
With roughly 44 observations covering just one geopolitical event, the
statistical power is well below the threshold we normally require. The
finding could be real, or it could be a coincidence driven by a handful
of countries responding strongly to Ukraine while others did not.

**How we address it**
We are transparent about this in the paper. We label the Regime 4 result
as preliminary evidence, not a confirmed finding. We quantify exactly how
underpowered the test is and state how many additional years of data
would be needed to confirm it. As 2024 and 2025 data become available
the pipeline can be re-run and the finding either confirmed or revised.

---

## 3. The structural break tests have a technical flaw

**The problem**
We test whether the relationship between threat and spending changed at
specific historical moments — 2003, 2014, 2022. The standard test we use
for this was designed for simple time series, not for a panel of 22
countries. When applied to our stacked dataset it is technically invalid
and the results should be treated with caution.

**How we address it**
We acknowledge this limitation explicitly and use a supplementary test —
comparing how well models with different historical period structures fit
the data using AIC and BIC information criteria — as the primary
evidence for the regime structure. The formal break tests are presented
as supporting evidence only, not as the main proof.

---

## 4. The neighbour effect might not be real

**The problem**
We find that countries tend to spend similarly to their geographic
neighbours. This looks like peer pressure or burden-sharing dynamics.
But it could simply be that defence budgets change very slowly and
neighbouring countries happen to share similar historical baselines.
If Germany and France both inherited similar spending levels from the
Cold War and both change slowly, they will always look similar even if
neither is watching the other.

**How we address it**
We directly test this alternative explanation in M12 by adding each
country's own previous year's spending to the model. When we do this,
the neighbour effect largely disappears. This confirms that what looked
like peer influence is mostly shared inertia. We present this as a
positive finding — a correction to papers in the literature that have
over-interpreted spatial correlation as strategic interaction.

---

## 5. The spatial model finds no spatial autocorrelation to explain

**The problem**
Before running the spatial models we test whether the data actually
shows any spatial pattern in the first place. The test — Moran's I —
finds no significant positive clustering in any year. In fact it finds
a slight negative pattern, meaning high-spending countries tend to be
surrounded by low-spending neighbours. This raises an uncomfortable
question: if there is no spatial clustering, why are we running a
spatial model?

**How we address it**
The negative pattern is itself theoretically meaningful — it is
consistent with free-riding, where countries that border high-spending
allies feel they can spend less. We reframe the spatial model not as
evidence of contagion but as a test of whether peer effects exist at
all. The spatial model is also the methodologically appropriate
framework given that our observations are geographically structured
and ignoring that structure entirely would be the wrong choice.

---

## 6. Great Britain is excluded

**The problem**
Britain is a major NATO member, consistently spends above 2% of GDP
on defence, and is one of the most strategically significant countries
in the sample. Excluding it makes the results less representative.

**How we address it**
The exclusion is justified on clear grounds: Britain is an island, our
threat measure is based on land-border proximity, and Britain's defence
spending reflects global commitments — nuclear deterrence, expeditionary
forces, special relationship with the US — that have nothing to do with
conflict near the EU land border. Including Britain would not test our
theory, it would contaminate it. We document this formally and run a
robustness check showing that including Britain produces unstable results
that confirm it behaves as a structural outlier.

---

## 7. The threat measure covers only state-based conflict

**The problem**
Our threat score counts only wars and military clashes involving
government armies. It excludes terrorism, organised crime, and
non-state violence. Yet terrorism — Paris 2015, Brussels 2016 — clearly
affected European security perceptions and may have influenced defence
budgets. By excluding it we may be missing part of the picture.

**How we address it**
The exclusion is a deliberate theoretical choice: NATO defence spending
is designed to deter and respond to state-level military threats, not
terrorism, which falls under police and intelligence budgets. Our
comparison with the GPR index in M13 partially addresses this — GPR
captures media salience of all geopolitical threats including terrorism.
If M13 performed much better than our measure it would suggest we are
missing something important. In the results it does not.

---

## 8. GPR country coverage is incomplete for Eastern Europe

**The problem**
When we compare our threat index to the Geopolitical Risk index in M13,
we can only do so for 14 of our 22 countries because the GPR index does
not cover most Eastern European countries — Bulgaria, Estonia, Latvia,
Lithuania, Romania, Croatia, Czech Republic, Slovakia, Slovenia are all
missing. These are precisely the countries most exposed to the Russian
threat, where the comparison would be most informative.

**How we address it**
We are transparent about this gap and note it as a structural limitation
of the comparison. The 14 countries we do have are still sufficient to
test the core question. We note that the missing countries are likely to
strengthen our finding — the GPR index would probably perform even worse
relative to our physical measure for frontline Eastern European states
whose threat environment is not well captured by English-language
newspaper coverage.

---

## 9. The threat index only counts dead bodies — it misses the 2014 Crimea shock and modern hybrid warfare

**The problem**
Our threat score is built by adding up fatalities from armed conflict events.
This means it only goes up when people are actually killed. That sounds
reasonable, but it creates a serious blind spot: some of the biggest
geopolitical shocks in our time period produced very few deaths on the day
they happened.

The clearest example is Russia's annexation of Crimea in March 2014. This
single event triggered NATO's Wales Summit, the 2% GDP pledge, and a
decade of Eastern European rearmament. Yet because the annexation was
largely bloodless — Russian troops moved in without a firefight — our
threat index barely registers 2014 as different from 2013. Countries
like Estonia, Latvia, Poland and Lithuania responded to what they correctly
read as an existential signal, but our index says "only a small threat
that year."

The same problem applies to other modern threats our index cannot see
at all: Russia's cyberattack on Estonia's entire internet infrastructure
in 2007; Russian hybrid warfare in Donbas from 2014–2021 (low casualty
count despite ongoing occupation); large-scale Russian military build-ups
on NATO's eastern border in 2021; and disinformation campaigns targeting
European elections. None of these register in our fatality-based measure.

**How we address it**
We are honest about this in the paper. Our index is designed to measure
*territorial military threat from active armed conflict* — not perceived
threat, not hybrid warfare, not political signals. The GPR index comparison
(M13) partially fills this gap: GPR captures media-salience of geopolitical
risk including non-kinetic events, and its pooled correlation with our
measure is only r=0.08, confirming they are measuring different things.

The key defence of our measure is theoretical: NATO conventional defence
spending — tanks, planes, troops — is specifically designed to deter and
respond to *military force*, not cyber operations or disinformation. The
2014 post-Crimea rearmament is actually captured in our data via its
*consequences*: regime 3 (2014–2021) shows higher base spending levels,
and the regime interaction coefficient in M4 recovers from the austerity
trough. What we cannot claim is that our index captures *why* 2014 was a
turning point — only that spending patterns changed after it.

For peer reviewers who raise this, the honest answer is: our index
measures the physical conflict environment that makes military spending
rational; the political signal of Crimea is part of what regime
periodisation captures, even if the continuous threat score misses
the bloodless annexation itself.

---

## 10. The 50km sea rule ignores naval and air threats for southern European countries

**The problem**
We filter out conflicts that require crossing more than 50km of sea to reach
European land. This works well for most of the continent — it correctly
excludes Syria, Libya, and Iraq from threatening Germany or Poland. But it
creates a real problem for the southern edge of Europe.

Greece and Turkey have been in a long-running dispute over the Aegean Sea,
with military incidents, airspace violations, and contested islands. Libya's
civil war — which collapsed into a proxy war involving Russian Wagner Group
forces — sits just 300km from Italy and 350km from Greece. For these
countries, genuine defence drivers come from the sea and air, not from land
borders. Our index assigns near-zero threat to Greece from the south and
west, yet Greece consistently spends the most on defence in the EU (above
3% of GDP for much of the period) precisely because of these maritime
tensions.

The result is that our model systematically under-explains Greek and
southern European defence spending, and the country fixed effects absorb
the difference rather than the threat variable explaining it.

**How we address it**
This is a genuine limitation we acknowledge explicitly. Our research
question is about *continental land-border threat* and its rationality —
and for 19 of our 22 countries this is the right threat concept. Greece
(and to a lesser extent Italy and Spain) are partial exceptions where the
threat environment includes a meaningful naval dimension our index ignores.

We note this in the methodology and treat Greek spending levels with
additional caution in interpretation. The country fixed effect for Greece
is the largest in the sample (+1.84 in M5), absorbing the structural
overspend relative to the model's threat prediction — exactly what a
well-specified fixed effect should do when a country has a systematically
different threat environment. This is not a flaw in the model but a
correctly identified residual that future work could address by extending
the threat measure to include naval incidents and airspace violations
(e.g., using ICPS maritime security data or ACLED).

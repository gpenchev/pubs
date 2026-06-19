# Talking Points — Slide by Slide
## Threat Proximity and Defence Spending in NATO-EU Member States, 1995–2023
### 15-minute conference presentation

---

> **How to use this file**
> Each section gives you the exact spoken sequence for that slide.
> — *Italics* = what to say verbatim or near-verbatim
> — Plain text = what to convey in your own words
> — [cue] = action or emphasis marker
> Do not read from this. Internalise the sequence. Each slide has a single core sentence — say that first, everything else supports it.

---

## SLIDE 1 — Title
**Target time: 0:30**

*"The question driving this paper is simple: do European governments actually respond to nearby military conflict when setting their defence budgets — and if not, why not?"*

Pause. Let it land. That is the only sentence you need here. Do not explain the dataset, do not mention UCDP, do not say "NATO burden-sharing." Just the question.

---

## SLIDE 2 — The Problem: Why a New Threat Measure?
**Target time: 1:30** *(1 minute on this slide)*

Open with the provocation:
*"The NATO 2% target treats Poland and Portugal as equally threatened. That is geographically absurd."*

Then move through the bullets quickly — one breath each:
- CINC measures what a country has, not what it faces.
- GPR measures what newspapers worry about — Iraq 2003, terrorism, trade wars — none of which put a tank on Poland's border.
- GDP ratios are circular: you cannot use defence-as-share-of-GDP as both your outcome and your threat proxy.

Close the slide:
*"There is no existing measure that asks: how close is an actual war to this country's actual border, right now? That is what we build."*

---

## SLIDE 3 — A Novel Georeferenced Territorial Threat Measure
**Target time: 3:30** *(2 minutes — the most time-intensive slide)*

Point at the formula briefly, then immediately move to the map:
*"Three design choices — I'll explain them through the maps."*

[Point to left panel — Balkans]
*"In 1995, the threat mass is here — Bosnia, Croatia, Kosovo. Countries like Romania and Hungary are on the doorstep. Germany and France are not."*

[Point to right panel — Ukraine]
*"By 2022 it shifts entirely east. Poland, Lithuania, Latvia are facing this at full weight. Spain and Portugal are at near-zero."*

Now explain the three choices naturally from the map:
- *"We weight by fatalities, not incidents — a single major battle matters more than a hundred skirmishes."*
- *"We decay by distance — 50km from your border is a different defence-planning problem from 450km."*
- *"And critically — we only count land-reachable conflicts. The UK has no land border to any conflict zone. That is why it is excluded from the regressions."*

Point at the caption:
*"That cross-national variation — Poland facing a hundred times more threat than Spain — is exactly what allows us to identify the model."*

---

## SLIDE 4 — Data and Sample
**Target time: 4:15** *(45 seconds — fast)*

*"Twenty-two NATO-EU states, 1998 to 2023, 529 observations."*

Scan the table quickly — one sentence for each distinctive row:
- Defence spending from WDI and SIPRI — standard source.
- Our threat index from UCDP GED — the novel variable.
- Fiscal deficit and debt — the constraint channel.
- ParlGov ideology scores are seat-weighted cabinet averages — they reflect coalition balance, not just the prime minister's party.

Then the exclusions:
*"Great Britain is excluded. Its threat score sits 39% below the sample mean while its defence spending is 47% above it — the exact opposite of what the theory predicts. That is not noise, that is structural misfit from island geography."*

*"Luxembourg is a micro-state outlier. Both drop out of primary regressions."*

---

## SLIDE 5 — Three Identification Layers
**Target time: 5:30** *(1 minute 15 seconds)*

Frame it first:
*"We run thirteen models. Not for robustness — each one answers a different question."*

Layer 1 — fast:
*"The first question is just: does threat predict spending at all? Two-way fixed effects strips out everything permanently different about each country, and everything that hit all countries in the same year. What's left is purely within-country, within-year variation."*

Layer 2 — fast:
*"The second question: is the cross-country correlation we see genuine strategic interaction, or just shared history? The spatial autoregressive model tests that directly."*

[Slow down here, slight emphasis on ③]
*"The third question is the one that matters most — which is why it has a star on this slide. Has the relationship between threat and spending actually changed over time? The structural break analysis says yes, dramatically, and that finding is the core of this paper."*

---

## SLIDE 6 — Threat Significant in 11 of 12 Specifications
**Target time: 7:00** *(1 minute 30 seconds)*

[Point at the forest plot]
*"Every single green dot is to the right of zero. Eleven of twelve specifications, positive and significant."*

*"The range — 0.05 to 0.11 — is not fragility. It is identification strategy. M1 captures cross-country level differences. M12, the most conservative, controls for the fact that last year's budget mostly just carries forward into this year's budget. Even after accounting for that inertia, threat still predicts the residual change."*

Point at M5 highlighted row:
*"Our primary spatial model gives β = 0.088. A one-log-unit increase in threat proximity is associated with a 0.088 percentage point increase in defence spending as a share of GDP. That is a tight, stable estimate."*

Point at the grey M10b:
*"The one grey point — post-2014 subsample — is not a failure. The 2022 Ukraine invasion hit every single country simultaneously. Year fixed effects absorbed that common shock completely. Later I will show you a cross-section of 2022 alone that gives β = +0.381 with R² of 0.56. The within-year gradient is real. The estimator just could not see it."*

Two additional findings from M5 to mention verbally:
*"Fiscal deficit is −0.023 in every specification — governments under fiscal stress cut defence even when facing real threat. And the spatial lag ρ = +0.177 — neighbouring countries' spending levels predict your own, consistent with NATO burden-sharing norms."*

---

## SLIDE 7 — Fiscal Austerity Broke Rational Response for a Decade
**Target time: 9:00** *(2 minutes — the centrepiece)*

[Pause before speaking. Point at the figure.]
*"This is the central finding of the paper."*

*"In 1995 to 2004 — the Balkans wars — governments responded normally. When threat went up, spending went up. Net elasticity of plus 0.10."*

[Point at R2 — red, below zero]
*"In 2005 to 2013 — the austerity decade — that relationship inverted. Countries facing rising threat actually cut defence spending. The net elasticity is minus 0.156. Fiscal consolidation completely overrode the strategic imperative."*

*"This is not irrationality. Latvia and Estonia in 2010 knew exactly where Russia was. They did not have the fiscal room to respond. That is the finding."*

[Point at R3]
*"Crimea in 2014 began a recovery, but governments were still running post-crisis budgets. The elasticity is still negative at minus 0.067."*

[Point at R4 — blue, above zero]
*"2022: the right direction finally, plus 0.151. But we only have two years of data. N equals 44. Statistical power is 28%. I will not overclaim this.*"

*"What I will say is this: the within-year cross-section for 2022 alone — no fixed effects, just 22 countries — gives β = +0.381 and R-squared of 0.56. Countries closest to Ukraine spent the most. That is about as clear as threat-responsive spending gets."*

Close firmly:
*"The policy implication is direct. Demanding that all NATO members hit 2% during a fiscal crisis is not a political problem — it is a structural impossibility. You cannot spend money you do not have, even when the threat is real."*

---

## SLIDE 8 — Spatial Structure: Alliance Coordination, Not Contagion
**Target time: 9:45** *(45 seconds — efficient)*

*"The positive spatial lag — ρ = +0.177 — looks like neighbours copying each other. But when we decompose it, it is not that simple."*

Scan the table quickly:
*"After controlling for spending persistence, ρ drops to 0.06 and loses significance. In first differences it actually goes negative — minus 0.09. Countries that surge in a given year are surrounded by neighbours that hold back slightly. Burden-sharing substitution in the short run."*

*"Pre-2014: no spatial effect at all. Countries responded independently to their own threat environments.*"

*"The interpretation: countries match spending levels because of the 2% target creating a shared anchor. They do not copy annual changes. The coordination is norm-driven and shock-driven — not a durable strategic diffusion mechanism."*

---

## SLIDE 9 — UCDP Outperforms GPR (ΔAIC = 17.6)
**Target time: 10:45** *(1 minute)*

Lead with the mechanism, not the horse race:
*"The most theoretically important result on this slide is not the AIC number — it is that the spatial lag reverses sign. UCDP gives ρ = +0.177. GPR gives ρ = −0.210. Physical threat generates strategic complementarity — countries near the same conflict both feel it and both respond. Media-perceived risk generates free-riding substitution — if your neighbour is in the news for being threatened and arms up, you relax slightly."*

[Point at 2014 annotation]
*"The kinetic bias is real and we acknowledge it. Crimea was a politically massive event that generated almost no fatalities near EU land borders. GPR captured it; UCDP barely moved. That is a genuine limitation of a fatality-based measure."*

*"Despite that limitation, UCDP fits the defence spending data better than GPR by 17.6 AIC units on the same 13 countries. And critically, the 10 Eastern European countries missing from GPR coverage are exactly the highest-threat states. The UCDP advantage is a conservative lower bound."*

---

## SLIDE 10 — Primary Results Stable Across 10 Sensitivity Checks
**Target time: 11:30** *(45 seconds — one sentence each)*

*"Five of ten checks shown here."*

- **Check C:** *"VIF of 13 on threat and debt looks alarming. Mathematically separating them changes the threat coefficient by exactly zero percent. The flag is benign."*
- **Check F:** *"Removing immigration from the model flips the sign of the EU-position and left-right ideology coefficients. That tells us political ideology effects on defence are conditioned on immigration context — they cannot be estimated independently. Immigration belongs in the model."*
- **Check H:** *"Bulgaria 2019 is the single most influential observation — F-16 procurement recorded as a one-year budget item. Removing it changes every key coefficient by less than one standard error."*
- **Check I:** *"Already mentioned — β = +0.381 in 2022 alone."*
- **Check J:** *"No distinct mechanism from the post-2022 refugee inflow. The immigration coefficient is stable across all subperiods."*

---

## SLIDE 11 — Pro-EU Governments Reversed Direction After 2014
**Target time: 12:15** *(45 seconds)*

*"One more finding worth its own slide."*

*"Before 2014, pro-EU governments spent marginally more on defence — EU membership was associated with security cooperation commitments."*

*"After 2014, the relationship reversed sharply. Pro-EU governments restrained defence spending. Eurosceptic nationalist parties — PiS in Poland, Fidesz in Hungary, the Baltic nationalist coalitions — became the strongest advocates for national military build-up."*

*"Left-right ideology has no significant effect in any within-country model. The relevant political cleavage for defence spending is no longer left versus right. It is national sovereignty versus European integration."*

---

## SLIDE 12 — European Governments Are Conditionally Rational
**Target time: 13:45** *(1 minute 30 seconds — land the argument)*

State the headline plainly:
*"European governments are conditionally rational on defence. The condition is fiscal space."*

Walk the table briefly:
*"Threat drives spending — β = +0.088, stable across all specifications. Fiscal deficit suppresses it — minus 0.023 in every model. When budgets are constrained, even genuine threats go underfunded. That is what the austerity decade was."*

*"The rationality has returned since 2022. But two years is not a trend."*

*"The EU position reversal — p = 0.008 — tells us something important about the politics of European security. The traditional assumption that pro-European governments are more security-cooperative no longer holds after 2014."*

Close with the policy statement — say it slowly:
*"If the lesson policymakers take from 2005 to 2013 is that some European countries were unwilling to spend on defence — they are drawing the wrong conclusion. The data says those governments were fiscally unable to respond. The policy prescription is not more pressure on spending targets. It is creating the fiscal space that makes rational response possible."*

---

## SLIDE 13 — Three Disclosed Limitations
**Target time: 14:15** *(30 seconds — brisk, no apology)*

*"Three limitations, stated directly."*

*"First: kinetic bias. Our measure counts fatalities. Crimea 2014 was a coercive annexation with almost no UCDP-coded deaths. We miss it. The GPR comparison demonstrates this explicitly, and UCDP still wins on model fit."*

*"Second: the 50-kilometre sea threshold excludes Aegean naval threats. Greece spends more than anyone in our sample and has near-average UCDP land threat. Country fixed effects partially absorb this, but Greece is a known structural fit problem."*

*"Third: Regime 4 has 44 observations and 28% statistical power. The direction is right. The structural break test confirms the regime exists. We are honest about what two years of data can and cannot prove."*

---

## SLIDE 14 — Open Science Supplement
**Target time: 15:00** *(45 seconds)*

*"Everything is public — data, code, and models, from raw UCDP and Eurostat downloads to the tables in the paper, in a single reproducible pipeline."*

[Point at the app bullet points]
*"The interactive application is designed specifically for moments like this one. If you want to challenge a specific country's profile, a specific model, or a specific robustness check during Q&A — it is already there. The Specific Issues tab has pre-prepared answers to the four main methodological questions this paper faces."*

*"Thank you."*

Pause. Do not say "any questions." Just pause and make eye contact. Let the chair open Q&A.

---

## Q&A QUICK-REFERENCE

| If asked about... | Go to backup slide | First sentence |
|---|---|---|
| "Why is threat not significant post-2014?" | **B2 — Check I scatter** | *"Year FE absorbs a common shock. Here is 2022 alone, no FE — β = +0.381, R² = 0.56."* |
| "What about Bulgaria 2019?" | **B3 — Bulgaria series** | *"F-16 procurement contract, single-year recording. Removing it changes nothing by more than one standard error."* |
| "Can I see the full M5 table?" | **B1 — M5 coefficients** | *"Here it is. Threat, deficit, immigration, EU position all significant. Debt and left-right are not."* |
| "Why not use Manifesto instead of ParlGov?" | No backup needed | *"ParlGov gives us seat-weighted coalition scores back to 1995. Manifesto has coverage gaps for Eastern EU early years that would lose the most threat-relevant country-years."* |
| "Have you controlled for GDP levels?" | No backup needed | *"GDP growth is in every model. GDP per capita was excluded after VIF analysis showed multicollinearity with debt — and its coefficient was never significant. Deficit and debt together capture the fiscal channel more cleanly."* |
| "What about endogeneity — could spending affect threat?" | No backup needed | *"UCDP conflict events are georeferenced fatalities from ongoing wars, not proximate to any single country's budget decision. The identification assumption is that Poland's defence budget does not cause wars in Ukraine. That is defensible."* |
| "Why exclude Great Britain?" | No backup needed | *"Island geography creates a structural misfit — threat 39% below mean, spending 47% above. The exclusion is theoretically motivated and confirmed by the divergence statistics. We run Check F including GB — results are qualitatively identical."* |

---

## Timing checkpoints

| Slide | Cumulative target | If running over |
|---|---|---|
| S1 Title | 0:30 | — |
| S2 Problem | 1:30 | Cut the proxy critique, keep just the gap sentence |
| S3 Threat measure | 3:30 | Cut decay explanation, just point at the maps |
| S4 Data | 4:15 | Skip ideology variables, keep threat + exclusions |
| S5 Strategy | 5:30 | Cut Layers 1 and 2 to one sentence each |
| S6 Forest plot | 7:00 | Skip the M10b explanation — mention in passing |
| S7 Regime ⭐ | 9:00 | **Do not cut this slide** |
| S8 Spatial | 9:45 | Cut to one sentence: "Levels complementarity, not changes contagion" |
| S9 GPR | 10:45 | Lead with sign reversal only, skip AIC |
| S10 Robustness | 11:30 | Keep Check I only, skip the rest |
| S11 EU position | 12:15 | Can cut entirely if over by 90 seconds |
| S12 Conclusions ⭐ | 13:45 | **Do not cut this slide** |
| S13 Limitations | 14:15 | Keep all three — brevity is the point |
| S14 Closing | 15:00 | — |

# What the models are actually doing — a plain explanation

This document explains all models M1 through M13 without statistics.
The goal is to understand what question each model is asking, what
problem it is solving, and what it adds over the previous one.

---

## The basic question we are trying to answer

We want to know: **do countries spend more on defence when there is
more conflict nearby?**

That sounds simple. But when you look at the data, many things are
happening at the same time. Some countries are rich and spend more on
everything, not just defence. Some years everyone cuts spending because
of an economic crisis. Some countries have always spent a lot on defence
regardless of nearby conflict, because of their history or their
politics. If we just look at the raw numbers, all of these things get
mixed together and we cannot tell what is actually driving what.

The models are a sequence of steps that progressively strip away
confounding factors, introduce spatial structure, and stress-test the
findings, until we can say with confidence what is actually driving
defence spending and what is not.

---

## M1 — Just look at the data as it is

### What we do

We take all 530 country-year observations and fit a single straight line
through them. We ask: across all countries and all years, when threat
goes up, does spending go up too? We include a set of other variables —
debt, deficit, economic growth, immigration, government ideology — to
account for obvious alternative explanations.

### What problem this has

Imagine two countries: Estonia and Spain. Estonia is small, close to
Russia, and spends a high share of GDP on defence. Spain is large, far
from any active conflict, and spends relatively little. In the raw data,
Estonia has a higher threat score and higher spending. Spain has a lower
threat score and lower spending. So the model says: yes, threat predicts
spending.

But is that really threat driving spending? Or is it just that Estonia
has always been a high-spending country because of its history with the
Soviet Union, completely independent of what is happening in any given
year? We cannot tell from M1, because we are mixing together differences
**between** countries with changes **within** countries over time. The
model is partly picking up the fact that some countries are permanently
different from others, not that threat is causing spending to change.

### What M1 tells us

A rough, uncontrolled first look. It says: in the data overall, these
variables move together. It does not say why.

---

## M2 — Each country compared only to itself

### What we do

We give every country its own baseline level. Instead of asking "do
high-threat countries spend more than low-threat countries?", we now ask
"when a country's threat goes up compared to its own normal level, does
its spending also go up compared to its own normal level?"

Technically, we add a separate intercept for each country. This absorbs
everything that is permanently different about a country — its history,
its geography, its strategic culture, its institutional setup — and
focuses the analysis entirely on changes over time within each country.

### What this solves

The Estonia-Spain comparison disappears. We are no longer asking whether
Estonia spends more than Spain. We are asking: in the years when Estonia
faces more conflict nearby than usual, does Estonia spend more than
usual? And separately: in the years when Spain faces more conflict nearby
than usual, does Spain spend more than usual?

This is a much cleaner question. The permanent differences between
countries — which could be driven by anything — are no longer
contaminating the answer.

### What M2 tells us

After removing everything that makes countries permanently different from
each other, threat proximity still has a positive relationship with
defence spending. The effect is now a genuine within-country signal: when
conflict gets closer, governments spend more.

---

## M3 — Each country compared to itself, and each year compared to itself

### What we do

We add one more layer of control. In addition to each country having its
own baseline, we now give every year its own baseline too. This absorbs
everything that happened to all countries at the same time in a given
year — regardless of what it was.

### What problem this solves

Think about 2010. Almost every country in the sample was cutting defence
spending that year because of the financial crisis that started in 2008.
This had nothing to do with nearby conflict — it was a universal
budgetary squeeze driven by fiscal emergency. In M2, this would show up
as a negative relationship between spending and everything, because
spending was falling everywhere regardless of threat levels.

By adding year effects, we say: whatever happened to all countries in
2010, we will absorb that entirely. We only look at what made one country
spend more or less than other countries in that same year, or
equivalently, what made a country's spending deviate from the common
trend in that year.

This is important because threat proximity is not the same for all
countries in a given year. In 2022 Poland was much more exposed to the
Ukraine war than Portugal. After controlling for the common 2022 trend
— which includes the general global pressure to rearm — we can ask
whether Poland's extra exposure translated into extra spending above and
beyond that common trend.

### What M3 tells us

This is the cleanest non-spatial result. After removing permanent country
differences and common year shocks, threat proximity remains a
significant positive predictor of defence spending. Equally important,
the fiscal deficit emerges as a strong negative predictor: governments
that are already running large deficits spend less on defence, regardless
of how much threat they face. This is the fiscal dominance finding —
capacity to spend matters as much as the reason to spend.

---

## The progression in plain terms

| Model | What we control for | What question we answer |
|---|---|---|
| M1 | Nothing about country differences or year effects | Do high-threat countries spend more overall? |
| M2 | Permanent differences between countries | When a country faces more threat than usual, does it spend more than usual? |
| M3 | Permanent differences between countries AND common shocks in each year | When a country faces more threat than its peers in a given year, does it spend more than its peers that year? |

Each step narrows the question. M3 is the most honest non-spatial version
of the question. The answer across all three is yes — threat proximity
predicts spending — but M3 gives us the most credible evidence for it,
because it has removed the most obvious alternative explanations.

---

## M4 — Does the threat effect change depending on the historical period?

### What we do

We split the data into four historical periods — the post-Cold War 1990s,
the pre-crisis 2000s, the post-Crimea rearmament years, and the
post-Ukraine surge — and ask whether the relationship between threat and
spending is the same in all of them. Everything else stays the same as M3:
we still control for country differences, year shocks, fiscal variables,
and political factors.

### What problem this solves

It is entirely plausible that the threat-spending relationship was not
constant over thirty years. In the 1990s, European governments were
enjoying a peace dividend and cutting defence regardless of nearby
conflicts. After 2022, the same governments were scrambling to rearm.
A model that forces a single average relationship across all periods
would mask this variation and produce a misleading middle ground that
does not accurately describe any particular period.

### What M4 tells us

Whether the effect of threat proximity on spending is higher or lower in
some periods than others. If the interaction terms are significant, it
means the threat-spending relationship is not stable — it switches on and
off depending on the geopolitical context. This is a crucial finding
because it means the standard assumption in the defence spending
literature — that threat always drives spending in the same way — is wrong.

---

## M5 — Do countries also respond to what their neighbours are spending?

### What we do

We take the cleanest model so far (M3) and add one new ingredient: each
country's spending is now allowed to be influenced by the average spending
of its geographic neighbours. We are asking whether, after controlling for
everything else, there is a contagion effect — if Germany increases defence
spending, do France and Poland tend to increase theirs too?

To measure this, we build a map of which countries share borders, and for
each country we compute the weighted average of its neighbours' spending.
That average enters the model as an additional explanatory variable.

### What problem this solves

Alliance politics and burden-sharing pressure are real. Countries do look
at what their partners are spending, partly because of diplomatic pressure
(the NATO 2% target) and partly because defence spending involves
collective goods — if one country invests heavily in deterrence, its
neighbours may feel they can spend a little less. M3 ignored this
completely. M5 captures it.

### What M5 tells us

Whether there is genuine peer influence in defence spending, and how
large it is. If the neighbour effect is significant, it means defence
budgets are not set in isolation — they respond to the alliance context.
If it disappears in later models, it means what looks like peer influence
is actually something else.

---

## M6 — What if the similarity between neighbours is not about copying, but about shared circumstances?

### What we do

We test an alternative explanation for why neighbouring countries tend to
spend similar amounts. Instead of saying "Poland responds to Germany's
spending", M6 says "Poland and Germany tend to spend similar amounts
because they share unobserved factors we have not measured — perhaps
similar political traditions, similar economic structures, or similar
threat perceptions that our threat index does not fully capture."

In M6 the similarity between neighbours operates through the error terms
rather than directly through spending levels. We compare M5 and M6
statistically to see which story fits the data better.

### What problem this solves

It is important to know whether countries actively watch and react to
each other (M5 story) or whether they just happen to be similar because
of shared background factors (M6 story). The policy implications are
very different. If M5 is correct, diplomatic pressure and burden-sharing
negotiations can actually change spending. If M6 is correct, the
similarity is mostly coincidental and structural.

### What M6 tells us

In the results, M5 fits the data better than M6. This means the spatial
pattern in defence spending is better described as countries reacting to
each other's spending levels than as countries merely sharing unobserved
background similarities. However, M12 later shows this reaction is more
about inertia than genuine strategic interaction.

---

## M7 — Does the neighbour effect also change across historical periods?

### What we do

We combine the two previous additions: the neighbour effect from M5 and
the historical period interactions from M4. This is the most complete
model in the sequence. We are asking simultaneously: does threat affect
spending, does it affect it differently in different periods, and do
countries respond to their neighbours — all at the same time?

### What problem this solves

It is possible that both things changed after 2014 or 2022: not just
the threat-spending relationship, but also the degree to which countries
copy each other. Before 2014, European defence spending was on
autopilot and peer effects were weak. After 2022, the political pressure
to reach the NATO 2% target made burden-sharing dynamics much more
active.

### What M7 tells us

The marginal effect of threat proximity on defence spending across
the four historical regimes, in the presence of the neighbour effect.
The headline finding from this model is that the threat effect is only
statistically detectable in the post-2022 period — it was not significant
in the earlier three periods, even controlling for spatial spillovers.

---

## M8 — What if we measure threat more broadly, including sea-separated conflicts?

### What we do

Everything stays the same as M5 — same country controls, same year
controls, same neighbour effect — but we swap out the threat measure.
Instead of using only conflicts reachable by land, we use a threat score
that includes all nearby conflicts regardless of whether a sea crossing
separates them from EU territory. This means North African conflicts
(Libya, Algeria), Middle Eastern conflicts (Syria), and Sahel instability
now enter the threat score for southern European countries like Italy,
Spain, and Greece.

### What problem this solves

Our primary threat measure deliberately excludes sea-separated conflicts
on the argument that they do not pose a direct military threat requiring
NATO-style territorial defence. But these conflicts do generate pressure
on defence budgets through other channels — they create refugee flows
that drive border security spending, they disrupt energy supplies that
require strategic reserves, and they generate humanitarian intervention
missions that are classified under defence expenditure. The question is
whether these indirect channels are real enough to show up in the data.

### What M8 tells us

If M8 fits the data better than M5, it means the indirect threat
channels are real and that southern EU members are responding to
a broader threat environment than the land-contiguity filter captures.
In the results, M8 does fit better on standard statistical criteria,
which is an important finding about how different European countries
perceive and respond to conflict in their neighbourhood.

---

## M9 — Does it matter how we define "neighbour"?

### What we do

We change the definition of neighbourhood. In M5, two countries are
neighbours if they share a border — France and Germany are neighbours,
France and Poland are not. In M9, every country is a neighbour of every
other country, but countries that are closer receive more weight. So
France and Poland are now in each other's neighbourhoods, but Poland
counts for less than Germany when calculating France's peer spending.
Everything else stays the same as M5.

### What problem this solves

Defining neighbours as only those sharing a border is simple and
transparent but also crude. Poland and the Baltic states have no shared
border but are deeply interconnected through NATO's eastern flank
dynamics. The inverse-distance approach captures a more continuous notion
of proximity — the idea that influence fades with distance rather than
dropping to zero at a border.

### What M9 tells us

If the results are similar to M5, the findings are robust — they do not
depend on whether we define neighbours by shared border or by distance.
If they differ substantially, we need to think harder about what kind
of spatial relationship is actually driving the data.

---

## M10a — What if we remove Finland from the analysis?

### What we do

We run the same model as M5 but drop Finland entirely from the sample.

### Why Finland specifically?

Finland is a special case in our spatial setup. It has no land border
with any other country in our sample — it borders Russia, which is not
in the sample, and the sea separates it from Estonia and Sweden. Because
our neighbourhood map requires at least one neighbour, we had to assign
Finland's neighbours artificially based on distance rather than shared
border. This is a workaround, not a proper geographical relationship,
and it could be distorting the results in ways that are hard to detect.

### What M10a tells us

If the results barely change when Finland is removed, we can be confident
that our workaround for Finland is not causing problems. If the results
change substantially, it would be a warning sign that the spatial
structure is sensitive to how we handle edge cases. In practice, removing
Finland makes very little difference.

---

## M10b and M10c — What if we look at only part of the time period?

### What we do

We split the sample in two at the year 2014 — the year Russia annexed
Crimea — and run the primary model separately on each half. M10b covers
2014 to 2023. M10c covers 1995 to 2013.

### What problem this solves

The Crimea annexation in 2014 is widely considered a turning point in
European security. Before 2014, most Western European governments
treated military conflict in Europe as a historical curiosity. After
2014, the threat became tangible. If this shift was real, we would expect
the relationship between threat and spending to look different in the two
periods — perhaps stronger after 2014, perhaps with different countries
driving the effect.

### What M10b and M10c tell us

Comparing the two models tells us whether the threat-spending
relationship and the neighbour effect changed after 2014. We also use
these two models to test a specific political finding: before 2014,
pro-European governments spent slightly more on defence; after 2014,
the relationship reversed and Eurosceptic governments in Eastern Europe
became the bigger spenders, driven by their proximity to the Russian
threat. This sign reversal is one of the more surprising political
findings in the paper.

---

## M12 — Is the neighbour effect real, or just a coincidence of slow-moving budgets?

### What we do

We go back to the primary model (M5) and add one more variable: each
country's own defence spending in the previous year. We are now asking
whether the neighbour effect survives once we account for the fact that
defence budgets barely change from year to year.

### What problem this solves

Here is the key suspicion: defence budgets are sticky. France spent
roughly 1.9% of GDP on defence in 2018, 2019, and 2020 — not because
the threat was identical in all three years, but because large government
budgets change slowly due to procurement cycles, institutional inertia,
and political constraints. Germany is France's neighbour and also has a
sticky budget. So France and Germany will always look similar — not
because France is watching Germany, but because both are independently
stuck near their own historical baselines.

If this inertia story is correct, the neighbour effect we found in M5
is not real. It is a statistical coincidence caused by neighbouring
countries having similarly slow-moving budgets.

### What M12 tells us

When we add last year's spending, the neighbour effect largely disappears.
This is the persistence versus diffusion finding: what looked like
countries influencing each other is actually countries independently
maintaining their own historical spending levels. They look similar not
because of strategic interaction but because neither is changing much.
This is an important corrective to papers that have interpreted spatial
correlation in defence spending as evidence of arms race dynamics or
burden-sharing contagion.

---

## M13 — Does our threat measure work better than an alternative?

### What we do

We run the same model as M5 but replace our threat measure — the
physically-constructed score based on conflict events near each country's
border — with a completely different kind of threat measure: the
Caldara-Iacoviello Geopolitical Risk index. This alternative index is
built from newspaper coverage. It measures how much prominent
English-language newspapers are writing about geopolitical risks
involving a given country. If newspapers are writing a lot about threats
involving Poland, Poland's score goes up.

We then compare how well the two models fit the data.

### What problem this solves

Our threat measure is based on actual physical events — real conflicts,
real fatalities, real distances. The GPR index is based on perception
and media attention. These are fundamentally different things and they
can diverge sharply. In 2015, Paris and Brussels experienced major
terrorist attacks. GPR scores for France and Belgium spiked because
newspapers were full of coverage. But our conflict-proximity measure
barely moved because the attacks were not part of a military conflict
near any EU land border. Which measure better predicts whether
governments actually changed their defence budgets?

### What M13 tells us

It directly tests the core claim of the paper: that what drives defence
spending is physical proximity to military conflict, not media anxiety
about geopolitical risk. If our measure wins, it validates the entire
methodological approach — the land-contiguity filter, the border-polygon
distances, the spatial decay formula. If the GPR wins, it would suggest
that governments respond more to how conflict is covered in the press
than to how close it is to their borders.

---

## The full progression in plain terms

| Model | What is new | What question it answers |
|---|---|---|
| M1 | Nothing — raw data | Do high-threat countries spend more overall? |
| M2 | Each country has its own baseline | When a country faces more threat than its own normal, does it spend more? |
| M3 | Each year also has its own baseline | When a country faces more threat than its peers in a given year, does it spend more than its peers? |
| M4 | The threat effect can vary by historical period | Is the threat-spending relationship the same across all thirty years? |
| M5 | Countries may respond to neighbours' spending | Is there a contagion or peer-pressure effect in defence spending? |
| M6 | An alternative explanation for why neighbours look similar | Do countries copy each other, or are they just similarly circumstanced? |
| M7 | Period variation added back into the spatial model | Does the threat effect vary across periods even after accounting for peer effects? |
| M8 | Broader threat measure including sea-separated conflicts | Do indirect threats through migration and energy channels also drive spending? |
| M9 | Different definition of neighbourhood (distance rather than border) | Does the peer effect depend on how we define neighbours? |
| M10a | Finland removed | Is the spatial result sensitive to the treatment of edge cases? |
| M10b/c | Split at 2014 | Did the threat-spending and peer relationships change after Crimea? |
| M12 | Last year's spending added | Is the peer effect real, or just slow-moving budgets that look similar? |
| M13 | Media-based threat measure replaces physical measure | Does physical conflict proximity or newspaper coverage of geopolitical risk better predict defence spending? |


# Variables — a plain explanation

---

## The dependent variable — what we are trying to explain

**Defence spending as a share of GDP**
How much of its total economic output a country spends on its military
in a given year, expressed as a percentage. A country with a GDP of
100 billion euros spending 2 billion on defence has a score of 2%.
This is the standard international measure used by NATO and SIPRI and
allows fair comparison across countries of very different sizes.

---

## Independent variables — what we think might explain it

### The threat variable

**Threat proximity score (land-contiguous)**
Our custom-built measure of how much armed conflict is happening near
each country's land border in a given year, weighted by how deadly the
conflict is and how close it is. Higher means more nearby military
violence reachable by land. This is the central variable the paper is
built around. Described in full in `threat.md`.

**Threat proximity score (all conflicts)**
The same measure but without the sea-crossing filter — includes
conflicts in North Africa and the Middle East. Used only in M8 to test
whether the land filter was the right design choice.

---

### Fiscal variables — can the country afford to spend?

**Fiscal deficit**
Whether the government is spending more than it earns in a given year,
expressed as a percentage of GDP. A negative number means the government
is running a deficit. The expectation is that countries under severe
fiscal pressure spend less on defence — not because the threat is lower
but because they simply cannot afford it. This turns out to be one of
the strongest findings in the paper.

**Government debt**
The total stock of money the government owes, accumulated over all
previous years, expressed as a percentage of GDP. A country carrying
a very large debt burden has less room to increase spending on anything,
including defence.

**GDP growth**
How fast the economy is growing in a given year. A shrinking economy
tends to compress all government spending including defence. A growing
economy creates fiscal space.

**GDP per capita**
How wealthy the country is on average per person. Wealthier countries
can afford to spend more on defence in absolute terms, though the
relationship with defence as a share of GDP is less straightforward.

---

### Political variables — who is in government and what do they believe?

**Government left-right position**
Whether the governing coalition leans left or right on the standard
political scale, measured from party manifesto data. Right-leaning
governments are traditionally associated with higher defence spending
in the political science literature, though the paper finds this
relationship is weaker than expected.

**Government EU integration position**
How strongly the governing coalition supports European integration,
again measured from party data. This variable produces one of the
more surprising findings: before 2014 more pro-European governments
spent slightly more on defence, but after 2014 the relationship
reversed — Eurosceptic Eastern European governments became the
bigger spenders as they prioritised national defence over European
solidarity frameworks.

**Election year**
A simple yes/no flag for whether a parliamentary election took place
in that country in that year. Governments sometimes adjust spending
around elections. In the results this variable turns out not to matter.

---

### Migration variable — a proxy for indirect conflict pressure

**Immigration rate**
The number of people immigrating into the country per thousand
population per year. Conflicts near a country's neighbourhood tend to
generate refugee flows toward safer countries. Immigration pressure
may drive up border security and related spending that gets classified
under the defence budget. It also serves as an indirect signal that
nearby conflict is intense enough to displace populations. Great Britain
has no data for this variable throughout the study period, which is one
reason it is excluded from the primary models.

---

## The alternative threat variable used in M13

**Geopolitical Risk Index (GPR)**
A measure built by economists Caldara and Iacoviello that counts how
often major English-language newspapers mention geopolitical threats
involving a given country. It captures perceived and anticipated risk
rather than actual physical conflict events. Used only in M13 to
compare against our own threat measure and test which better predicts
defence spending.

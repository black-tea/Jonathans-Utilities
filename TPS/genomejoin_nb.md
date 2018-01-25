Genome Joining Applications in Transport
========================================

As part of the transit signal priority (TSP) evaluation application for
LADOT, I needed to be able to match two different data sets by time.
Specifically, I wanted to match LAFD EMS runs with sequences of TSP
activation along the target corridors.

In my initial build of the application, I was matching only one EMS
transponder to one specific TSP loop tag ID. Therefore, to make this
match, I only needed to look for overlapping time between the two
different “runs”; if the time windows between those two overlapped at
all, for any length of time, I considered it a match. I found athe
perfect R package, `fuzzyjoin,` which had a function called
`interval_join` designed to do just that.

However, as the evaluation expanded to multiple vehicles, each with a
separate corresponding TSP tag ID, I faced a dilemma. How can I match by
not only overlapping time, but also by an appropriate match ID? I didn’t
have to look far; `fuzzyjoin` also contains a function called
`genome_join`. Why genomes? Apparently “Genomic intervals include both a
chromosome ID and an interval: items are only considered matching if the
chromosome ID matches and the interval overlaps.”

This function ended up suiting my purposes perfectly. Instead of
matching by chromosome ID, I was using the matching tag ID for the TSP.
The only hitch was that I needed to first convert `POSIXct` values to
numeric values for the join (the underlying `findOverlaps` method within
the IRanges functionality requires numeric values) and then convert them
back to date/time values after the join.

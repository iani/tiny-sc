// Wed, Apr 16 2014, 06:15 EEST

Mdef(\x, (freq: [60, 64].midicps.pseq, dur: 1));
Mdef(\x).clone(\y);
Mdef(\x).monitor;
Mdef(\x).start;

Mdef(\x) => \test;

Mdef(\x).inspect;
//:
Mdef(\x).clone(\y);
// Mdef(\y).inspect;
Mdef(\y).monitor.start;

Mdef(\y).monitor;

Mdef(\x).patterns;
Mdef(\y).patterns;

Mdef(\y).set((freq: { ~freq * (5/4) }))

Mdef(\y) => \test2;
Mdef(\x).durations = 0.1;

Mdef(\y).durations = 2;


Mdef(\x).clone(\z);

Mdef(\z).monitor.start;

Mdef(\z).set([1, 2].pseq).monitor;

Mdef(\z).start;

//:

Mdef(\q, [freq: 500]);
Mdef(\q).monitor.start;
//Mdef(\q).inspect;
Mdef(\q).clone(\r);
Mdef(\r).monitor.start;

Mdef(\r).valueFilter = (freq: { | x | Pseq([1, 2], inf) * x });

//

Mdef(\x).durations = 3;

Mdef(\y) => \test;

PatternInstrument
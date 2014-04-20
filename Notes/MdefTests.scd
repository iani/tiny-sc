// Wed, Apr 16 2014, 06:15 EEST

Mdef(\x, (freq: [60, 64].midicps.pseq, dur: 1)) => \test;

Mdef(\x, (freq: [50, 54].midicps.pseq, dur: 1));

Mdef(\x).clone(\y);

Mdef(\y).monitor.start;

Mdef(\y).set((freq: { ~freq * (6/2) }))

Mdef(\y) => \test2;

Mdef(\y).set((dur: 1/3));

\test2.fadeOut;

\test2.start;

TempoClock.tempo = 2;

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
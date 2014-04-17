// Wed, Apr 16 2014, 06:15 EEST

Mdef(\x, [10, 20].pseq);
Mdef(\x).monitor;
Mdef(\x).start;
Mdef(\x).clone(\y);
// Mdef(\y).inspect;
Mdef(\y).start;

Mdef(\y).monitor;


Mdef(\y).valueFilter = { | p | p * 10 };

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
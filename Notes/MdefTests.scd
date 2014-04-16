// Wed, Apr 16 2014, 06:15 EEST

Mdef(\x, { 100.rand });

Mdef(\x).monitor;

Mdef(\x).start;

Mdef(\x).clone(\y);

Mdef(\x).clone(\z, [1, 2].pseq);
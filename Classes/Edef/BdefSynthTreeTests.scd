//:
Edef(\x, (degree: 20)) => \test;
//:
Edef(\x, (degree: Pwhite(10, 20, inf), dur: 0.1)) => \test;

//:
3 =!> \fadeTime;
//:
(degree: [20, 22].pseq, dur: 0.5) => \test;
//:
(degree: [10, 11].pseq, dur: 0.5) => \test;
//:
(amp: [0.01, 0.01, 0.3].pseq) %> \test;
//:
(instrument: [\lpfsaw, \lpfpulse, \bassfoundation].prand) %> \test;

(dur: 0.05) +> \test;

(dur: Pbrown(0.02, 0.15, 0.01, inf)) +> \test;

(amp: { 0.01 exprand: 1.2 }.pfunc) %> \test;

(degree: (-20..20).prand) %> \test;


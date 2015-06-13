//:
{ WhiteNoise.arps.perc(0.1) } +> \x;
//:
\x *> \y;
0.15 *>\y;
//:
[0.15, 0.3].pseq *>\y;
//:
[0.15, 0.15, 0.15, 0.3].pseq *>\y;
//:
\x *>.xoxxooxxxxxoooxxxxooooxxxxxooooooooxoooooooo \y;
//:

\z *> \x; 

{ SinOsc.arps(Rand(400, 800) )} +> \z;

//:
{ SinOsc.arps(Rand(60, 100).midicps )} +> \z;

//:
\z *>.xox___xoooxoxx \y;
//:
\z *>.xox___xoxo \x;
//:
{ LFSaw.arps(Rand(400, 800)).perc(0.1) } +> \z;

//:

{ LFSaw.arps(Rand(400, 8000).perc(0.5)).perc(0.5) }  +> \z;

//:
{ SinOsc.arps(Rand(400, 8000).perc(0.5)).perc(0.5) }  +> \z;
//:
{ SinOsc.arp({ Rand(400, 8000) }.dup.perc(0.5)).perc(0.5) }  +> \z;
//:
{ LFSaw.arp({ Rand(400, 8000) }.dup.perc(0.5)).perc(0.5) }  +> \z;


//:
1 *> \a;

//:

0.1 *> \a;

{ WhiteNoise.arps() } +> \a;

\sub *>.xoxoxoxx \master;

{ WhiteNoise.arps() } ++> \sub;

0.1 *> \master;
//:

\sub2 *>.oxoxxxoo \master;

{ BrownNoise.arps() } +> \sub2;

//:

\sub3 *>.xoxxx___ \master;

{ LFPulse.arps(\freq.kr(800)) } +> \sub3;

[60, 64, 67, 69].midicps.pseq +>.freq \sub3;

//:
\sub3 *>.x_xxxooo \sub2;
//:
\sub4 *>.xo \sub3;

{ SinOsc.arp([2500, 2000]).perc(0.3) * 2 } +> \sub4;

//:
0.1 *> \m1;

\r1 *>.xox \m1;

{ WhiteNoise.arps().perc } +> \r1;

\r2 *>.xox \m1;

[60, 62].midicps.pseq +>.freq \r2

{ SinOsc.arps(\freq.kr(400)) } +> \r2;

\r2 *>.xox \r1;

[64, 65].midicps.pseq +>.freq \r3

{ SinOsc.arp(\freq.kr(400) * [1, 1.2]) } +> \r3;

\r3 *>.xox \m1;

\r3 *>.xox \r1;

\r3 *>.xox \r2;

\r3 *>.x_x \r2;

(40..80).midicps.prand +>.freq \r3;




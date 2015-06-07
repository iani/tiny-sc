//:
\x |> \y;
0.15 |>\y;
//:
[0.15, 0.3].pseq |>\y;
//:
[0.15, 0.15, 0.15, 0.3].pseq |>\y;
//:
\x |>.xoxxooxxxxxoooxxxxooooxxxxxooooooooxoooooooo \y;
{ WhiteNoise.arps.perc(0.1) } ==> \x;

//:
\z |> \x;

{ SinOsc.arps(Rand(400, 800) )} ==> \z;

//:
\z |> \x;

{ SinOsc.arps(Rand(60, 100).midicps )} ==> \z;

//:
\z |>.xox___xoooxoxx \y;
//:
\z |>.xox___xoxo \x;
//:
{ LFSaw.arps(Rand(400, 800)).perc(0.1) } ==> \z;

//:

{ LFSaw.arps(Rand(400, 8000).perc(0.5)).perc(0.5) }  ==> \z;

//:
{ SinOsc.arps(Rand(400, 8000).perc(0.5)).perc(0.5) }  ==> \z;
//:
{ SinOsc.arp({ Rand(400, 8000) }.dup.perc(0.5)).perc(0.5) }  ==> \z;
//:
{ LFSaw.arp({ Rand(400, 8000) }.dup.perc(0.5)).perc(0.5) }  ==> \z;


//:
1 |> \a;

//:

0.1 |> \a;

{ WhiteNoise.arps() } ==> \a;

0 ==> \a;

\b |>.xoxoxoxx \a;

{ WhiteNoise.arps() } ==> \b;
//:

\c |>.oxoxxxoo \a;

{ BrownNoise.arps() } ==> \c;

//:

\d |>.oxoxxx__ \a;

{ LFSaw.arps(\freq.kr(800)) } ==> \d;

[60, 64, 67, 69].midicps.pseq =>.freq \d;

//:
([0.5, 1.00001, 5] * 0.15).pseq |> \mastertimeline;

0.1 |> \mastertimeline;

{ WhiteNoise.arps().perc(0.5) } ==> \initialstuff;

{ PinkNoise.arps().perc(0.5) } ==> \stuff1;

{ LFSaw.arps(300).perc(0.5) } ==> \stuff2;

{ SinOsc.arps(800).perc(0.5) } ==> \stuff3;


//:
\initialstuff |>.x__oooo \mastertimeline;
\stuff2 |>.ooxooooo \mastertimeline;
\stuff3 |>.xxoxoxoo \mastertimeline;








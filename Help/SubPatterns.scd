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


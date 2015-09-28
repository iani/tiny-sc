//:
"(ab)".arlink;
{ Blip.arp(140, 5) * 4 } ++> \a;
{ Ringz.ar(Inp.ar, LFNoise0.kr(5).range(30, 1000)) * 0.1 } ++> \b;
//:
"a".arlink
//:
"(abc)".arlink;
{ Inp.ar.abs * LFNoise0.kr(20) * 0.1 } ++> \c;
//:
"(a[bc])".arlink;
{ Inp.ar.abs * LFNoise0.kr([10, 20]) * 2 } ++> \c;
//:
{ Ringz.ar(Inp.ar, LFNoise0.kr(50).range(300, 400)) * 0.4 } ++> \b;
{ Inp.ar.sqrt.abs * SinOsc.ar(800) * 2 * LFNoise0.kr([5, 10]) * 2 } ++> \c;
//:
"(a[cb])".arlink;
{ Inp.ar.abs * LFNoise0.kr(10) * 30 } ++> \c;

//:
"(a[(1c2)(3b4)])".arlink;
{ Inp.ar.abs * LFNoise0.kr(10) * 30 } ++> \c;
{ Inp.ar } ++> '1';
{ Inp.ar } ++> '2';
{ Inp.ar } ++> '3';
{ Inp.ar } ++> '4';
//: ================================================================
//: Trying how to do this with compilation:
"%".arlink;
//:
SynthPlayer.all.first.isPlaying;
//:
"%%".arlink;
//:
"%a%".arlink;
{ PinkNoise.arp } ++> \a;
//:
"(%a%)".arlink;
{ PinkNoise.arp } ++> \a;
//:
"(%ab%)".arlink;
{ PinkNoise.arp } ++> \a;
{ Ringz.ar(Inp.ar, LFNoise2.kr(30).range(100, 1000)) * 0.1 } ++> \b;
//:
"(a[bc])".arlink;
{ PinkNoise.arp } ++> \a;
{ Ringz.ar(Inp.ar, LFNoise2.kr(30).range(100, 1000)) * 0.1 } ++> \b;
{ Ringz.ar(Inp.ar, LFNoise0.kr([1, 1]).range(100, 1000)) * 0.1 } ++> \c;
//:

//: ================================================================
//:
{ Ringz.ar(Inp.ar, LFNoise2.kr(1).range(30, 1000), 0.1) * 0 } ++> \b;
//:
{ LFNoise1.arp(140) } ++> \a;
//:
{ Inp.ar.abs * LFNoise0.kr(10) * 5.00001 } ++> \c;
//:
"(ac)".arlink;
{ Inp.ar.abs * LFNoise0.kr(10) * 1 } ++> \c;
//:
"(acb)".arlink;
{ Inp.ar * LFNoise0.kr(10) } ++> \c;
//:



//: ================================================================

//: mainly test level sorting for groups!

"b[(abc)[ax]]".arlink;
MiniSteno.current.pp;

"(a[bcd]e)".arlink;
MiniSteno.current.pp;

//:

a.inspect;
//:
"(abc)[x]de(fghzxqw[ij])".arlink;
MiniSteno.current.pp;
//:
"(a(bcd)e)".arlink;
MiniSteno.current.pp;
//:

b.findContainerOf(\a.synthPlayer);

b.pp;
 b.inspect;

"a".miniSteno;
"[a]".miniSteno;

"(ab)".arlink;
"a".arlink;
SynthPlayer.initInactive;
SynthPlayer.inactive;

//:
Server.default.queryAllNodes;

//:
		.replace("[", "Par(Ser('%', ")
	.replace("]", " '%')), ")

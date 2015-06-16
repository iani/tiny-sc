
a = "b[(abc)[ax]]".miniSteno;
a.pp;
a.inspect;

b = "(abc)[x]de(fgh[ij])".miniSteno;
b.findContainerOf(\a.chuck);

b.pp;
 b.inspect;

"a".miniSteno;
"[a]".miniSteno;

"(ab)".arlink;
"a".arlink;
Chuck.initInactive;
Chuck.inactive;

{ WhiteNoise.arps(0.1) } ++> \a;
{ Resonz.ar(In.ar(\in.kr(0)), LFNoise0.kr([10, 20]).range(30, 3000), 4.0) } ++> \b;


/* Debugging MapFunc, pub method.
IZ Tue, Mar  4 2014, 22:25 EET.

*/

~source = Pub();
~source.source = (1..10);
~source.start;

//:
~synth = { SinOsc.ar(\freq.kr(400), 0, 0.1) }.play;
~synth.pub(~source, [400, 800].bimapSet(\freq, [1, 10]));
//:
// ~source.source = MappingView(QSli(), [1, 10]);
~source.source = QSli().map([1, 10]);
//:
~source.source.spec = [1, 10].asSpec;
//:
~source.source = QSli();
~synth.pub(~source, \freq.mapSet(\freq));
//:
\monitor.pub(~source, \freq);
//:
\monitor.setPub(~source);
//:
~source.stop;
//:
~mapFunc = MapFunc(~source, \monitor, { \set });
//:
\monitor.setPub(~source, ~mapFunc);
//:
\monitor.setPub(~source, \freq); 

//////////////////////////////////////////////////////////
//:
~source.source = (1..10) / 10;
~source.start;
//:
\monitor.setPub(~source);
//:

~unmapper = UnmapSet(\x, [1, 10]);
~slider = QSli().pub(~source, [1, 10].unmapSet);

[1, 10].mapSet.inspect;
[1, 10].unmapSet.inspect;

1 respondsTo: \postln;
1 respondsTo: \unmapSet;

[1, 2].unmapSet;

[1, 2] respondsTo: \unmapSet;

UnmapSet(\x, [1, 10]);

//:

Pfuncn({ 10.rand }, 3).asStream.nextN(5);


/* Debugging MapFunc, src method.
IZ Tue, Mar  4 2014, 22:25 EET.

*/

~source = Source();
~source.source = (1..10);
~source.start;

//:
~synth = { SinOsc.ar(\freq.kr(400), 0, 0.1) }.play;
~synth.src(~source, [400, 800].bimapSet(\freq, [1, 10]));
//:
// ~source.source = MappingView(QSli(), [1, 10]);
~source.source = QSli().map([1, 10]);
//:
~source.source.spec = [1, 10].asSpec;
//:
~source.source = QSli();
~synth.src(~source, \freq.mapSet(\freq));
//:
\monitor.src(~source, \freq);
//:
\monitor.setSource(~source);
//:
~source.stop;
//:
~mapFunc = MapFunc(~source, \monitor, { \set });
//:
\monitor.setSource(~source, ~mapFunc);
//:
\monitor.setSource(~source, \freq); 

//////////////////////////////////////////////////////////
//:
~source.source = (1..10) / 10;
~source.start;
//:
\monitor.setSource(~source);
//:

~unmapper = UnmapSet(\x, [1, 10]);
~slider = QSli().src(~source, [1, 10].unmapSet);

[1, 10].mapSet.inspect;
[1, 10].unmapSet.inspect;

1 respondsTo: \postln;
1 respondsTo: \unmapSet;

[1, 2].unmapSet;

[1, 2] respondsTo: \unmapSet;

UnmapSet(\x, [1, 10]);


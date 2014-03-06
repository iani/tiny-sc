/* PatternPlayer tests
	IZWed, Mar  5 2014, 12:30 EET 

Tested: Wed, Mar  5 2014, 13:09 EET
*/

~player = PatternPlayer((1..10) / 10, 1);

QSli().pub(~player, \nil.mapSet);

~player.start;

~player.source.source.durations = 0.1;

~player.source.source.durations = Pwhite(0.01, 1);
~player.source.source.values = Pwhite(0.0, 1);

//:
~player.source.stop;
//:
~player.source.start;

//:
~player2 = PatternPlayer((1..10) / 10);

QSli().pub(~player2, \nil.mapSet);

~player2.start;

//: ------------- Testing PatternPlayer created by default: 

~source = Pub((1..10) / 10);

~source.start;

//:
~source.isPlaying;

//:
~source.stop;

//:
\monitor.addNotifier(~source, \value, { | ... args | args.postln; });

//:
~sourcex = QSli().pub([0.1, 0.2]);
~sourcex.start;

~sourcex.isPlaying;

~sourcex.source.values = (1..100)  / 100;

~sourcex.stop;
//:
\monitor.addNotifier(~sourcex, \value, { | ... args | args.postln });
//:
\monitor.objectClosed;
//:

~sourcey = QSli().pub(Prand((0..10)/10, inf));
~sourcey.start;




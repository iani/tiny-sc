/*
Facilitate transient mapping of SynthTree synth parameters to busses, 
into which control-rate functions may write signals to create "curves".

Start with BusLine:  Make a parameter go from one value 
to another in a specified time interval.

Later perhaps add generalizations.

IZ Sat, Mar 29 2014, 23:14 EET
*/

BusLine {
	var <param;
	var <from = 0, <to = 0, <dur = 1;
	var <bus;
	var <synth;
	
	*new { | param, from = 0, to = 0, dur = 1 |
		^this.newCopyArgs
	}

}
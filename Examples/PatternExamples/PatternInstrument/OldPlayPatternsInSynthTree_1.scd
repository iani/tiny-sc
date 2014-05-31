/*
Note: 

The syntax described here, using Array => Symbol, employs PatternInstrument
as a source that plays patterns inside a SynthTree.  This is an older technique, 
and may be deprecated in the future.  A newer technique uses Event => Symbol
to create Idef as player and has features such inheritance, cloning and filtering.
The other, newer technique is preferable. 

1. Playing a Pbind-like pattern in a SynthTree

Array => \symbol : 

Create a Pbind-style pattern and plays it in SynthTree named \symbol. 
The array is key-value pair array similar to that used to create a Pbind. 
For example:

[instrument: \bassfoundation, dur: Pseq([0.1, 0.2], inf)] => \symbol

2. Replacing the pattern of a single parameter in the last operated SynthTree

(Function, Value or Pattern) *> \symbol : 

Replace the pattern of the parameter named by \symbol with the new pattern,
in the last SynthTree, that is the last synthtree that was chucked to with => \symbol.

Also: (In next example file):
To replace the pattern of a single parameter in a synthree chosen by name, use 

pattern -> parameterName => \synthTreeName

For example: 

500 -> \freq => \pattern1
means: 
Set the parameter \freq in synthree named \pattern1 to 500

(Updated on: Sat, May 31 2014, 09:49 EEST)
*/

//: Play a pattern with (instrument: \default)
[instrument: \default] => \pattern1;

//: Add pattern for duration parameter
[dur: 0.1] => \pattern1;

//: Add pattern for note parameter
[note: [1, 3, 5, 10].prand + Pstutter(10, [0, 7, 9].pseq, inf)] => \pattern1;

//: Add pattern for legato
[legato: 0.1] => \pattern1;

//: Replace pattern for legato
[legato: [0.1 pn: 10, 6 pn: 5].pseq] => \pattern1;

//: Replace pattern for instrument parameter

[\instrument: [\sine, \default, \bassfoundation, \moogbasstone].pseq] => \pattern1;

//: Change pattern for duration
[0.1 pn: 8, 0.2 pn: 4, 0.05 pn: 12, 0.6].prand *> \dur;

//: Change pattern for note parameter

[note: [1, 3, 5, 10].prand + Pstutter(10, [0, 7, 9].pseq, inf) 
+ Pstutter(60, Pbrown(-10, 10, 5, inf) * Pstutter(10, [1, 2, 3].prand))
] => \pattern1;

//: Add pattern for amp parameter
{ 0.01 exprand: 0.2 } *> \amp;

//: Replace pattern for legato parameter
{ 0.1 exprand: 0.35 }.pfunc *> \legato;

//: Replace pattern for dur parameter

[
Pstutter([1, 2, 4, 8].prand, 0.1 pn: 8), 
Pstutter([1, 2, 4, 8].prand, 0.2 pn: 4), 
Pstutter([1, 2, 4, 8].prand, 0.05 pn: 12), 
Pstutter([1, 2, 4, 8].prand, 0.6 pn: 1),
2
].prand *> \dur;

//: Replace pattern for dur parameter

0.5 pn: inf *> \dur;

//: More examples of changing the patterns of single parameters.
//:
500 *> \freq;
//:
{ 100 rrand: 400 }.pfunc *> \freq;
//:
\bassfoundation *> \instrument;
//:
\werkitd *> \instrument;
//:
{ 1000 rrand: 4000 }.pfunc *> \freq;
//:
{ 0.1 exprand: 3 }.pfunc *> \legato;
//:
0.1 *> \dur;
//:
[0.5 pn: 5, { 2.0 exprand: 4.5 }.pfuncn(1)].prand *> \legato;
//:
Prand([500, 1000, 1500, 2000, 5000], inf) *> \freq;
//:
Pbrown(30, 90, 2, inf).midicps *> \freq;
//:
0.03 *> \dur;

//: Finally, fade out the current SynthTree;
~st.fadeOut(20);

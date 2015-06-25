/* 
Conclusions from the tests below: 

1000 events per second, while also changing rates at any moment are safe, *if one does not also load new synthdef functions while playing the task*.  The CPU load on an I7 Intel Mac can be under 20%. 

If loading new SynthDef functions, then the rate of events may fall as low as 100 events per second.  

To make that safer, the SynthDef loading mechanism has to be combined with the Synth starting and freeing mechanism.  This is not high on the priority list at the moment. 

*/

//: Jaeh
(
{
	{ SinOsc.arps(Rand(1000, 2000)).perc } +> \test0 *> 0.01;
	loop {
        [
            {{ SinOsc.arps(Rand(1000, 2000)).perc } +> \test0;},
			{{ SinOsc.arps(Rand(1000, 2000)) } +> \test0;},
            {   // 200 events per second
                [Pn(0.005, 120) ! 10, Pn(0.1, 4) ! 5, 0.5, 1.5].flat.prand **> \test0;
            },
            {    // 100, 1000, 5, 1 events per second
                [0.01.pn(50), 0.001.pn(100), 0.2.pn(10), 1].prand **> \test0;
            },
            { 0.001 **> \test0; } // 1000 events per second

        ].choose.value;
        0.2.rrand(3.0).wait;
	}
}.fork
)

//: 500 and 100 events per second, alternating at random. Stable ?
(
{
	0.002 *> \test0; // Initialize at 500 events / second;
	loop {
        [   // occasionaly send synthdefs to create CPU load spikes: 
            {{ SinOsc.arps(Rand(1000, 2000)).perc } +> \test0 },
			{{ SinOsc.arps(Rand(1000, 2000)) } +> \test0;},
            { 0.002 **> \test0; }, // 500 events / second
			{ 0.01 **> \test0; }   // 100 events / second
        ].choose.value;
        [0.1, 0.01 exprand: 0.1, 2].choose.wait;
	}
}.fork
)

//: As above, but remove synthef-sends from the loop
(
{
	0.002 *> \test0; // Initialize at 500 events / second;
	{ SinOsc.arps(Rand(1000, 2000)).perc } +> \test0;
	0.1.wait;
	loop {
        [  
            { 0.002 **> \test0; }, // 500 events / second
			{ 0.01 **> \test0; }   // 10 events / second
        ].choose.value;
        [0.1, 0.01 exprand: 0.1, 2].choose.wait;
	}
}.fork
)

//: As above, but at 1000 and 200 events per second
(
{
	0.002 *> \test0; // Initialize at 500 events / second;
	{ SinOsc.arps(Rand(1000, 2000)).perc } +> \test0;
	0.1.wait;
	loop {
        [  
            { 0.001 **> \test0; }, // 1000 events / second
			{ 0.005 **> \test0; }   // 200 events / second
        ].choose.value;
        [0.1, 0.01 exprand: 0.5, 1].choose.wait;
	}
}.fork
)

//: As above, but with released (not self-stopping enveloped) synth
(
{
	0.002 *> \test0; // Initialize at 500 events / second;
	{ SinOsc.arps(Rand(1000, 2000)) } +> \test0;
	0.1.wait;
	loop {
        [  
            { 0.001 **> \test0; }, // 1000 events / second
			{ 0.1 **> \test0; }   // 10 events / second. For distinctness
        ].choose.value;
        [0.1, 0.01 exprand: 0.5, 1].choose.wait;
	}
}.fork
)

//: Stable ???
(
{
	0.001 *> \test0; // start playing task with 1000 events / second;
	loop {
        [
            {{ SinOsc.arps(Rand(1000, 2000)).perc } +> \test0 },
			{{ SinOsc.arps(Rand(1000, 2000)) } +> \test0;},
            { 0.001 **> \test0; },
			{ 0.01 **> \test0; }
        ].choose.value;
        [0.1, 0.01, 2].choose.wait;
	}
}.fork
)

//:

(
{ SinOsc.arps(Rand(1000, 2000)).perc } +> \test0;
[Pn(0.005, 120) ! 10, Pn(0.1, 4) ! 5, 0.5, 1.5].flat.prand *> \test0;
)
//:

// More stress-tests. 

(
{ SinOsc.arps(Rand(400, 5000)).perc } +> \test0 *> [0.01.pn(50), 0.05.pn(30), 1].prand;
)

//: With nodes released at each new event
(
{ SinOsc.arps(Rand(100, 5000)) } +> \test0 *> [0.02.pn(50), 0.2.pn(10), 1].prand;
)
//:


// Testing the limits ...:
(
0.001 *> \test0;
)
//:
(
{ SinOsc.arps(Rand(100, 300)).perc } +> \test0 *> 0.001;
)
//:
(
{ SinOsc.arps(Rand(100, 300)) } +> \test0 *> 0.001;
)
//:

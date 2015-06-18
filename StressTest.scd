(
{
	{ SinOsc.arps(Rand(1000, 2000)).perc } +> \test0 *> 0.01;
	loop {
        [
            {{ SinOsc.arps(Rand(1000, 2000)).perc } +> \test0;},
			{{ SinOsc.arps(Rand(1000, 2000)) } +> \test0;},
            {
                [Pn(0.005, 120) ! 10, Pn(0.1, 4) ! 5, 0.5, 1.5].flat.prand **> \test0;
            },
            {
                [0.01.pn(50), 0.001.pn(100), 0.2.pn(10), 1].prand **> \test0;
            },
            { 0.001 **> \test0; }

        ].choose.value;
        0.2.rrand(3.0).wait;
	}
}.fork
)

//: Stable ?
(
{
	0.002 *> \test0; // start playing task with 500 events / second;
	loop {
        [
            {{ SinOsc.arps(Rand(1000, 2000)).perc } +> \test0 },
			{{ SinOsc.arps(Rand(1000, 2000)) } +> \test0;},
            { 0.002 **> \test0; },
			{ 0.01 **> \test0; }
        ].choose.value;
        [0.1, 0.01, 2].choose.wait;
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

// Here are some stress-tests.  Please report if your system encounters limits with these:

(
{ SinOsc.arps(Rand(400, 5000)).perc } +> \test0 *> [0.01.pn(50), 0.05.pn(30), 1].prand;


// With nodes released at each new event, the safety threshold is at about 50 events per second:
(
{ SinOsc.arps(Rand(100, 5000)) } +> \test0 *> [0.02.pn(50), 0.2.pn(10), 1].prand;
)
//:


// Testing the limits ...:
(
0.001 *> \test0;  // Can be unstable depending on general CPU load
)
//:


(
{ SinOsc.arps(Rand(100, 200)) } +> \test0 *> 0.001;
)
//:

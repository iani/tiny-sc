
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
                [0.02.pn(50), 0.2.pn(10), 1].prand **> \test0;
            },
            { 0.001 **> \test0; }

        ].choose.value;
        0.2.rrand(3.0).wait;
	}
}.fork

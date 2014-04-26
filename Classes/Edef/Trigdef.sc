/*
A Bdef that is triggered via OSC from trigger messages issued from a synth, 
rather than by a routine. 

Idea by Yorgos Diapoulis.

IZ Fri, Apr 25 2014, 16:04 EEST
*/

Trigdef : Bdef {

	var oscFuncs;

	play { 
		this.oscFuncs do: _.enable;
	}

	oscFuncs {
		oscFuncs ?? { oscFuncs = () };
		^oscFuncs;
	}

	test {
		this.changed(\event, (degree: 20));
	}

	addSource { | id = 0 |
		
	}

}
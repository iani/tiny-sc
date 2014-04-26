//Sound recipes from:
//Mitchell Sigman (2011) Steal this Sound. Milwaukee, WI: Hal Leonard Books
//adapted for SuperCollider and elaborated by Nick Collins (http://www.sussex.ac.uk/Users/nc81/index.html)
//under GNU GPL 3 as per SuperCollider license

//if you see strange multipliers, like *0.3, these are compensation factors for mixing together many oscillators, or loss of amplitude from filters so the patches work out to roughly equal output

/*
Here SynthDefs adapted as SynthTemplates for tiny-sc

IZ Thu, Apr 10 2014, 21:36 EEST
*/

/*
//bpm 120 assumed in the following, impacting on some delay recipes.
TempoClock.default.tempo = 2;

*/

//:
[
SynthDef(\prophet5pwmstrings, {
	| out = 0 freq = 440 amp = 1.0 gate = 1 lforate = 10 lfowidth = 0.5 
	cutoff = 12000 rq = 0.5 pan = 0.0 |

	var lfo, pulse, filter, env;

	lfo = LFTri.kr(lforate * [1, 1.01], Rand(0, 2.0)!2);
	pulse = Pulse.ar(freq * [1, 1.01], lfo * lfowidth + 0.5);
	filter = RLPF.ar(pulse, cutoff, rq);
	env = EnvGen.ar(Env.adsr(0.01, 0.0, 1.0, 0.5), gate, doneAction: 2);
	Out.ar(out, Pan2.ar(Mix(filter) * env * amp * 0.5, pan));
}), 

SynthDef(\singleoscillatorwobble, {
	| out = 0 freq = 440 amp = 1.0 gate = 1 lforate = 10 lfowidth = 0.5 
	cutoff = 12000 rq = 0.5 pan = 0.0 |

	var lfo, pulse, filter, env;

	lfo = LFTri.kr(lforate, Rand(0, 2.0) ! 2);
	pulse = Pulse.ar(freq * (1.0 + (lfowidth * lfo)), 0.5);
	filter = RLPF.ar(pulse, cutoff, rq);
	env = EnvGen.ar(Env.adsr(0.01, 0.0, 1.0, 0.5), gate, doneAction: 2);
	Out.ar(out, Pan2.ar(filter * env * amp * 0.5, pan));
	//	Pan2.ar(filter, pan).adsrOut;
}), 

SynthDef(\trianglewavebells, {
	| out = 0 freq = 440 amp = 0.1 gate = 1 lforate = 10 lfowidth = 0.0
	cutoff = 100 rq = 0.5 pan = 0.0 |

	var osc1, osc2, vibrato, filter, env;

	vibrato = SinOsc.ar(lforate, Rand(0, 2.0));

	osc1 = Saw.ar(freq * (1.0 + (lfowidth * vibrato)), 0.75);

	//Saw a bit rough, possibly slighter smoother:
	//osc1 = DPW4Saw.ar(freq * (1.0+(lfowidth * vibrato)), 0.5);

	osc2 = Mix(LFTri.ar((freq.cpsmidi+[11.9, 12.1]).midicps));

	//filter = (osc1+(osc2 * 0.5)) * 0.5; //no filter version
	filter = RHPF.ar((osc1+(osc2 * 0.5)) * 0.5, cutoff, rq);

	env = EnvGen.ar(Env.adsr(0.01, 0.1, 1.0, 0.5), gate, doneAction:2);

	Out.ar(out, Pan2.ar(filter * env * amp, pan));
}), 

SynthDef(\organdonor, { 
	|out = 0 freq = 440 amp = 0.1 gate = 1 lforate = 10 
	lfowidth = 0.0 cutoff = 100 rq = 0.5 pan = 0.0 | 

	var vibrato, pulse, filter, env;

	vibrato = SinOsc.ar(lforate, Rand(0, 2.0));

	//up octave, detune by 4 cents
	//11.96.midiratio = 1.9953843530485
	//up octave and a half, detune up by 10 cents
	//19.10.midiratio = 3.0139733629359

	//Pulse version
	//pulse = Mix(Pulse.ar(([1, 1.9953843530485, 3.0139733629359] * freq) * (1.0+(lfowidth * vibrato)), Rand(0.4, 0.6)!3, [1.0, 0.7, 0.3])) * 0.5;

	//better alternative
	pulse = Mix(VarSaw.ar(([1, 1.9953843530485, 3.0139733629359] * freq) * (1.0+(lfowidth * vibrato)), Rand(0.0, 1.0)!3, Rand(0.3, 0.5)!3, [1.0, 0.7, 0.3])) * 0.5;

	filter = RLPF.ar(pulse, cutoff, rq);

	env = EnvGen.ar(Env.adsr(0.01, 0.5, 1.0, 0.5), gate, doneAction:2);

	Out.ar(out, Pan2.ar(filter * env * amp, pan));

}), 

SynthDef(\werkit, { | out = 0 freq = 440 amp = 0.1 gate = 1 cutoff = 100 rq = 0.1 pan = 0.0 | 

	var source, filter, env;

	source = WhiteNoise.ar;

	filter = BLowPass4.ar(source, freq, rq) * 0.3;

	//env = EnvGen.ar(Env.adsr(0.01, 0.0, 1.0, 0.1), gate, doneAction:2);
	//no gate, fixed envelope size
	env = EnvGen.ar(Env([0, 1, 0.5, 0.0], [0.02, 0.1, 0.1]), doneAction:2);

	Out.ar(out, Pan2.ar((0.7 * filter+(0.3 * filter.distort)) * env * amp, pan));

}), 

SynthDef(\werkitd, { | out = 0 freq = 440 amp = 0.1 gate = 1 cutoff = 100 rq = 0.1 pan = 0.0 | 
	// Werkit with gate.
	var source, filter, env;

	source = WhiteNoise.ar;

	filter = BLowPass4.ar(source, freq, rq); // * 0.3;

	//env = EnvGen.ar(Env.adsr(0.01, 0.0, 1.0, 0.1), gate, doneAction:2);
	// release note added:
	env = EnvGen.ar(Env([0, 1, 0.5, 0.0], [0.02, 0.1, 0.1], releaseNode: 2), 
		gate, doneAction:2);

	Out.ar(out, Pan2.ar((0.7 * filter+(0.3 * filter.distort)) * env * amp, pan));

}), 

SynthDef(\laserbeam, { | out = 0 freq = 440 amp = 0.1 attackTime = 0.04 gate = 1 pan = 0.0 | 

	var osc1, freqenv, ampenv;

	freqenv = EnvGen.ar(Env([4, 0.5, 1, 1], [attackTime, 0.01, 1.0]));

	osc1 = LFTri.ar(freq * freqenv);

	//env = EnvGen.ar(Env.adsr(0.01, 0.0, 1.0, 0.1), gate, doneAction:2);
	//no gate, fixed envelope size
	ampenv = EnvGen.ar(Env([0, 1, 0.5, 0.0], [0.02, 0.2, 0.1]), doneAction:2);

	Out.ar(out, Pan2.ar(osc1 * ampenv * amp, pan));

}), 

SynthDef(\laserbeamd, { | out = 0 freq = 440 amp = 0.1 attackTime = 0.04 gate = 1 pan = 0.0 | 

	var osc1, freqenv, ampenv;

	freqenv = EnvGen.ar(Env([4, 0.5, 1, 1], [attackTime, 0.01, 1.0]));

	osc1 = LFTri.ar(freq * freqenv);

	//env = EnvGen.ar(Env.adsr(0.01, 0.0, 1.0, 0.1), gate, doneAction:2);
	// gate added:
	ampenv = EnvGen.ar(Env([0, 1, 0.5, 0.0], [0.02, 0.2, 0.1], releaseNode: 2), 
		gate, doneAction:2);

	Out.ar(out, Pan2.ar(osc1 * ampenv * amp, pan));

}), 

SynthDef(\moogbasstone, { | out = 0 freq = 440 amp = 0.1 gate = 1 cutoff = 1000 gain = 2.0 lagamount = 0.01 pan = 0.0 | 

	var osc, filter, env, filterenv;

	osc = Mix(VarSaw.ar(freq.lag(lagamount) * [1.0, 1.001, 2.0], Rand(0.0, 1.0)!3, Rand(0.5, 0.75)!3, 0.33));

	//alternative: richer source: see moogbasstone2 below
	//osc = Mix(Pulse.ar(freq.lag(lagamount) * [1.0, 1.001, 2.0], Rand(0.45, 0.5)!3, 0.33));

	filterenv = EnvGen.ar(Env.adsr(0.2, 0.0, 1.0, 0.2), gate, doneAction:2);
	filter =  MoogFF.ar(osc, cutoff * (1.0+(0.5 * filterenv)), gain);

	env = EnvGen.ar(Env.adsr(0.001, 0.3, 0.9, 0.2), gate, doneAction:2);

	Out.ar(out, Pan2.ar((0.7 * filter+(0.3 * filter.distort)) * env * amp * 1.5, pan));

}), 
SynthDef(\moogbasstone2, { | out = 0 freq = 440 amp = 0.1 gate = 1 attackTime = 0.2 
	fenvamount = 0.5 cutoff = 1000 gain = 2.0 pan = 0.0 | 

	var osc, filter, env, filterenv;

	//alternative: richer source
	osc = Mix(Pulse.ar(freq.lag(0.05) * [1.0, 1.001, 2.0], Rand(0.45, 0.5)!3, 0.33));

	filterenv = EnvGen.ar(Env.adsr(attackTime, 0.0, 1.0, 0.2), gate, doneAction:2);
	filter =  MoogFF.ar(osc, cutoff * (1.0+(fenvamount * filterenv)), gain);

	env = EnvGen.ar(Env.adsr(0.001, 0.3, 0.9, 0.2), gate, doneAction:2);

	Out.ar(out, Pan2.ar((0.7 * filter+(0.3 * filter.distort)) * env * amp, pan));

}), 
SynthDef(\mrostinato, { | out = 0 freq = 440 amp = 0.1 gate = 1 lforate = 10 lfowidth = 0.5 pan = 0.0 | 

	var lfo, pulse, filter, env;
	lfo = LFTri.kr(lforate, Rand(0, 2.0)!3);
	pulse = Pulse.ar(freq * [1, 1.01, 0.5], lfo * lfowidth+0.5);
	env = EnvGen.ar(Env.adsr(0.01, 0.05, 0.5, 0.1), gate, doneAction:2);
	Out.ar(out, Pan2.ar(Mix(pulse) * env * amp, pan));
}), 
SynthDef(\sampleplay, { | out = 0 bufnum = 0 amp = 0.1 gate = 1 pan = 0.0 | 

	var playbuf, env;
	playbuf = PlayBuf.ar(1, bufnum);
	env = EnvGen.ar(Env.adsr(0.0, 0.0, 1.0, 0.1), gate, doneAction:2);
	Out.ar(out, Pan2.ar(playbuf * env * amp, pan));
}), 
SynthDef(\plastickystrings, { | out = 0 freq = 440 amp = 0.1 gate = 1 lforate = 5900 lfowidth = 0.01 cutoff = 12000 rq = 0.5 pan = 0.0 | 

	var lfo, saw, filter, env;
	lfo = LFTri.ar(lforate, Rand(0, 2.0));
	saw = Saw.ar(freq * (1.0+(lfowidth * lfo)), 0.5);
	//filter = BBandPass.ar(saw, freq, 4);
	filter = BHiPass.ar(saw, freq, rq); 	//or using cutoff
	env = EnvGen.ar(Env.adsr(0.4, 0.0, 1.0, 1.0), gate, doneAction:2);
	Out.ar(out, Pan2.ar(filter * env * amp, pan));
}), 
SynthDef(\handclapsmash, { | out = 0 amp = 0.1 gate = 1 cutoff = 12000 rq = 0.5 releaseTime = 0.5 pan = 0.0 | 

	var noise, filter, filterenv, env;
	noise = WhiteNoise.ar;
	filterenv = EnvGen.ar(Env([0.0, 1.0, 0.3, 0.0], [0.0, 0.3, 0.3]));
	//this involves a gate control, less use here
	//filterenv = EnvGen.ar(Env.adsr(0.0, 0.3, 0.3, 0.3), gate);
	filter = BHiPass.ar(RLPF.ar(noise, cutoff * filterenv, rq), 150);
	//body
	filter = BPeakEQ.ar(filter, 440, 1.0, 8);
	//snap
	filter = BPeakEQ.ar(filter, 1200, 1.0, 14);
	//no gate, fixed percussion sound
	env = EnvGen.ar(Env.perc(0.005, releaseTime), doneAction:2);
	Out.ar(out, Pan2.ar(filter * env * amp, pan));
}), 
SynthDef(\handclapsmash2, { | out = 0 amp = 0.1 gate = 1 cutoff = 12000 cutoff2 = 10000 rq = 0.5 modamount = 0.0 modrate = 50 releaseTime = 0.7 pan = 0.0 | 

	var noise, filter, filterenv, modulation, env;
	noise = WhiteNoise.ar;
	filterenv = EnvGen.ar(Env([0.0, 1.0, 0.3, 0.0], [0.0, 0.3, 0.3]));
	modulation = (LFSaw.ar(modrate).range(0.0, 1.0)) * EnvGen.ar(Env([1, 1, 0, 0], [0.1, 0.0, 1.0]));

	//filter = BHiPass.ar(RLPF.ar(noise, cutoff * filterenv * (1.0+(modamount * modulation)), rq), 150); //alternative
	filter = BHiPass.ar(RLPF.ar(noise, cutoff * filterenv+ (cutoff2 * modamount * modulation), rq), 150);

	//body
	filter = BPeakEQ.ar(filter, 440, 1.0, 8);
	//snap
	filter = BPeakEQ.ar(filter, 1200, 1.0, 14);

	//no gate, fixed percussion sound
	env = EnvGen.ar(Env.perc(0.005, releaseTime), doneAction:2);

	//amp modulation too not very effective ((1.0-modamount)+(modamount * modulation))
	Out.ar(out, Pan2.ar(filter * env * amp, pan));

}), 
SynthDef(\bassfoundation, { | out = 0 freq = 440 amp = 0.1 gate = 1 /* cutoff = 1000 */
			rq = 0.5 pan = 0.0 | 
	var osc, filter, env, filterenv;
	osc = Saw.ar(freq);
	filterenv = EnvGen.ar(Env.adsr(0.0, 0.5, 0.2, 0.2), gate, doneAction:2);
	filter =  RLPF.ar(osc, \cutoff.cr(\freq) * filterenv + 100, rq);
	env = EnvGen.ar(Env.adsr(0.01, 0.0, 0.9, 0.05), gate, doneAction:2);
	Out.ar(out, Pan2.ar(filter * env * amp * 2, pan));
}),
SynthDef(\bassfoundationdebug, { | out = 0 freq = 440 
	/* amp = 0.1 */ gate = 1 /*cutoff= 1000 */
	rq=0.5 pan=0.0
	|
	
	var osc, filter, env, filterenv;
	osc = Saw.ar(freq);
	filterenv = EnvGen.ar(Env.adsr(0.0,0.5,0.2,0.2),gate,doneAction:2);
	filter =  RLPF.ar(osc, \cutoff.cr(\freq) * filterenv + 100, rq);
	env = EnvGen.ar(Env.adsr(0.01,0.0,0.9,0.05),gate,doneAction:2);
	//	Out.ar(out,Pan2.ar(filter*env*amp*2,pan));
	filter.ladsrOut;
}), 
SynthDef(\basshighend, { 
	| out = 0 freq = 440 amp = 0.1 gate = 1 /* cutoff = 3000 */
	rq = 0.2 /* drive = 2.0 */ pan = 0.0 |

	var osc, filter, env, filterenv;
	var ab, drive, cutoff;

	drive = \drive.cr([0, 10, 'linear', 0, 1.0]);
	cutoff = \cutoff.kr(3000);

	//osc = Mix(VarSaw.ar(freq * [0.25, 1, 1.5], Rand(0.0, 1.0)!3, 0.9, [0.5, 0.4, 0.1]));
	osc = Mix(Saw.ar(freq * [0.25, 1, 1.5], [0.5, 0.4, 0.1]));
	//osc = Mix(DPW4Saw.ar(freq * [0.25, 1, 1.5], [0.5, 0.4, 0.1]));
	filterenv = EnvGen.ar(Env.adsr(0.0, 0.5, 0.2, 0.2), gate, doneAction:2);
	filter =  RLPF.ar(osc, cutoff * filterenv + 100, rq);

 	//distortion
 	//filter = filter.distort.softclip;
	ab = abs(filter);
 	filter = (filter * (ab + drive)/(filter ** 2 + (drive - 1) * ab + 1));
	//remove low end
	filter = BLowShelf.ar(filter, 300, 1.0, -12);
	//dip at 1600Hz
	filter = BPeakEQ.ar(filter, 1600, 1.0, -6);
	env = EnvGen.ar(Env.adsr(0.01, 0.0, 0.9, 0.05), gate, doneAction:2);
	Out.ar(out, Pan2.ar(filter * env * amp * 2, pan));
}), 
SynthDef(\winwoodlead, { | out = 0 freq = 440 amp = 0.1 gate = 1 cutoff = 8000 rq = 0.8 lfowidth = 0.01 lforate = 8 lagamount = 0.01 pan = 0.0 | 

	var pulse, filter, env, lfo;

	lfo = LFTri.kr(lforate, Rand(0, 2.0)!2);

	pulse = Mix(Pulse.ar((freq.lag(lagamount)) * [1, 1.001] * (1.0+(lfowidth * lfo)), [0.2, 0.19])) * 0.5;

	filter =  RLPF.ar(pulse, cutoff, rq);

	//remove low end
	filter = BLowShelf.ar(filter, 351, 1.0, -9);

	env = EnvGen.ar(Env.adsr(0.01, 0.0, 0.9, 0.05), gate, doneAction:2);

	Out.ar(out, Pan2.ar(filter * env * amp, pan));

}), 
SynthDef(\spookystrings, { | out = 0 freq = 440 amp = 0.1 gate = 1 cutoff = 8000 rq = 0.2 lfowidth = 0.01 lforate = 3.8 pan = 0.0 | 

	var pulse, filter, env, lfo;

	lfo = LFTri.kr(lforate, Rand(0, 2.0)!2);

	//LFO only on second oscillator
	pulse = Mix(Saw.ar(freq * [1, 1.001] * [1.0, 1.0+(lfowidth * lfo)])) * 0.5;

	filter = BHiPass.ar(pulse, cutoff, rq);

	env = EnvGen.ar(Env.adsr(0.1, 0.0, 1.0, 3), gate, doneAction:2);

	Out.ar(out, Pan2.ar(filter * env * amp, pan));

}), 
SynthDef(\spookystrings2, { | out = 0 freq = 440 amp = 0.1 gate = 1 cutoff = 8000 rq = 0.2 pan = 0.0 | 

	var saw, pulse, filter, env, lfo, pulsewidthlfo;

	lfo = LFTri.kr([3.5, 5], Rand(0, 2.0)!2);

	pulsewidthlfo = SinOsc.kr(4.5);

	saw = Saw.ar(freq * (1.0+(0.052 * lfo[0])));

	pulse = Pulse.ar(freq * (1.0+(0.072 * lfo[1])), pulsewidthlfo * 0.25+0.5);

	//combine, equal mix
	pulse = 0.5 * (saw+pulse);

	//filter specification not the clearest, so just going for slow attack to brightness
	filter = BLowPass.ar(pulse, XLine.kr(678, cutoff, 0.378), rq);

	env = EnvGen.ar(Env.adsr(0.1, 0.0, 1.0, 3), gate, doneAction:2);

	Out.ar(out, Pan2.ar(filter * env * amp, pan));

}), 
SynthDef(\synchronicity, { | out = 0 freq = 440 amp = 0.1 gate = 1 cutoff = 8000 rq = 0.2 pan = 0.0 | 

	var pulse, filter, env, slavefreq;

	slavefreq = EnvGen.ar(Env([1, 5, 1], [0.04, 0.9])) * freq;

	pulse = SyncSaw.ar(freq, slavefreq);

	filter = BLowPass.ar(pulse, cutoff, rq);

	env = EnvGen.ar(Env.adsr(0.01, 0.0, 1.0, 0.1), gate, doneAction:2);

	Out.ar(out, Pan2.ar(filter * env * amp, pan));

}), 
SynthDef(\situationsynth, { | out = 0 freq = 440 amp = 0.1 gate = 1 cutoff = 8000 rq = 0.8 lfowidth = 0.001 lforate = 3.3 pan = (-0.1) | 

	var pulse, filter, env, filterenv, lfo;

	lfo = LFTri.kr(lforate, Rand(0, 2.0)!2);

	pulse = Mix(Pulse.ar((((freq.cpsmidi)+[0, 0.14])+(lfo * lfowidth)).midicps, [0.5, 0.51]+(lfowidth * lfo))) * 0.5;

	filterenv = EnvGen.ar(Env([0.0, 1.0, 0.3, 0.0], [0.005, 0.57, 0.1], -3));

	filter =  RLPF.ar(pulse, 100+(filterenv * cutoff), rq);

	env = EnvGen.ar(Env.adsr(0.002, 0.57, 1.0, 0.3), gate, doneAction:2);

	Out.ar(out, Pan2.ar(filter * env * amp, pan));

}), 
SynthDef(\ressquares, { | out = 0 freq = 440 amp = 0.1 gate = 1 cutoff = 8000 rq = 0.8 pan = (-0.1) | 

	var pulse, filter, env;

	//2 cents detune for second oscillator
	pulse = Mix(Pulse.ar( ((freq.cpsmidi)+[0, 0.02]).midicps, 0.5)) * 0.5;

	filter =  BLowPass.ar(pulse, 100+cutoff, rq);

	env = EnvGen.ar(Env.adsr(0.002, 0.1, 1.0, 0.2), gate, doneAction:2);

	Out.ar(out, Pan2.ar(filter * env * amp, pan));

}), 
SynthDef(\whoorgan, { | out = 0 freq = 440 amp = 0.1 gate = 1 cutoff = 8000 rq = 0.8 pan = (-0.1) | 

	var osc, filter, env;
	var basefreq =  ((freq.cpsmidi)+[0, 0.1]).midicps;

	//osc = Mix(Blip.ar(basefreq, 16)) * 0.5;

  	osc = Mix.fill(10, { | i| SinOsc.ar(basefreq * (i+1), 0.0, (0.25+(0.75 * LFNoise0.kr(10.1+i, 0.5, 0.5)))/(1.0+(0.5 * i)))}) * 0.2;

	filter =  BLowPass.ar(osc, 2000+(amp * cutoff), rq);

	env = EnvGen.ar(Env.adsr(0.02, 0.1, 0.9, 1.0), gate, doneAction:2);

	Out.ar(out, Pan2.ar(Mix(filter) * env * amp, pan));

}), 
SynthDef(\eightoeightkick, { | out = 0 freq = 440 amp = 0.1 ringTime = 10.0 releaseTime = 1.0 distortion = 0.1 pan = (-0.1) | 

	var impulse, filter, env;

  	impulse = Impulse.ar(0);

	filter = Ringz.ar(impulse, XLine.ar(freq, 60, 0.1), ringTime);

	env = EnvGen.ar(Env.perc(0.001, releaseTime), doneAction:2);

	filter = (1.0-distortion) * filter + (distortion * (filter.distort));
	Out.ar(out, Pan2.ar(filter * env * amp, pan));

}), 
SynthDef(\pwmbling, { | out = 0 freq = 440 amp = 0.1 gate = 1 lforate = 4.85 lfowidth = 0.5 cutoff = 12000 rq = 0.25 pan = 0.0 | 

	var lfo, pulse, filter, env;
	var basefreq =  ((freq.cpsmidi)+[0, 12.12]).midicps;

	lfo = LFTri.kr(lforate * [1, 1.01], Rand(0, 2.0)!2);

	pulse = Pulse.ar(basefreq, lfo * lfowidth+0.5);

	env = EnvGen.ar(Env.adsr(0.0, 1.0, 0.2, 1.5), gate, doneAction:2);

	filter = RLPF.ar(pulse, (cutoff * (env.squared))+100, rq);

	Out.ar(out, Pan2.ar(Mix(filter) * env * amp, pan));

}), 

SynthDef(\tonewheeltwo, { | out = 0 freq = 440 amp = 0.1 gate = 1 lforate = 4.85 lfowidth = 0.1 cutoff = 5000 rq = 0.25 pan = 0.0 | 

	var lfo, pulse, filter, env;

	lfo = LFTri.kr(lforate * [1, 1.01], Rand(0, 2.0)!2);

	pulse = Pulse.ar( (((freq * [1, 3]).cpsmidi) + (lfo * lfowidth)).midicps, [0.5, 0.51], [0.4, 0.6]);

	env = EnvGen.ar(Env.adsr(0.0, 0.0, 1.0, 0.1), gate, doneAction:2);

	filter = BLowPass4.ar(pulse, cutoff, rq);

	filter = BPeakEQ.ar(filter, 500, 1.0, 3);

	Out.ar(out, Pan2.ar(Mix(filter) * env * amp, pan));

}), 

SynthDef(\everythingrhodes, { | out = 0 freq = 440 amp = 0.1 gate = 1 lforate = 4.85 lfowidth = 0.5 cutoff = 2000 rq = 0.5 pan = 0.0 | 

	var pulse, filter, env;

	pulse = Pulse.ar(freq * [1, 33.5.midiratio], [0.2, 0.1], [0.7, 0.3]);

	env = EnvGen.ar(Env.adsr(0.0, 1.0, 0.8, 3.0), gate, doneAction:2);

	//keyboard tracking filter cutoff
	filter = BLowPass4.ar(pulse, (cutoff * (env.squared))+200+freq, rq);

	Out.ar(out, Pan2.ar(Mix(filter) * env * amp, pan));

}), 

SynthDef(\justwannahavefun, { | out = 0 freq = 440 amp = 0.1 ringTime = 10.0 pan = (-0.1) | 

	var impulse, filter, env;

  	impulse = Impulse.ar(0);

	filter = Ringz.ar(impulse, (freq.cpsmidi + (Rand(0.2, 1.2) * SinOsc.kr(Rand(10, 50)))).midicps, ringTime);

	env = EnvGen.ar(Env([0.0, 1.0, 1.0, 0.0], [0.001, 0.04, 0.04]), doneAction:2);

	Out.ar(out, Pan2.ar((filter.softclip(0.9)) * env * amp, pan));

}), 
SynthDef(\spacetheremin, { | out = 0 freq = 440 amp = 0.1 gate = 1 lforate = 6 lfowidth = 0.5 cutoff = 4000 rq = 0.25 lagTime = 0.1 pan = 0.0 | 

	var lfo, osc, filter, env;

	lfo = LFTri.kr(lforate+(LFNoise1.kr(5, 0.3, 0.3)), Rand(0, 2.0));

	osc = Saw.ar((freq.lag(lagTime).cpsmidi+(lfo * lfowidth)).midicps, 0.5);

	filter = BLowPass4.ar(osc, cutoff.lag(lagTime * 4), rq);

	env = EnvGen.ar(Env.adsr(0.6, 0.0, 1.0, 0.05), gate, doneAction:2);

	Out.ar(out, Pan2.ar(filter * env * amp.lag(lagTime * 4), pan));

}), 

SynthDef(\fatvelocitybass, { | out = 0 freq = 440 amp = 0.1 gate = 1 cutoff = 12000 rq = 0.25 lagTime = 0.01 pan = 0.0 | 

	var lfo, osc, filter, env;

	var basefreq =  ((freq.lag(lagTime).cpsmidi)+[0, 11.95, 31.03]).midicps;

	osc = Saw.ar(basefreq, [0.5, 0.4, 0.1]); //+PinkNoise.ar(Line.kr(1.0, 0, 0.03));

	env = EnvGen.ar(Env.adsr(0.01, 1.0, 1.0, 0.25), gate, doneAction:2);

	filter = BLowPass4.ar(osc, 100+((amp.squared) * (freq+cutoff)), rq);

	Out.ar(out, Pan2.ar(Mix(filter) * env * amp * 0.8, pan));

})
] do: { | sd | SynthTemplate(sd.name, sd.add, ['sts-instruments']) };

//: = = = = = = = = = = = = = = = = EFFECTS = = = = = = = = = = = = = = = = 
[
SynthDef(\choruseffect, { | in = 0 out = 0 gate = 1 |
	var source = In.ar(in, 2);
	var chorus;
	var env = Linen.kr(gate, 0.1, 1, 0.1, 2);

	chorus = Mix.fill(7, {
		var maxdelaytime = rrand(0.005, 0.02);
		DelayC.ar(source, maxdelaytime, 
			LFNoise1.kr(Rand(4.5, 10.5), 0.25 * maxdelaytime, 0.75 * maxdelaytime)
		)
	});
	Out.ar(out, env * chorus);
}), 
SynthDef(\delayeffect, { | out = 0 gate = 1 | 
	var source = In.ar(out, 2);
	var delay;
	var env = Linen.kr(gate, 0.1, 1, 0.1, 2);

	delay = CombC.ar(source, 0.25, 0.25, 2.0);

	XOut.ar(out, env, delay);

}), 
SynthDef(\sampleeq1, { | out = 0 gate = 1 | 

	var source = In.ar(out, 2);
	var env = Linen.kr(gate, 0.0, 1, 0.1, 2);
	var eq;

	eq = BLowShelf.ar(source, 100, 1.0, 3);
	eq = BPeakEQ.ar(eq, 600, 1.0, -3);

	XOut.ar(out, env, eq);

}), 

SynthDef(\sampleeq2, { | out = 0 gate = 1 | 

	var source = In.ar(out, 2);
	var env = Linen.kr(gate, 0.0, 1, 0.1, 2);
	var eq;

	eq = BHiPass(150, 0.3);

	XOut.ar(out, env, eq);

}), 


SynthDef(\samplereverb, { | out = 0 gate = 1 | 

	var source = In.ar(out, 2);
	var env = Linen.kr(gate, 0.0, 0.3, 0.1, 2);
	var reverb;

	reverb = FreeVerb.ar(source, 1.0, 0.6, 0.6);

	XOut.ar(out, env, reverb);

}), 

SynthDef(\choruseffect2, { | out = 0 gate = 1 | 
	var source = In.ar(out, 2);
	var chorus;
	var env = Linen.kr(gate, 0.1, 1, 0.1, 2);

	chorus = Mix.fill(17, {
		var maxdelaytime = rrand(0.005, 0.02);
		DelayC.ar(source, maxdelaytime, LFNoise1.kr(Rand(4.5, 10.5), 0.25 * maxdelaytime, 0.75 * maxdelaytime) )

	});

	XOut.ar(out, env, 0.5 * chorus);

}),

SynthDef(\spookyeffect, { | out = 0 gate = 1 delaytime = 0.25 phaserate = 0.1 | 
	var source = In.ar(out, 2);
	var delay, phaser;

	var env = Linen.kr(gate, 0.0, 1, 0.1, 2);

	delay = DelayN.ar(source, 0.8, delaytime);

	phaser = delay;
	//phaser = AllpassN.ar(delay, 0.04, SinOsc.kr(phaserate, 0, 0.005, 0.035)); //not working that well, too obvious, need to make more subtle

	Out.ar(out, phaser);

}),

SynthDef(\yazoodelayeffect, { | out = 0 gate = 1 pan = 0.1 | 
	var source = In.ar(out, 2);
	var delay;
	var env = Linen.kr(gate, 0.0, 1, 0.1, 2);

	delay = DelayC.ar(source[0].distort, 0.25, 0.25);

	Out.ar(out, Pan2.ar(delay * env, pan));

}), 
SynthDef(\synthdistortion, { | out = 0 gate = 1 | 
	var source = In.ar(out, 2);
	var env = Linen.kr(gate, 0.0, 1, 0.1, 2);
	var abs, excess, output;

	abs = source.abs;

	excess = (abs-0.1).max(0.0).min(0.9)/0.9;

	//original plus sinusoidal perturbation of amount based on absolute amplitude
	output = source+(excess * (sin(excess * 2pi * 5) * 0.5-0.5));

	XOut.ar(out, env, output * env);

}), 
SynthDef(\filtereffect, { | out = 0 gate = 1 | 
	var source = In.ar(out, 2);
	var env = Linen.kr(gate, 0.0, 1, 0.1, 2);
	var output;

	output = BLowPass.ar(BLowPass.ar(source, SinOsc.ar(0.25, 0, 5000, 5500), 0.4), (200+(5000 * SinOsc.ar(4.01, 0.5, 0.5, 0.5))), 0.6);

	//output = BLowPass.ar(source, SinOsc.ar(0.275, 0, 5000, 5500), 0.1);

	XOut.ar(out, env, output * env);

}),

SynthDef(\choruscompresseffect, { | out = 0 gate = 1 | 
	var source = In.ar(out, 2);
	var chorus;
	var env = Linen.kr(gate, 0.1, 1, 0.1, 2);

	chorus = Splay.ar(Array.fill(4, {
		var maxdelaytime = rrand(0.005, 0.02);

		DelayC.ar(source[0], maxdelaytime, LFNoise1.kr(Rand(0.1, 0.6), 0.25 * maxdelaytime, 0.75 * maxdelaytime) )
	}));

	chorus = Compander.ar(4 * (source + chorus), source, 0.4, 1, 4);

	XOut.ar(out, env, chorus);

}), 

SynthDef(\gatedreverb, { | out = 0 gate = 1 threshold = 0.1 amp = 0.1 | 

	var a, c, z, y, in;
	var env = Linen.kr(gate, 0.1, 1, 0.1, 2);

	c = 5; // number of comb delays
	a = 4; // number of allpass delays

	in = In.ar(out, 2);

	gate = if((in.mean)>threshold, in.mean, DC.ar(0)!2);

	// reverb predelay time :
	z = gate; //DelayN.ar(gate, 0.048, 0.048);

	//for delaytime if want modulation-	//LFNoise1.kr(0.1.rand, 0.04, 0.05)
	y = Mix.arFill(c, {CombL.ar(z, 0.1, rrand(0.01, 0.1), rrand(1.5, 4))});

	// chain of 4 allpass delays on each of two channels (8 total) :
	a.do({ y = AllpassN.ar(y, 0.051, [rrand(0.01, 0.05), rrand(0.01, 0.05)], 1) });

	Out.ar(out, Pan2.ar(y, 0) * env * amp);

}),

SynthDef(\spacereverb, { | out = 0 gate = 1 threshold = 0.1 amp = 0.1 | 

	var a, c, z, y, in;
	var env = Linen.kr(gate, 0.1, 1, 0.1, 2);

	c = 6; // number of comb delays
	a = 4; // number of allpass delays

	in = In.ar(out, 2);

	// reverb predelay:
	z = DelayN.ar(in, 0.048, 0.048);

	y = Mix.arFill(c, {CombL.ar(z, 0.1, rrand(0.01, 0.1), 5)});

	// chain of 4 allpass delays on each of two channels (8 total) :
	a.do({ y = AllpassN.ar(y, 0.051, [rrand(0.01, 0.05), rrand(0.01, 0.05)], 1) });

	Out.ar(out, Pan2.ar(y, 0) * env * amp);

}),

SynthDef(\samplecompress, { | out = 0 gain = 2 reduction = 8 ratio = 3 gate = 1 
	attackTime = 0.016 relaxTime = 0.05 | 

	var source = In.ar(out, 2);
	var compression;
	var env = Linen.kr(gate, 0.0, 1, 0.1, 2);

	compression = Compander.ar(2 * source, source, (-8).dbamp, 1.0, ratio.reciprocal, attackTime, relaxTime);

	XOut.ar(out, env, compression);

})
] do: { | sd | SynthTemplate(sd.name, sd.add, ['sts-fx']) }


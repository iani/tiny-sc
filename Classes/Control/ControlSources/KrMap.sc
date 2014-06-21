/* 
Experimental: Kr bus that has many inputs 
and can be mapped to many parameters in many synthtrees. 

Wed, Jun 18 2014, 11:52 EEST

*/

KrSource {
	var <template;
	var <bus;
	var <synth;

	*new { | template bus |
		^this.newCopyArgs(template, bus);
	}

	setTemplate { | argTemplate |
		template = argTemplate;
		if (synth.isPlaying) {
			synth.objectClosed.free; // do not notify stopped
			this.makeSynth;
		};
	}

	makeSynth {
		synth = template.kr(bus.index, server: bus.server)
		.onEnd(this, { this.changed(\stopped); synth = nil; })
		.onStart(this, { this.changed(\started) });
	}

	start {
		if (synth.isPlaying) { ^this };
		this.makeSynth;
		// synth.onStart(this, { this.changed(\started) });
	}
	isPlaying { ^synth.isPlaying }
	free { if (synth.isPlaying) { synth.free } }
}

KrMap : Bus {
	var <sources;

	// Note: new method not subclassable
	*newKr { | server numChannels = 1 |
		^this.control(server, numChannels).initKrMap;
	}


	initKrMap { sources = IdentityDictionary() }

	addSource { | template name = \source startNow = true |
		var source;
		source = sources[name];
		if (source.isNil) {
			source = KrSource(template, this);
			sources[name] = source;
		}{
			source.setTemplate(template);
		};
		if (startNow) { source.start };
		^source;
	}

	free {
		sources do: _.free;
		super.free;
		this.objectClosed;
	 }

}

+ Symbol {
	k { | server, numChannels = 1 |
		^Registry(\krMap, this, { KrMap(server, numChannels) })
	}
}
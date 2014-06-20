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
			synth.free;
			this.start(true);
		};
	}

	start { | doRestart = false |
		if (doRestart.not and: { synth.isPlaying }) { ^this };
		synth = template.kr(bus.index, server: bus.server);
	}
}

KrMap : Bus {
	var <name;
	var sources;

	*new { | server numChannels = 1 |
		^super.control(server, numChannels).initKrMap;
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
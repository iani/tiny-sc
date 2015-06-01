/*
	Tue, May 26 2015, 09:37 CEST

	Simpler alternative to SynthTree?
*/

Chuck {
    var <name, <process;

    *new { | name, template, params |
        ^Registry(Chuck, name, { this.newCopyArgs(name).init(template, params) });
    }

    init { | template, params |
        process = template.asChuckProcess (this, params)
    }

    play { | template |
        process.stop;
        this.makeProcess (template ?? { process.template }).play;
        this.changed (\started);
    }

    eval { | func |
        this.play (CfuncTemplate (func))
    }

    makeProcess { | template |
        process = template.asChuckProcess(this, process.params);
        ^process;
    }

    setArgs { | ... args |
        // set a parameter in the synth of a Chuck type which
        // has a synth or pattern type playing process
        process.setArgs (args);
        this.changed (\args, args);
    }

    setProcessParameter { | parameter, value |
        // Set a parameter in the environment of the process
        process.setProcessParameter (parameter, value);
        this.changed (\parameter, parameter, value);
    }

    free { process.free }
    release { | dur = 0.1 | process release: dur }

	// LINKING
	prependAudio { | chuck, io = \in_out |
		/* Add chuck to my writers.  Move it to my Linkbus */
		
    }
    appendAudio { | chuck, io = \in_out |
		/* Add chuck to my readers.  Move it to my LinkBus */
    }

	// RHYTHM
    addToBeat { | beat |
        beat.add(this, { this.play });
        ^beat;
    }
}

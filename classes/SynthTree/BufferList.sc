/*
Save and load list of buffers from the Library.

IZ Sun, Mar 16 2014, 17:45 EET
*/

BufferList {
	
	classvar <>autoload = false;
	
	var <server;
	var <namesPaths;

	*initClass {
		StartUp add: {
			this.addNotifier(SynthTree, \serverBooted, {
				[this, thisMethod.name, autoload].postln;
				if (autoload) { this.loadFolder };
			});
		}
	}

	*new { | server |
		^this.newCopyArgs(server ? Server.default).init;
	}

	init {
		var bufferDict, buffer;
		bufferDict = Library.at(server);
		namesPaths = bufferDict.keys.asArray.select({ | key |
			bufferDict[key].path.notNil;
		}).collect({ | key |
			buffer = bufferDict[key];
			[key, buffer.path]
		});
	}

	save {
		namesPaths.writeArchive(this.defaultPath);
	}

	defaultPath { ^this.class.defaultPath }

	*defaultPath {
		^Platform.userAppSupportDir +/+ "BufferList.sctxar";
	}

	*loadFolder { | path |
		var extension;
		[this, thisMethod.name].postln;
		path ?? { path = Platform.userAppSupportDir +/+ "sounds/*" };
		path.pathMatch.postln;
		path.pathMatch do: { | p |
			extension = PathName(p).extension.asSymbol;
			if ([\aiff, \aif, \wav] includes: extension) {
				postf("will load: %\n", p);
				
			}
		}
		
		//		^Object.readArchive(this.defaultPath);
	}
	

}
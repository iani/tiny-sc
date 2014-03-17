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
			if (autoload) { this.loadFolder };
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
		var pathname, extension, server;
		//		[this, thisMethod.name].postln;
		server = Server.default;
		path ?? { path = Platform.userAppSupportDir +/+ "sounds/*" };
		//	path.pathMatch.postln;
		path.pathMatch do: { | filePath |
			pathname = PathName(filePath);
			extension = pathname.extension.asSymbol;
			if ([\aiff, \aif, \wav] includes: extension) {
				postf("Pre-loading: %\n", filePath);
				Library.put(server, pathname.fileNameWithoutExtension.asSymbol, 
					BufferDummy(filePath);
				);
			}
		}
		
		//		^Object.readArchive(this.defaultPath);
	}
}

BufferDummy {
	var <path;
	*new { | path | ^this.newCopyArgs(path) }
	server { ^SynthTree.server }
}
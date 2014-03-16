/*
Save and load list of buffers from the Library.

IZ Sun, Mar 16 2014, 17:45 EET
*/

BufferList {
	
	var <server;
	var <namesPaths;

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

	*load {
		^Object.readArchive(this.defaultPath);
	}
	

}
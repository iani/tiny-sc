/*
Save and load list of buffers from the Library.

IZ Sun, Mar 16 2014, 17:45 EET
*/

BufferList {
	
	classvar <>autoload = false;
	classvar <all;

	var <server;
	var <namesPaths;

	*initClass {
		all = IdentityDictionary();
		StartUp add: {
			if (autoload) { this.loadFolder };
		}
	}

	*new { | server |
		var instance;
		server = server ?? { SynthTree.server };
		instance = all[server];
		if (instance.isNil) {
			instance = this.newCopyArgs(server).init;
			all[server] = instance;
		};
		^instance;
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
	}

	*showList { | key, server |
		^this.new(server).showList(key);
	}

	showList { | key |
		var buffers, keys;
		buffers = Library.at(server);
		keys = buffers.keys.asArray.select({ | b | buffers[b].path.notNil }).sort;
		Windows.for(this, key ? \list, { | window |
			var list;
			window.view.layout = VLayout(
				list = ListView().items_(keys);
			);
			list.action = { | me | window.changed(\buffer, me.item); };
			list.keyDownAction = { | view, char, modifiers, unicode, keycode, key |
				switch (char,
					13.asAscii, {
						if (view.item.asSynthTree.isPlaying) {
							view.item.asSynthTree.stop;
						}{
							{ \buf.playBuf } => view.item.buf
							.set(\amp, 1)
							.set(\loop, if (modifiers == 0) { 0 } { 1 });
						}
					},
					Char.space, { Library.at(server, view.item).play; },
					$l, { SynthTree.faders; },
					{ view.defaultKeyDownAction(
						char, modifiers, unicode, keycode, key) 
					}
			)
		};
		});
	}
	asString {
		
	}
}

BufferDummy {
	var <path;
	*new { | path | ^this.newCopyArgs(path) }
	server { ^SynthTree.server }
}
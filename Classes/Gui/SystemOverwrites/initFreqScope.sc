/*
Per default, use linear mode for frequency display, instead of logarithmic mode. 
*/

+ PlusFreqScopeWindow {
	*new { arg width=522, height=300, busNum=0, scopeColor, bgColor;
		var rect, scope, window, pad, font, freqLabel, freqLabelDist, dbLabel, dbLabelDist;
		var setFreqLabelVals, setDBLabelVals;
		var nyquistKHz;
		if(scopeOpen != true, { // block the stacking up of scope windows
			//make scope

			scopeOpen = true;

			if(scopeColor.isNil, { scopeColor = Color.green });
			if(bgColor.isNil, { bgColor = Color.green(0.1) });

			rect = Rect(0, 0, width, height);
			pad = [30, 48, 14, 10]; // l,r,t,b
			font = Font.monospace(9);
			freqLabel = Array.newClear(12);
			freqLabelDist = rect.width/(freqLabel.size-1);
			dbLabel = Array.newClear(17);
			dbLabelDist = rect.height/(dbLabel.size-1);

			nyquistKHz = PlusFreqScope.server.sampleRate;
			if( (nyquistKHz == 0) || nyquistKHz.isNil, {
				nyquistKHz = 22.05 // best guess?
			},{
				nyquistKHz = nyquistKHz * 0.0005;
			});

			setFreqLabelVals = { arg mode, bufsize;
				var kfreq, factor, halfSize;

				factor = 1/(freqLabel.size-1);
				halfSize = bufsize * 0.5;

				freqLabel.size.do({ arg i;
					if(mode == 1, {
						kfreq = (halfSize.pow(i * factor) - 1)/(halfSize-1) * nyquistKHz;
					},{
						kfreq = i * factor * nyquistKHz;
					});

					if(kfreq > 1.0, {
						freqLabel[i].string_( kfreq.asString.keep(4) ++ "k" )
					},{
						freqLabel[i].string_( (kfreq*1000).asInteger.asString)
					});
				});
			};

			setDBLabelVals = { arg db;
				dbLabel.size.do({ arg i;
					dbLabel[i].string = (i * db/(dbLabel.size-1)).asInteger.neg.asString;
				});
			};

			window = Window("Freq Analyzer", rect.resizeBy(pad[0] + pad[1] + 4, pad[2] + pad[3] + 4), false);

			freqLabel.size.do({ arg i;
				freqLabel[i] = StaticText(window, Rect(pad[0] - (freqLabelDist*0.5) + (i*freqLabelDist), pad[2] - 10, freqLabelDist, 10))
					.font_(font)
					.align_(\center)
				;
				StaticText(window, Rect(pad[0] + (i*freqLabelDist), pad[2], 1, rect.height))
					.string_("")
					.background_(scopeColor.alpha_(0.25))
				;
			});

			dbLabel.size.do({ arg i;
				dbLabel[i] = StaticText(window, Rect(0, pad[2] + (i*dbLabelDist), pad[0], 10))
					.font_(font)
					.align_(\left)
				;
				StaticText(window, Rect(pad[0], dbLabel[i].bounds.top, rect.width, 1))
					.string_("")
					.background_(scopeColor.alpha_(0.25))
				;
			});

			scope = PlusFreqScope(window, rect.moveBy(pad[0], pad[2]));
			scope.xZoom_((scope.bufSize*0.25) / width);

			setFreqLabelVals.value(scope.freqMode, 2048);
			setDBLabelVals.value(scope.dbRange);

			Button(window, Rect(pad[0] + rect.width, pad[2], pad[1], 16))
				.states_([["Power", Color.white, Color.green(0.5)], ["Power", Color.white, Color.red(0.5)]])
				.action_({ arg view;
					if(view.value == 0, {
						scope.active_(true);
					},{
						scope.active_(false);
					});
				})
				.font_(font)
				.canFocus_(false)
			;

			StaticText(window, Rect(pad[0] + rect.width, pad[2]+20, pad[1], 10))
				.string_("BusIn")
				.font_(font)
			;

			NumberBox(window, Rect(pad[0] + rect.width, pad[2]+30, pad[1], 14))
				.action_({ arg view;
					view.value_(view.value.asInteger.clip(0, Server.internal.options.numAudioBusChannels));
					scope.inBus_(view.value);
				})
				.value_(busNum)
				.font_(font)
			;

			StaticText(window, Rect(pad[0] + rect.width, pad[2]+48, pad[1], 10))
				.string_("FrqScl")
				.font_(font)
			;
			PopUpMenu(window, Rect(pad[0] + rect.width, pad[2]+58, pad[1], 16))
				.items_(["lin", "log"])
				.action_({ arg view;
					scope.freqMode_(view.value);
					setFreqLabelVals.value(scope.freqMode, 2048);
				})
				.canFocus_(false)
				.font_(font)
			// Use linear mode - not logarithmic mode - for frequency display
			//				.valueAction_(1)
			;

			StaticText(window, Rect(pad[0] + rect.width, pad[2]+76, pad[1], 10))
				.string_("dbCut")
				.font_(font)
			;
			PopUpMenu(window, Rect(pad[0] + rect.width, pad[2]+86, pad[1], 16))
				.items_(Array.series(12, 12, 12).collect({ arg item; item.asString }))
				.action_({ arg view;
					scope.dbRange_((view.value + 1) * 12);
					setDBLabelVals.value(scope.dbRange);
				})
				.canFocus_(false)
				.font_(font)
				.value_(7)
			;

			scope
				.inBus_(busNum)
				.active_(true)
				.background_(bgColor)
				.style_(1)
				.waveColors_([scopeColor.alpha_(1)])
				.canFocus_(false)
			;

			window.onClose_({
				scope.kill;
				scopeOpen = false;
			}).front;
			^super.newCopyArgs(scope, window)
		});
	}
}
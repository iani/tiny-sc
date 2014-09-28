/*
Introductory tutorial to tiny-sc

(Converted from README.org)

Sat, May 31 2014, 12:48 EEST

*/

/*
1. The "chuck operator" => sets the source of a named sound and plays it.
The source can be:
*/

//: 1.1. A function:
{ WhiteNoise.ar(0.1) } => \sound1;
//: 1.2. A reference to the name of a template holding a SynthDef
`\lpfsawr => \sound1;
//: 1.3. An event (plays like a pattern):
(degree: Pseq([0, 2, 4, 7], inf), dur: 0.1) => \sound1;

//: 2. To stop a named sound (SynthTree), send it the message fadeOut.
\sound1.fadeOut(0.05);

//: 3. To restart a sound, send it the message start
\sound1.start;

//: 4. To change a parameter of the sound, use the 'set' message
// New sound to test:
{ Formant.ar(\fundfreq.kr(440), \formfreq.kr(1760), 880) } => \sound1;

//: Changing the formant frequency
\sound1.set(\formfreq, 200);
//: Again
\sound1.set(\formfreq, 2000);
//: Change the fundamental frequeny
\sound1.set(\fundfreq, 220);
//: Change the fundamental frequeny
\sound1.set(\fundfreq, 2200);

//: Note: Open a fader interface for all SynthTrees:
SynthTree.faders;

/*
5. If the playing sound is a pattern, you can add or ovewrite exisitng
key-pattern pairs on the fly with the operator
*/

//: 5.1 Chuck a new key-pattern array to the sound
//  The patterns included in the array replace the ones with the
//  same name in the SynthTree, or are added to it.
(dur: Prand([0.01, 0.2], inf), amp: Pseq([0.01, 0.1], inf)) => \sound1;


// Replace the synth at \sound with another one
{ SinOsc.ar(440, 0, 0.1) } => \sound;
// Set the fade-in fade-out time to 0.5 seconds
\sound.fadeTime = 5;
// Replace again, fading-out the previous sound in 0.5 seconds
{ PinkNoise.ar(0.1) } => \sound;
// Change the fade-time of the SynthTree in the same line:
{ LPF.ar(GrayNoise.ar(0.1*Decay.kr(Dust.kr(10))), \freq.kr(5000)) } => \sound.fadeTime_(0.5);
// Output channel can be set
1 => ~out;
// So can the freq or any other parameter
250 => ~freq;  // Alternative syntax: \sound.set(\freq, 250)
// Fadeout with custom duration
\sound.fadeOut(12);

The environment variable ~fadeTime stores the global default duration for fade-in and fade-out of synths.  It is stored in the parent environment of the current environment.  The =!> operator is used to set environment variables of the parent environment.  Examples:

#+BEGIN_EXAMPLE
// Set global fade time to 3 seconds
10 =!> \fadeTime;
// Fade in a synth.
{ WhiteNoise.ar(0.1) } => \sound2;
// Cross-fade another synth in its place
{ SinOsc.ar(0.1 * LFNoise0.kr(5).range(400, 4000)) } => \sound2;
// And a third synth
{ LPF.ar(0.1 * GrayNoise.ar, LFNoise0.kr(5).range(400, 4000)) } => \sound2;
// Set global fade time to 0.01 seconds
0.01 =!> \fadeTime;
// Notice difference in cross-fading duration
{ LFDNoise3.ar(LFNoise0.kr(15).range(400, 4000), 0.1) } => \sound2;
// One more time
{ 0.1 * SinOsc.ar(LFNoise0.kr(15).range(400, 4000)) } => \sound2;
// Fadeout also uses the global default
\sound2.fadeOut;


/* Setting synth parameters

When a synth template gets chucked to a synthtree with =>, the parameters of that synth get pushed to the current environment.  They can then be accessed and set directly as environment variables.
*/
{ 0.1 * LPF.ar(LFSaw.ar(\freq.kr(440)), \filterFreq.kr(1000)) } => \sound3;
// Parameters of last chucked SynthTree are in the environment.
// Change the filter frequency:
400 => ~filterFreq;
// Change the generator frequency:
600 => ~freq;
// Play a routine into frequency, then fade out
{ 50 do: { 50.rrand(80).midicps => ~freq; 0.05.wait }; \sound3.fadeOut(3); }.fork;
//:
{ inf do: { 50.rrand(80).midicps => ~freq; 0.05.wait }; \sound3.fadeOut(3); }.fork;
//:
{ 10 do: { 50.rrand(80).midicps => ~freq; 0.5.wait }; \sound3.fadeOut(3); }.fork;

//: Playing patterns in single parameters

{ SinOsc.ar(\freq.kr(400), 0, 0.1) } => \sound4;
// Play a pattern into ~freq:
{ 50.rrand(80).midicps } *> \freq;
// Alternative formulation:
{ 80.rrand(90).midicps } => 0.02 *> \freq;
//: Play another pattern into freq parameter:
{ 250 rrand: 350 } => 0.05 *> \freq;
//: And another:
{ [60, 62, 66, 67].choose.midicps } => Prand([0.1, 0.2, 0.4], inf) *> \freq;
//: Shorter formulation of the above (with different paremeter values):
[70, 72, 76, 77].midicps.prand => [0.2, 0.4, 0.8].prand *> \freq;
// Pattern keeps playing when new synth is chucked into tree:
{ 0.1 * LPF.ar(LFPulse.ar(\freq.kr(440)), 1000) } => \sound4;
// Open knobs interface to watch how freq changes
~st.knobs;
//  Play with global duration stored in ~dur:
{ [65, 69, 70, 73].choose.midicps } => Pfunc({ ~dur.next }) *> \freq;
// Change global duration:
0.1 =!> \dur;
// Fade out this synth
\sound4.fadeOut;

//: Playing patterns in multiple parameters
/*
Sending an Event to a Symbol plays patterns to all parameters of the corresponding SynthTree with the same duration pattern.   Here is a sequence that also shows how to chuck SynthDef templates into the SynthTree to change the playing synth.
*/

//: Sending patterns to parameters of synths in SynthTree.
`\basshighend => \funnyStuff;
//: Play a single parameter with a duration pattern
{ 60.rrand(90).midicps }.pfunc => [0.1, 0.1, 0.2].pseq *> \freq;
//: You can change the instrument, the pattern keeps changing
`\bassfoundation => \funnyStuff;
//:
`\werkitd => \funnyStuff;
//: The pattern can be also set to play a single value
1000 *> \freq;
//: An event sets both the duration and the parameter
(dur: 0.1, freq: [1, 2, 3].pseq * 1000) *> \funnyStuff;
//: Multiple parameters can be set
(dur: 0.05, freq: (1..13).pseq * 100, amp: [0.01, 0.01, 0.2, 0.03].pseq) *> \funnyStuff

//: Change the instrument again
`\bassfoundation => \funnyStuff;
//: And again
`\organdonor => \funnyStuff;
//: And again
`\everythingrhodes => \funnyStuff;
//: Funny effects
(
(dur: [0.1 pn: 15, 0.05 pn: 30, 0.01 pn: 100].prand,
freq: (1..13).pseq * 100,
amp: [0.01, 0.01, 0.2, 0.03].pseq) *> \funnyStuff;
)
//:

//: Playing events directly in SynthTree
//: Set a longish fade time to observe cross-fading
3 =!> \fadeTime;
//: Start playing an EventPattern in a SynthTree (similar to Pbind)
(degree: [20, 22].pseq, dur: 0.5) => \test;
//: Change the pattern on the fly
(degree: [10, 11].pseq) => \test;
//: Modify a parameter in the SynthTree
(amp: [0.04, 0.04, 0.1].pseq) +> \test;
//: And nother parameter
(instrument: [\sine, \bassfoundation].prand) +> \test;
//: Change tempo, modifying the stream's event
(dur: 0.05) +> \test;
//: Wander about
(degree: Pbrown(10, 15, 1, inf)) +> \test
//: Wander about differently
(degree: Pstutter(2, Pbrown(-15, 15, 1, inf)) + [10, 11].pseq) +> \test;
//: Insert some held notes
(legato: [0.5 ! 15, 20].flat.prand) +> \test;
//: And silences
(amp: [Pfuncn({ 0 }, 40), [0.04, 0.04, 0.1].pseq(1) ! 10].flat.prand) +> \test;
//: Vary the pulse
(dur: Pbrown(0.02, 0.15, 0.01, inf)) +> \test;
//: Modify amplitude
(amp: { 0.001 exprand: 0.3 }.pfunc) +> \test;
//: Modify degree
(degree: (-20..20).prand) +> \test;
//: Use note instead of degree (non-diatonic)
(note: (-10, -9 .. 12).prand) +> \test
//: Modify duration again
(dur: 0.1) +> \test;
//: Fade-out
\test fadeOut: 5;

//: Cloning patterns and streams, propagating changes
//: Start a named pattern reference in a SynthTree
Edef(\simple, (degree: [1, 2, 3].pseq)) => \wander;

//: Change the named pattern Edef.  (Edef(\simple) =< ... also possible)
\simple =< (degree: Pbrown(-5, 5, 3, inf));
//: Modify some more
\simple =< (dur: 0.125, amp: 0.1);
//: And more
\simple =< (instrument: [\bassfoundation, \sine].prand);

//: Many keys in event OK
\simple =< (legato: { 0.4 exprand: 1.5 }.pfunc, amp: [Pn(0.01, 3), 0.05, 0.02, 0.02].pseq);

//: Modify how the synthtree interprets the stream
(degree:{if(~degree%3==0){~degree+(0..2)*[2,3].choose}{~degree}})%>\wander;

//: Add another SynthTree playing from the same stream as \wander
\wander cloneInto: \other;

//: Modify how the second SynthTree plays the stream
(degree: { 20 + ~degree }, ratio: 0.5) %> \other;

//: Like it? Lets have more:
(degree: { (17, 19 .. 23) + ~degree }) %> \other;

//: Hm, up there a little louder:
(amp: { Pbrown(0.5, 2, 0.1, inf).asStream * (~amp ? 0.01) }) %> \other;

//: Or maybe try playing inverse amplitude:
(amp: { 1 - (~amp ? 0.01) / 10 }) %> \other;

//: See if propagation works
Edef(\simple) =< (degree: Pwhite(-10.1, -12, inf));
//: OK
Edef(\simple) =< (degree: (-10..10).pyramid(5).pseq);
//: OK
Edef(\simple) =< (degree: (10..-10).pyramid(5).pseq);

//: Pauses? Suddenly a whiff of baroque.
(
(amp: [0, 0, 0.1, 0.2, 0.05].pseq, degree: { ~degree }) %> \wander;
(amp: [0, 0.1].pseq, degree: { 2 + ~degree }) %> \other;
)
//: Try some modulation
(
\simple =< (mtranspose: Pstutter(12, (-4..2).pseq),
	ctranspose: Pstutter(24, ((0..5) * 5 % 12).pseq),
	scale: Scale.harmonicMinor
);
)
//: Fade out
\wander.fadeOut(5);
\other.fadeOut(10);


//: Restart synths after Command-.

// Add some more synths to the tree:

{ SinOsc.ar(440) } => \la;
{ SinOsc.ar(550) } => \doDiese;
{ SinOsc.ar(660) } => \mi;
#+END_EXAMPLE

/* Stop all synths by typing Command-. (on SC IDE),
  or Control-c Control-s (on Emacs), or by evaluating this:
*/
thisProcess.stop;

// Then run this to restart the synths:

SynthTree.initTree;

//: Clearing SynthTree status

// To stop all synths of the SynthTree from being restarted, evaluate this:

SynthTree.stopAll;

// After this, no synths will be restarted with =SynthTree.initTree=.

// ============== Patching Audio Synth I/O ================

//: Example 1:  Simple patching of one source to one effect:

//: Start an effects synth with a low-pass filter
{ LPF.ar(Inp.ar, \freq.kr(2000)) } => \lpf;
//: Start a WhiteNoise synth
{ WhiteNoise.ar } => \source;
//: Send the noise synth to the filter
\lpf =< \source;
//: Change the frequency of the low pass filter
\lpf.set(\freq, 5000);

//: Example 2: Several synths sending to one effect.

{ LPF.ar(Inp.ar, LFNoise0.kr(40 ! 2).range(500, 4000)) } => \lpf;
\lpf =< ({ PinkNoise.ar } ==> \source);
\lpf =< ({ LFPulse.ar(LFNoise0.kr(30).range(3000, 4000)) } ==> \source2);

//: Example 3: Changing the synths of the source and of the effect:

{ LPF.ar(Inp.ar, \freq.kr(1000)) * Decay.kr({ Dust.kr(\trigRate.kr(1)) } ! 2) } =>.5 \lpf;
\lpf =< ({ LFTri.ar(LFNoise2.kr(12).range(400, 4000)) } ==> \source2);

//: Confirm that the tree can be restarted after Command-. also when it contains linked synths:

thisProcess.stop; // run this to stop all synths first
//: Then run this to restart all stopped synths:
SynthTree.initTree;
//: Modify parameters of the lpf SynthTree
\lpf.set(\trigRate, 10, \freq, 2000);

// ================= GUI Views =================
/*
** Overview: 5 View Types                                              :test:

There are 5 main view types:

- Fader View :: Vertical strip on the left, showing the currently registered SynthTree instances and their run status, with a slider for controlling the level of each instance.
- Knobs View :: Horizontal strip at the bottom, one for each SynthTree instances, with knob controls for setting all registered parameters of the SynthTree.  This is opened from the Fader view by typing "k" on a selected SynthTree strip's label.
- Synth Template View :: A window with 2 list views: The left list shows the tags (categories) of SynthTree templates (SynthDefs or Functions) and the right list shows the templates belonging to the selected category.  At the bottom is a drag view showing the name of the selected template.  Drag the selected template onto any label on the Fader view to play that template on the SynthTree belonging to that fader strip.
- Pattern Template View :: (Tentative / Under development!)  Holds templates of patterns to play in SynthTrees.  May be integrated in the same list as the Synth Template View.
- Process Registry View :: Experimental / Proof of concept: Shows a list of currently running synth and routine processes, in a manner similar to the process view of Mini Audicle in ChucK.  Works together with Emacs/Org-Mode (see keyboard shortcuts).  Can also work with SuperCollider IDE, but requires using different methods for playing Functions, Synths or Routines.  This feature is superseded by the Fader View, but kept here as proof-of-concept.
*/

//: The Synth Template View

SynthTemplate.gui;

/* Keyboard commands on Synth Template view

|----------------+-----------------------------------------------------------------+---|
| Key            | Action                                                          |   |
|----------------+-----------------------------------------------------------------+---|
| return         | send template to currently selected SynthTree instance*         |   |
| shift-return   | send template to a new SynthTree instance                       |   |
| control-return | add template as input to currently selected SynthTree instance  |   |
| control-.      | thisProcess.quit (like in SuperCollider IDE)                    |   |
| control-/      | SynthTree.initTree (restart all SynthTrees stopped by control-. |   |
|----------------+-----------------------------------------------------------------+---|

(*) Note : The currently SynthTree is selected in the Faders panel by clicking on the label displaying the SynthTree description (template name + synthtree name), or by chucking into a SynthTree in code (=>).

*/

//: The SynhTree-Fader View

SynthTree.faders;

/* Keyboard commands on SynthTree.faders

|-------+-----------------------------------------------------------+---|
| Key   | Action                                                    |   |
|-------+-----------------------------------------------------------+---|
|       | *On the whole window*                                     |   |
| b     | Open Buffer List for creating buffer-playback synth       |   |
| ,     | Stop synths and routines (=thisProcess.stop=)             |   |
| .     | Stop synths and remove from SynthTree.initTree            |   |
| i     | SynthTree.initTree.  Restart non-removed synths           |   |
| /     | SynthTree.initTree.  Restart non-removed synths           |   |
| 0-9   | Set global fade time to 0.02, 1, 2, 3 ... 9 seconds       |   |
|-------+-----------------------------------------------------------+---|
|       | *On slots that contain a SynthTree:*                      |   |
|-------+-----------------------------------------------------------+---|
| k     | Open knobs window for controlling all parameters of synth |   |
| g     | start synth                                               |   |
| s     | stop synth                                                |   |
| space | Toggle play status of selected SynthTree.                 |   |
| ,     | Stop synths and routines (=thisProcess.stop=)             |   |
| .     | Stop synths and remove from SynthTree.initTree            |   |
|-------+-----------------------------------------------------------+---|

*/

//:The Knobs view
// Creating Views for any parameter

{ SinOsc.ar(\freq.kr(440)) } => \viewtest;
\viewtest.view(\freq).view(\amp);

//: Setting fadeTime in individual synths
// Start an "effect" synth with an input
{ LPF.ar(In.ar(\in.kr(0)), \freq.kr(4000)) } => \lpf;
// Set fadeTime of effect:
\lpf.fadeTime = 10;
// Send a synth to the input of the effect synth
\lpf =< ({ WhiteNoise.ar } ==> \source);
// Set fadeTime of source;
\source.fadeTime = 5;
// change effect, with fadeTime stored previously
{ Inp.ar * Decay2.kr(Dust.kr(3)) } => \lpf;
// change source, with fadeTime stored previously
{ SinOsc.ar(2000 rrand: 3000) } ==> \source;
// change source again, With fadeTime stored previously
{ LFTri.ar(400 rrand: 800) } ==> \source;

// ============ Buffers and samples ==============

/* Play a sample loaded from disk with PlayBuf
If no name is specified, the name of the receiver of =.buf= is used to find a buffer of the same name.
If no such buffer exists, then a Dialog window is opened for choosing a file to load into a buffer.
*/

{ \buf.playBuf } => \chimes.buf.set(\amp, 1);

// Play the same sample in a different synth, with different rate

{ \buf.playBuf(rate: 1.2) } => \different.buf(\chimes).set(\amp, 1);

// BufferList autoload
/*
Setting classvar autoload of =BufferList= to =true= will make SuperCollider load all =.aiff= and =.wav= files that are found under folder =sounds= in the SuperCollider user support directory (=Platform.userAppSupportDir=) whenever the default server boots.
*/

/* Buffer list view

Following opens a Buffer List view with all buffers loaded through selecting from a SynthTree as shown above, or put in the default "sounds" folder in User App Support Dir/SuperCollider

*/


BufferList.showList;

/*
Keyboard commands on the Buffer List list view:

|--------------------+--------------------------------------------------------------|
| Key                | Action                                                       |
|--------------------+--------------------------------------------------------------|
| return             | play/stop selected buffer in a SynthTree named as the buffer |
| shift-return       | like return, but set loop to 1 (loop buffer)                 |
| space, shift-space | like return, but always create new SynthTree to play in      |
| l                  | load a new buffer from file                                  |
| s                  | save list of loaded buffers to file                          |
| o                  | load list of buffers from file                               |
|--------------------+--------------------------------------------------------------|

*/

/* ============= Keyboard Bindings =============

Following keyboard bindings only apply to Emacs.

** Global key bindingsg

- H-c l :: sclang-start
- C-c C-x C-/ :: sclang-init-synth-tree

Following keyboard shortcuts allow one to choose a synthtree from the list of synthtrees currently loaded in SuperCollider, or operate on the last chosen synthtree in emacs:

- H-c c :: org-sc-select-synthtree-then-chuck
- H-c H-c :: org-sc-chuck-into-last-synthtree
- H-c k :: org-sc-select-synthtree-then-knobs
- H-c space :: org-sc-toggle-synthtree
- H-c H-space :: org-sc-toggle-section-synthtree
- H-c g :: org-sc-start-synthtree
- H-c s :: org-sc-stop-synthtree
- H-c H-s :: org-sc-stop-last-synthtree

The chuck commands (=H-c c=, =H-c H-c=) enclose the snippet or section into a function before chucking.  Try for example =H-c c= placing the cursor in the following line of code in sclang-mode:
*/

//:
SinOsc.ar(\freq.kr(800) * LFNoise0.kr(12).range(0.8, 1.2));
//:

// Stop the example above by typing =H-c H-space=.
/*
Following keyboard shortcuts select a buffer from the list of buffers currently loaded in SuperCollider, or operate on the buffer list:

- H-b g :: org-sc-play-buffer
- H-b l :: org-sc-load-buffer
- H-b f :: org-sc-free-buffer
- H-b L :: org-sc-show-buffer-list
- H-b o :: org-sc-open-buffer-list
- H-b s :: org-sc-save-buffer-list

*/

/* ======== Org-mode bindings =========

*** General org-mode bindings for SuperCollider
- C-c C-s :: sclang-main-stop
- H-C-o :: org-sc-toggle-mode
*** Evaluating org-mode sections in sclang
**** New - in progress

2 types of movement x 3 choices of what to do with section = 12 commands.

add choices of what to do without moving: = 14 commands

- 2 types of movement ::
  1. Move to immediately next/previous section
  2. Move to next/previous section at same level

- 3 choices of actions ::
  1. Do nothing (possibly: select current section as region)
  2. Evaluate heading or contents of current section
     If current section heading begins with "!", then evaluate heading.
     Else evaluate contents of section.
  3. Wrap selection in { } and chuck it into synthtree.
     See 2 above regarding what is evaluated.
     Name of synthtree to be chucked to can be given as tag or as value of property =SYNTHREE_NAME=.  If no name is given, then the expression is chucked into the current synthtree (=~st=).

- 2 choices of what to do without moving:
  1. evaluate this section
  2. chuck this section

- =H-C-space= :: evaluate this section
- =H-M-space= :: chuck this section
- =H-n= :: next section
- =H-C-n= :: next section + evaluate
- =H-M-n= :: next section + chuck
- =H-p= :: previous section
- =H-C-p= :: previous section + evaluate
- =H-M-p= :: previous section + chuck
- =H-j= :: next section at same level
- =H-C-j= :: next section at same level + evaluate
- =H-M-j= :: next section at same level + chuck
- =H-k= :: previous section at same level
- =H-C-k= :: previous section at same level + evaluate
- =H-M-k= :: previous section at same level + chuck

* From alpha02
Note: The process registry window and the org-sc-eval-in-routine technique is now superseded by SynthTree and its guis.  SynthTree Fader gui is a more convenient way to control running synths.  The process registry is nevertheless kept here as mere "proof of concept", imitating the MiniAudicle process list window of ChucK.

  - H-C-r :: sclang-process-registry-gui: Open registry gui.
  - C-M-x :: org-sc-eval
  - H-C-x :: org-sc-eval-in-routine.  Wraps code in routine and registers it in ProcessRegistry.
  - C-M-z :: org-sc-stop-section-processes.  Stop all processes started from the current section.  Uses automatically generated section ID to identify the current section.
  - H-C-z :: org-sc-stop-section-processes
  - C-c C-M-. :: org-sc-stop-section-processes
  - H-C-n :: org-sc-next-section
  - C-M-n :: org-sc-eval-next.  Go to next section and evaluate as in org-sc-eval.
  - H-C-p :: org-sc-previous-section
  - C-M-p :: org-sc-eval-previous
  - C-c C-, :: sclang-eval-line
  - C-c C-9 :: sclang-eval-dwim
  - C-c C-x l :: org-sc-toggle-autoload
  - C-c C-x C-l :: org-sc-load-marked

*** Examples for evaluating in orgmode

Before evaluating the following sections, type =H-C-r= to open the Process Registry window.  This displays the currently running processes.   Selecting a process and typing delete will stop or free that process.

**** A sine, 3 frequencies
:PROPERTIES:
:ID:       0BDDB708-1BFE-45F0-8B50-2340127918F1
:eval-id:  11
:END:
// Type C-M-x with the cursor in the current section

a = { SinOsc.ar(\freq.kr(440), 0, 0.1) }.pla;
0.1.wait;
a.set(\freq, 550);
0.1.wait;
a.set(\freq, 660);
a release: 3;

**** Sine, simple loop
:PROPERTIES:
:ID:       0BDDB708-1BFE-45F0-8B50-2340127918F1
:eval-id:  13
:END:
// Type C-M-x with the cursor in the current section

a = { SinOsc.ar(\freq.kr(440), 0, 0.1) }.pla;
7 do: {
	0.1.wait;
	a.set(\freq, 550);
	0.1.wait;
	a.set(\freq, 660);
};
a release: 3;

**** Sine, random melody loop
:PROPERTIES:
:ID:       0BDDB708-1BFE-45F0-8B50-2340127918F1
:eval-id:  22
:END:
// Type C-M-x with the cursor in the current section

a = { SinOsc.ar(\freq.kr(440), 0, 0.1) }.pla;
50 do: {
	0.1.wait;
	a.set(\freq, (440 * (4..12).choose / 4).postln);
};
a release: 3;
3.wait;
"DONE!".postln;

**** Wandering dense sine cluster
:PROPERTIES:
:ID:       5869D44F-B75A-4713-9E9C-C5A7C6CFBFA1
:eval-id:  2
:END:
// Watch the registry window tracking 1 to 30 rapidly changing synths
// Kill the routine by selecting it in the registry window and
// hitting the backspace key.
// Then kill any remaining synths one by one with the backspace key

var synths, fwalk, swalk, synth;
synths = List();
fwalk = (Pbrown(30, 90, 0.75, inf) + Pfunc({ 0.01.exprand(1.5)})).asStream;
swalk = Pbrown(0, 30, 1, inf).asStream;
loop {
	if (swalk.next > synths.size) {
		synths add: Syn("adsrsine", [\freq, fwalk.next.midicps]);
	}{
		synth = synths.choose;
		synth.release(1.0.exprand(5.0));
		synths remove: synth;
	};
	0.05.wait;
};

**** Ghost voices
:PROPERTIES:
:ID:       F6420F84-E198-4469-9788-7119EB29CA00
:eval-id:  3
:END:
*/
//:
var synths, fwalk, swalk, synth;
synths = List();
fwalk = (Pbrown(30, 90, 0.75, inf) + Pfunc({ 0.01.exprand(1.5)})).asStream;
swalk = Pbrown(0, 30, 1, inf).asStream;
loop {
	if (swalk.next > synths.size) {
		synths add: Syn("adsrringz", [\freq, fwalk.next.midicps, \decayTime, 3, \amp, 0.02]);
	}{
		synth = synths.choose;
		synth.release(1.0.exprand(5.0));
		synths remove: synth;
	};
	0.05.wait;
};

/* Sclang-mode bindings

- H-C-o :: org-sc-toggle-mode
- C-c .  :: sclang-execute-current-snippet
- C-c C-, :: sclang-eval-line
- C-c C-. :: sclang-select-snippet
- C-M-x :: sclang-execute-current-snippet
- C-M-f :: sclang-goto-next-snippet
- C-M-b :: sclang-goto-previous-snippet
- C-M-n :: sclang-execute-next-snippet
- C-M-p :: sclang-execute-previous-snippet
- C-H-f :: sclang-goto-next-snippet
- C-H-b :: sclang-goto-previous-snippet
- C-H-n :: sclang-execute-next-snippet
- C-H-p :: sclang-execute-previous-snippet
- C-H-r :: sclang-process-registry-gui
- C-c l :: sclang-recompile
- M-C :: sclang-clear-post-buffer
*/

/* Triggering synths in routines
:PROPERTIES:
:DATE:     <2014-03-17 Mon 17:58>
:ID:       BD7795E1-1FAD-418D-8CF8-6A48925DA909
:eval-id:  18
:END:

Use =|> to set the source of a SynthTree without starting it.  This is necessary in cases like the following, where the starting of the synth is done explicitly by trig in a routine:
*/

//:
{
	var synth;
	synth = { [SinOsc, LFPulse, LFTri, LFSaw].choose.ar(\freq.kr(400)) } =|> \test;
	50 do: {
		synth.trig(\freq, 400 rrand: 1200);
		0.1.wait;
	}
}.fork
//:

// ======== UGen shortcuts ======

// =Inp.ar=

{ Inp.ar * Decay2.kr({ Dust.kr(1) } ! 2, 0.5, 2) } => \smooth;
\smooth =< ({ GrayNoise.ar(3) } ==> \gray);

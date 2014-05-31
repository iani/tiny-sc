// Possibly for  README opening examples
// Demonstrating Edef capabilities incrementally
// Tue, Apr 22 2014, 13:27 EEST


// 0. Play a pattern
Edef(\x, (degree: (0..8).pseq)).play;

// 1. Madify contents by replacing or adding patterns.
// Modify degree.  The new pattern for degree replaces the previous one.
Edef(\x).set(\degree, (0..4).pseq);

// Replace degree pattern and add pattern for dur.
Edef(\x).set((degree: (5..1).pseq, dur: 0.1));

// ================================================================

// 2. Modify a pattern based on its previous contents

Edef(\x).set((degree: { ~degree * 2 }));

// ================================================================

// 3. All spawned processes inherit changes
Edef(\x).play;  // spawn new player process

// Both processes inherit changes:
Edef(\x).set((degree: { Pstutter(3, ~degree) + [0, 1, 0].pseq }));

// ================================================================

// 4. Modify playing process - shadowing or inheriting modifications from parent

a = Edef(\y, (degree: (0..8).pseq)).play;

// Changes in the parent Edef will be inherited
a.set((degree: { [0, 1, 0].pseq * Pstutter(3, ~degree) }, dur: { ~dur / 3} );

// Test the inheritance of changes in parent Edef
Edef(\y).set((degree: [1, 3, 5, 3].pseq));

// Clone a new playing process
b = Edef(\y).play;

// This will shadow the parent pattern and disable inheritance.
b.set((degree: (-10..10).pseq);

// Test: "a" inherits while "b" does not inherit
Edef(\y).set((degree: [0, 2, 4, 0, 8, 7, 8, 5].pseq));

//: ================================================================ 
/*

Next: 

Edef().setClear(...)

Edef(\y).clone(\z)

Furthermore:
*/

Edef(\parallelDrunkard, (degree: Pbrown(-5, 10, 1, inf)) => \anotherSynthtree;

\parallelDrunkard => \parralelTree;

// The %> operator applies the filter to the event received from the stream player
(degree: { ~degree + 2 }) %> \parallelTree;

//: More sophisticated stuff is possible: 
(degree: { if (~degree.odd) {[0, 2] + ~degree },  { [-1, 3, 5] + ~degree }},
strum: { if (~degree.odd) { 0 } { 0.05 }}) 
%> \parallelTree;

//: ================================================================
// Next: =<, =<< etc. operators.

\pat1 =< (degree: Pbrown(-5, 10, 1, inf)) => \synthtree3;



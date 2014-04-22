// Possibly for  README opening examples
// Demonstrating Mdef capabilities incrementally
// Tue, Apr 22 2014, 13:27 EEST


// 1. Modify the initial pattern

Edef(\x, (degree: (0..8).pseq)).play;

// Modify degree
Edef(\x).set((degree: (0..4).pseq);

// Modify degree and add pattern for dur.
Edef(\x).set((degree: (5..1).pseq, dur: 0.1));

// ================================================================

// 2. Modify the initial pattern based on itself

Edef(\x).set((degree: { ~degree * 2 }));

// ================================================================

// 3. All spawned processes inherit changes
Edef(\x).play;  // spawn new player process

// Both processes inherit changes:
Edef(\x).set((degree: { Pstutter(3 ~degree) + [0, 1, 0].pseq }));

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
// Older stuff: 

/*
Some more specs that are used.
*/

Spec.specs.at(\amp).default = 0.1;
Spec.specs.[\trigRate] = [0.1, 50, 'exp', 0, 1, ""].asSpec;
Spec.specs.[\filterFreq] = [20, 20000, 'exp', 0, 1000, "hz"].asSpec;


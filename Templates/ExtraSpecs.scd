/*
Some more specs that are used.
*/

Spec.specs.at(\amp).default = 0.1;
Spec.specs.[\trigRate] = [0.1, 50, 'exp', 0, 1, ""].asSpec;
Spec.specs.[\filterFreq] = [20, 20000, 'exp', 0, 1000, "hz"].asSpec;
Spec.specs.[\lforate] = [0.1, 30, 'exp', 0, 7, "hz"].asSpec;
Spec.specs.[\lfowidth] = [0.0, 2, 'linear', 0, 0.1, "mul"].asSpec;
Spec.specs.[\cutoff] = \freq.asSpec;
Spec.specs.[\cutoff2] = \freq.asSpec;
Spec.specs.[\rq] = [0.001, 10, 'exp', 0, 0.5].asSpec;
Spec.specs.[\gain] = [0.0, 5, 'linear', 0, 2.0].asSpec;
Spec.specs.[\lagamount] = [0.0, 5, 'linear', 0, 0.01].asSpec;
Spec.specs.[\fenvamount] = [0.0, 5, 'linear', 0, 0.5].asSpec;
Spec.specs.[\ringtime] = [0.001, 30, 'exp', 0, 10.0].asSpec;
Spec.specs.[\releasetime] = [0.001, 30, 'exp', 0, 1.0].asSpec;
Spec.specs.[\distortion] = [0.001, 5, 'exp', 0, 0.1].asSpec;
Spec.specs.[\lagTime] = [0.001, 5, 'exp', 0, 0.1].asSpec;
Spec.specs.[\reduction] = [0.0, 30, 'linear', 0, 8.0].asSpec;
Spec.specs.[\ratio] = [0.0, 30, 'linear', 0, 3.0].asSpec;
Spec.specs.[\gain] = [0.0, 5, 'linear', 0, 2.0].asSpec;
Spec.specs.[\lagTime] = [0.001, 5, 'exp', 0, 0.05].asSpec;


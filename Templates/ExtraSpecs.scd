/*
Some more specs that are used.
*/

Spec.specs.at(\amp).default_(0.1).maxval_(3);
Spec.specs[\trigRate] = [0.1, 50, 'exp', 0, 1, ""].asSpec;
Spec.specs[\filterFreq] = [20, 20000, 'exp', 0, 1000, "hz"].asSpec;
Spec.specs[\lforate] = [0.1, 30, 'exp', 0, 7, "hz"].asSpec;
Spec.specs[\lfowidth] = [0.0, 2, 'linear', 0, 0.1, "mul"].asSpec;
Spec.specs[\cutoff] = \freq.asSpec;
Spec.specs[\cutoff2] = \freq.asSpec;
Spec.specs[\rq] = [0.001, 10, 'exp', 0, 0.5].asSpec;
Spec.specs[\gain] = [0.0, 5, 'linear', 0, 2.0].asSpec;
Spec.specs[\lagamount] = [0.0, 5, 'linear', 0, 0.01].asSpec;
Spec.specs[\fenvamount] = [0.0, 5, 'linear', 0, 0.5].asSpec;
Spec.specs[\ringtime] = [0.001, 30, 'exp', 0, 10.0].asSpec;
Spec.specs[\releasetime] = [0.001, 30, 'exp', 0, 1.0].asSpec;
Spec.specs[\distortion] = [0.001, 5, 'exp', 0, 0.1].asSpec;
Spec.specs[\lagTime] = [0.001, 5, 'exp', 0, 0.1].asSpec;
Spec.specs[\reduction] = [0.0, 30, 'linear', 0, 8.0].asSpec;
Spec.specs[\ratio] = [0.0, 30, 'linear', 0, 3.0].asSpec;
Spec.specs[\gain] = [0.0, 5, 'linear', 0, 2.0].asSpec;
Spec.specs[\lagTime] = [0.001, 5, 'exp', 0, 0.05].asSpec;
Spec.specs[\magbelow] = [0.000001, 10, 'exp', 0, 10].asSpec;
Spec.specs[\magabove] = [0.000001, 3, 'linear', 0, 0.000001].asSpec;
Spec.specs[\pos] = \pan.asSpec;
Spec.specs[\formfreq] = \freq.asSpec;
Spec.specs[\fundfreq] = \freq.asSpec;
Spec.specs[\bwfreq] = \freq.asSpec;
Spec.specs[\freq1] = \freq.asSpec;
Spec.specs[\freq2] = \freq.asSpec;

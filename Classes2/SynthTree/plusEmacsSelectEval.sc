/*

selectEval: 

Send Emacs a list of elements to select from, format it with a format string,
and evaluate the resulting SC expression in sclang.

selectEvalSnippet: 

Like selectEval, but additionally use current snippet or org-mode section
to format the sclang expression.

IZ Tue, Mar 25 2014, 11:27 EET

*/

+ Emacs {
	*selectEval { | list, formatString, prompt, requireMatch |
		/* Select a string from a list passed from sclang, 
			get the current snippet or org-mode section, 
			compose and run an sclang expression from these two using a format string.
		*/
		this.evalLispExpression(
			['org-sc-select-eval', 
				[\quote, list collect: _.asString], 
				formatString, prompt, requireMatch
			].asLispString
		)
	}

	*selectEvalSnippet { | list, formatString, prompt, requireMatch |
		/* Select a string from a list passed from sclang, 
			get the current snippet or org-mode section, 
			compose and run an sclang expression from these two using a format string.
		*/
		this.evalLispExpression(
			['org-sc-select-eval-snippet', 
				[\quote, list collect: _.asString], 
				formatString, prompt, requireMatch
			].asLispString
		)
	}
}
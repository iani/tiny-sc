;;; sc-snippets.el --- SHORTCUTS FOR EVALUATING REGIONS OF SC CODE

;;; COMMENTARY:
;;; Define keyboard shortcuts in sclang-mode for evaluating regions of
;;; code contained between two lines with comments like this //:

;;; CODE:

;; Modify sclang-eval-string to provide process-monitoring a-la Audicle.
;; To work, this file must be loaded after sclang.el.

(defcustom sclang-register-processes t
  "Non-nil means use ProcessRegistry to monitor processes a-la Audicle."
  :type 'boolean
  :group 'sclang)

(defun sclang-eval-string (string &optional print-p)
  "Send STRING to the sclang process for evaluation and print the result
if PRINT-P is non-nil. Return STRING if successful, otherwise nil."
  (sclang-send-string
   (if print-p sclang-token-interpret-print-cmd-line sclang-token-interpret-cmd-line)
   (if sclang-register-processes
       (concat
        "var sclang_evaluation_result;
sclang_evaluation_result = { \n"
        string
        "\n }.value;
ProcessRegistry.register(sclang_evaluation_result);
sclang_evaluation_result; ")
     string)))


(defun sclang-execute-current-snippet ()
  "Evaluate region between //: comments in sclang."
  (interactive)
  (save-excursion
    (let ((here (point))
          (blockstart (re-search-backward "^//:" nil t))
          (blockend))
      (if (not blockstart) (setq blockstart 0))
      (set-mark blockstart)
      (goto-char here)
      (setq blockend (re-search-forward "^//:" nil t))
      (if (not blockend) (setq blockend (point-max)))
      (goto-char blockend)
      (sclang-eval-region)
      (set-mark nil))))

(defun sclang-goto-next-snippet ()
  "Go to the next region delimited with //: comment line."
  (interactive)
  (let ((nextsnippet (re-search-forward "^//:" nil t)))
  (if (not nextsnippet) (setq nextsnippet (point-max)))
  (goto-char nextsnippet)
  (next-line)))

(defun sclang-goto-previous-snippet ()
  "Go to the preceding region delimited with //: comment line."
  (interactive)
  (let ((previoussnippet (re-search-backward "^//:" nil t)))
  (if (not previoussnippet) (setq previoussnippet (point-min)))
  (goto-char previoussnippet))
  (previous-line))

(defun sclang-execute-previous-snippet ()
  "Go to the previous sclang snippet and evaluate it."
  (interactive)
  (sclang-goto-previous-snippet)
  (sclang-execute-current-snippet))

(defun sclang-execute-next-snippet ()
  "Go to the next sclang snippet and evaluate it."
  (interactive)
  (sclang-goto-next-snippet)
  (sclang-execute-current-snippet))

(defun sc-snippets ()
  "Define sclang mode keys for snippets."
  (local-set-key (kbd "C-c .") 'sclang-execute-current-snippet)
  (local-set-key (kbd "C-M-x") 'sclang-execute-current-snippet) ;; alternative
  (local-set-key (kbd "C-M-f") 'sclang-goto-next-snippet)
  (local-set-key (kbd "C-M-b") 'sclang-goto-previous-snippet)
  (local-set-key (kbd "C-M-n") 'sclang-execute-next-snippet)
  (local-set-key (kbd "C-M-p") 'sclang-execute-previous-snippet)
  ;; alternatives using Control+Function-key:
  (local-set-key (kbd "C-H-f") 'sclang-goto-next-snippet)
  (local-set-key (kbd "C-H-b") 'sclang-goto-previous-snippet)
  (local-set-key (kbd "C-H-n") 'sclang-execute-next-snippet)
  (local-set-key (kbd "C-H-p") 'sclang-execute-previous-snippet)
  ;; additional key for convenience: provide SC-IDE shortcut for clearing buffer:
  (local-set-key (kbd "M-C") 'sclang-clear-post-buffer))

;; add to sclang-mode-hook
(add-hook 'sclang-mode-hook 'sc-snippets)

;;; sc-snippets.el ends here

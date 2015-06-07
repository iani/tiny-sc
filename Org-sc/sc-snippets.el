;;; sc-snippets.el --- SHORTCUTS FOR EVALUATING REGIONS OF SC CODE

;;; COMMENTARY:
;;; Define keyboard shortcuts in sclang-mode for evaluating regions of
;;; code contained between two lines with comments like this //:

;;; CODE:

(defun sclang-get-current-snippet ()
  "Return region between //: comments in sclang, as string,
for evaluation after processing in Emacs."
  (save-excursion
    (let (region
          (here (point))
          (blockstart (re-search-backward "^//:" nil t))
          (blockend)
          (found-block-end t))
      (if (not blockstart) (setq blockstart 0))
      (set-mark blockstart)
      (goto-char here)
      (setq blockend (re-search-forward "^//:" nil t))
      (when (not blockend)
        (setq blockend (point-max))
        (setq found-block-end nil))
      (goto-char blockend)
      (if found-block-end (beginning-of-line))
      (setq region (buffer-substring-no-properties (mark) (point)))
      (set-mark nil)
      region)))

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

(defun sclang-duplicate-current-snippet ()
  "Paste a copy of the region between //: comments as new snippet."
  (interactive)
  (save-excursion
    (let ((here (point))
          (blockstart (re-search-backward "^//:" nil t))
          (blockend))
      (when (not blockstart)
        (setq blockstart 1)
        (goto-char 1)
        (insert "//:\n"))
      (set-mark blockstart)
      (goto-char here)
      (setq blockend (re-search-forward "^//:" nil t))
      (if blockend
          (setq blockend (- blockend 3))
        (progn
          (insert "\n")
          (setq blockend (point-max))))
      (goto-char blockend)
      (copy-region-as-kill blockstart blockend)
      (set-mark nil)
      (yank)))
  (sclang-goto-next-snippet)
  (set-mark nil))


(defun sclang-chuck-current-snippet ()
  "Evaluate region between //: comments in sclang."
  (interactive)
  (sclang-eval-string
   (format "{ %s } => (~st ?? { \\st0.asSynthTree });"
           (sclang-get-current-snippet))))

(defun sclang-goto-next-snippet ()
  "Go to the next region delimited with //: comment line."
  (interactive)
  (let ((next-snippet (re-search-forward "^//:" nil t)))
  (if (not next-snippet) (setq next-snippet (point-max)))
  (goto-char next-snippet)
  (next-line)))

(defun sclang-goto-previous-snippet ()
  "Go to the preceding region delimited with //: comment line."
  (interactive)
  (let ((previous-snippet (re-search-backward "^//:" nil t)))
  (if (not previous-snippet) (setq previous-snippet (point-min)))
  (goto-char previous-snippet))
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

(defun sclang-chuck-previous-snippet ()
  "Go to the previous sclang snippet and evaluate it."
  (interactive)
  (sclang-goto-previous-snippet)
  (sclang-chuck-current-snippet))

(defun sclang-chuck-next-snippet ()
  "Go to the next sclang snippet and evaluate it."
  (interactive)
  (sclang-goto-next-snippet)
  (sclang-chuck-current-snippet))

(defun sclang-select-snippet ()
  "Select the region between two //: comments."
  (interactive)
  (let ((here (point))
        (blockstart (re-search-backward "^//:" nil t))
        (blockend))
    (if (not blockstart) (setq blockstart 0))
    (set-mark blockstart)
    (goto-char here)
    (setq blockend (re-search-forward "^//:" nil t))
    (if (not blockend) (setq blockend (point-max)))
    (goto-char blockend)
    (beginning-of-line)))

(defun sclang-process-registry-gui ()
  "Show ProcessRegistryGui window."
  (interactive)
  (sclang-eval-string "ProcessRegistryGui.gui"))

(defun sclang-init-synth-tree ()
  "Restart all non-disabled synths in SynthTree."
  (interactive)
  (sclang-eval-string "SynthTree.initTree;"))

(defun sclang-eval-line-optionally-inspect (inspect-p)
  "Evaluate current line as sclang code.
If prefix argument given, then open inspector in SC on the result"
  (interactive "P")
  (if inspect-p
      (let ((string (sclang-line-at-point)))
        (when string
          (sclang-eval-string (format "(%s).inspect;" string) t))
        (and sclang-eval-line-forward
             (/= (line-end-position) (point-max))
             (forward-line 1))
        string)
    (sclang-eval-line)))

(defun sclang-eval-line-inspect ()
  "Evaluate current line as sclang code.
If prefix argument given, then open inspector in SC on the result"
  (interactive)
  (let ((string (sclang-line-at-point)))
    (when string
      (sclang-eval-string (format "(%s).inspect;" string) t))
    (and sclang-eval-line-forward
         (/= (line-end-position) (point-max))
         (forward-line 1))
    string))

(defun sclang-snippet-menu ()
  (interactive)
  (let* ((menu (grizzl-make-index
               '("recompile"
                 "stop"
                 "start"
                 "boot server"
                 "query all nodes")))
         (command (grizzl-completing-read "select command:" menu)))
    (call-interactively
     (intern
      (concat "sclang-" (replace-regexp-in-string " " "-" command))))))

(defun sc-snippets ()
  "Define sclang mode keys for snippets."
  (local-set-key (kbd "C-M-c") 'sclang-snippet-menu)
  (local-set-key (kbd "C-c .") 'sclang-execute-current-snippet)
  ;; For some aggravating reason, cannot un-bind C-c C-c from sclang-eval-dwim
  ;; But I do not want to modify the source code of sc-extensions
  ;; So instead:
  (local-set-key (kbd "C-c C-,") 'sclang-eval-line)
  (local-set-key (kbd "C-c C-M-,") 'sclang-eval-line-inspect)
  ;; sclang-switch-to-post does not work as expected:
  ;; (local-set-key (kbd "C-c C-M-p") 'sclang-switch-to-post)
  (local-set-key (kbd "C-c C-.") 'sclang-select-snippet)
  (local-set-key (kbd "C-M-x") 'sclang-execute-current-snippet) ;; alternative
  (local-set-key (kbd "C-M-2") 'sclang-duplicate-current-snippet)
  (local-set-key (kbd "C-M-,") 'sclang-duplicate-current-snippet) ;; alternative
  (local-set-key (kbd "C-M-f") 'sclang-goto-next-snippet)
  (local-set-key (kbd "C-M-b") 'sclang-goto-previous-snippet)
  (local-set-key (kbd "C-s-n") 'sclang-execute-next-snippet)
  (local-set-key (kbd "C-s-p") 'sclang-execute-previous-snippet)
  ;; alternatives using Control/Meta+Function-key:
  (local-set-key (kbd "H-SPC") 'org-sc-toggle-synthtree)
  (local-set-key (kbd "H-C-SPC") 'sclang-execute-current-snippet)
  (local-set-key (kbd "H-C-.") 'sclang-execute-current-snippet)
  (local-set-key (kbd "H-M-SPC") 'sclang-chuck-current-snippet)
  (local-set-key (kbd "H-M-.") 'sclang-chuck-current-snippet)
  (local-set-key (kbd "H-n") 'sclang-goto-next-snippet)
  (local-set-key (kbd "H-p") 'sclang-goto-previous-snippet)
  (local-set-key (kbd "H-C-n") 'sclang-execute-next-snippet)
  (local-set-key (kbd "H-C-p") 'sclang-execute-previous-snippet)
  (local-set-key (kbd "H-M-n") 'sclang-chuck-next-snippet)
  (local-set-key (kbd "H-M-p") 'sclang-chuck-previous-snippet)
  (local-set-key (kbd "H-C-r") 'sclang-process-registry-gui)
  ;; C-c C-l is overrriden by sc-extensions. Therefore substitute:
  (local-set-key (kbd "C-c l") 'sclang-recompile)
  ;; additional key for convenience: provide SC-IDE shortcut for clearing buffer:
  (local-set-key (kbd "M-C") 'sclang-clear-post-buffer)
  (global-set-key (kbd "C-c C-x C-/") 'sclang-init-synth-tree))

;; add to sclang-mode-hook
(add-hook 'sclang-mode-hook 'sc-snippets)

;;; sc-snippets.el ends here

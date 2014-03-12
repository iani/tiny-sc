;;; sc-snippets.el --- SHORTCUTS FOR EVALUATING REGIONS OF SC CODE

;;; COMMENTARY:
;;; Define keyboard shortcuts in sclang-mode for evaluating regions of
;;; code contained between two lines with comments like this //:

;;; CODE:

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

(defun sc-snippets ()
  "Define sclang mode keys for snippets."
  (local-set-key (kbd "C-c .") 'sclang-execute-current-snippet)
  ;; For some aggravating reason, cannot un-bind C-c C-c from sclang-eval-dwim
  ;; But I do not want to modify the source code of sc-extensions
  ;; So instead: 
  (local-set-key (kbd "C-c C-,") 'sclang-eval-line)
  ;; sclang-switch-to-post does not work as expected:
  ;; (local-set-key (kbd "C-c C-M-p") 'sclang-switch-to-post)
  (local-set-key (kbd "C-c C-.") 'sclang-select-snippet)
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
  (local-set-key (kbd "C-H-r") 'sclang-process-registry-gui)
  ;; C-c C-l is overrriden by sc-extensions. Therefore substitute:
  (local-set-key (kbd "C-c l") 'sclang-recompile)
  ;; additional key for convenience: provide SC-IDE shortcut for clearing buffer:
  (local-set-key (kbd "M-C") 'sclang-clear-post-buffer)
  (global-set-key (kbd "C-c C-x C-/") 'sclang-init-synth-tree))

;; add to sclang-mode-hook
(add-hook 'sclang-mode-hook 'sc-snippets)

;;; sc-snippets.el ends here

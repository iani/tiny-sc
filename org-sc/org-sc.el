
(defun org-sc-eval ()
  "Evaluate contents of org-mode element as SuperCollider code.
If inside a section, evaluate whole contents of section.
If inside a src block, evaluate contents of block."
  (interactive)
  (let ((element (org-element-at-point)))
    (cond ((equal (car element) 'src-block)
           (sclang-eval-string (plist-get (cadr element) :value)))
          (t
           (save-excursion
             (save-restriction
               (widen)
               (org-back-to-heading)
               (setq element (cadr (org-element-at-point)))
               (org-id-get-create)
               (search-forward ":END:")
               (end-of-line)
               (sclang-eval-string
                (concat
                 "(process_id: '"
                 (org-id-get-create)
                 "', eval_id: UniqueID.next) use: {\n"
                 (buffer-substring
                  (point)
                  (plist-get element :contents-end))
                 "\n}")
                t)))))))

(defun org-sc-eval-next ()
  "Go to next org-mode section and evaluate its contents as SuperCollider code."
  (interactive)
  (outline-next-heading)
  (org-sc-eval))

(defun org-sc-eval-previous ()
  "Go to previous org-mode section and evaluate its contents as SuperCollider code."
  (interactive)
  (outline-previous-heading)
  (org-sc-eval))

(define-key org-mode-map (kbd "C-M-x") 'org-sc-eval)
(define-key org-mode-map (kbd "C-M-n") 'org-sc-eval-next)
(define-key org-mode-map (kbd "C-M-p") 'org-sc-eval-previous)
;; this overrides the default binding org-schedule, which I do not use often:
(define-key org-mode-map (kbd "C-c C-s") 'sclang-main-stop)
(define-key org-mode-map (kbd "H-M-r") 'sclang-process-registry-gui)

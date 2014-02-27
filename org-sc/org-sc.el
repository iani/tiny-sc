
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
               (sclang-eval-string
                        (buffer-substring
                         (plist-get element :contents-begin)
                         (plist-get element :contents-end)))))))))

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

;;; lisp/props.el -*- lexical-binding: t; -*-

(defun org-entry-get-special-with-inheritance (property &optional literal-nil)
  "Same as org's `org-entry-get-with-inheritance' but for special properties"
  (move-marker org-entry-property-inherited-from nil)
  (org-with-wide-buffer
   (let (value)
     (catch 'exit
       (while t
     (let ((v (org-entry-properties nil property)))
       (cond
        (v
       (setq value (cdr (assoc-string property v)))
         (org-back-to-heading-or-point-min t)
         (move-marker org-entry-property-inherited-from (point))
         (throw 'exit nil))
        ((org-up-heading-or-point-min))
        (t
         (let ((global (org--property-global-or-keyword-value property literal-nil)))
           (cond ((not global))
             (value (setq value (concat global " " value)))
             (t (setq value global))))
         (throw 'exit nil))))))
     (if literal-nil value (org-not-nil value)))))

(defun org-entry-get (pom property &optional inherit literal-nil)
    "Same as org's `org-entry-get' but allows inheriting special properties
     and inheritance is always on for inhertied properties."
  (org-with-point-at pom
    (let ((special (member-ignore-case property (cons "CATEGORY" org-special-properties)))
          (inherit (or (equal inherit 'inherit) ;; Force inheritance
                       (org-property-inherit-p property))))
      (cond
       ((and special (not inherit))
        (cdr (assoc-string property (org-entry-properties nil property))))
       ((and special inherit)
        (org-entry-get-special-with-inheritance property literal-nil))
       ((and (not special) inherit)
        (org-entry-get-with-inheritance property literal-nil))
       (t
        (let* ((local (org--property-local-values property literal-nil))
               (value (and local (mapconcat #'identity (delq nil local) " "))))
          (if literal-nil value (org-not-nil value))))))))

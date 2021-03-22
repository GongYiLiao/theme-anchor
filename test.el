
;; 
(if (eq (car spc) 'ansi-color-names-vector)
    (setq-local ansi-color-map (ansi-color-make-color-map)))

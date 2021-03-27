


(defun theme-anchor-spec-inherit (valid-specs)
  "Get specs using inherit proporties.
This is for those faces not specified in agiven theme but have their parent
faces in the theme. This function is designed for `theme-anchor-buffer-local'
and not supposed to use separately" 
  (let* ((valid-keys (mapcar #'car valid-specs))
	 ;; get name of faces those exists but not specified in theme
	 (face-stack (cl-set-difference (face-list) valid-keys))
	 ;; This filter check the following conditions 
	 ;; 1. the face's spec is derived through :inherit property
	 ;; 2. if #1 holds, check if the face this face inherit
	 ;;    from does exist in curent buffer's face-remapping-alist
	 (inherit-specs (mapcar (lambda (fc)
				  (let ((ihr (face-attribute fc :inherit nil t)))
				    (if (and ihr
					     (not (eq ihr 'unspecified))
					     (member ihr valid-keys))
					(list fc (car (alist-get ihr valid-specs))))))
				face-stack)))
    ;; clean up `nil's
    (cl-remove nil inherit-specs)))



(defun theme-anchor-buffer-local-attempt1 (theme)
  "Extract applicable face settings from THEME.
Argument THEME the theme to be applied in the mode hook .
It uses 'face-remap-set-base' to load that theme in a buffer local manner"
  ;; make sure the theme is available, copied from custom.el's load-theme
  ;; definition 
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar #'symbol-name
				     (custom-available-themes))))))
  (unless (custom-theme-name-valid-p theme)
    (error "Invalid theme name `%s'" theme))
  ;; prepare the theme for face-remap
  (load-theme theme t t)
  ;; set the theme-values as well 
  (theme-anchor-set-values theme)
  ;; choose the most appropriate theme for the environment 
  (let* ((valid-specs
	  (cl-remove 'nil ;; filter out non-applicable specs
		     (mapcar #'theme-anchor-spec-choose
			     ;; get the theme-face specs from the theme
			     (theme-anchor-get-faces theme))))
	 (inherit-specs (theme-anchor-spec-inherit valid-specs))
	 (valid-and-inherit-specs (append valid-specs inherit-specs)))
    ;; make sure the face is set as the buffer's based face by
    ;; 1. use face-remap-se-base face `nil' to make global face ignored
    ;; 2. apply the spec as the new base for the buffer 
    (cl-map nil (lambda (spec)
		  ;; attemp to cleanup base definition, seems not usefull 
		  ;; (face-remap-set-base (car spec) nil)
		  (apply #'face-remap-set-base spec)) ;; anchor the spec as base 
	    ;; ignore faces without applicable specs
	    valid-and-inherit-specs)))

;; -*- lexical-binding: t -*- 
;; ------------------------------------------------------------------------------
;; File name:   anchor-theme-buffer-local.el
;; Purpose  :   Using `face-remap's `face-remap-set-base function to set
;;              buffer-specific custom theme
;; Created by:  Liāu, Kiong-Gē <gongyi.liao@gmail.com>
;; License:     FSF GNU GPL V3 or newer
;; ------------------------------------------------------------------------------

;; 
(require 'cl-lib)  			;; for 'cl-remove-if'
(require 'faces) 			;; for 'face-spec-choose'
(require 'custom) 			;; for 'load-theme'
(require 'face-remap) 			;; for 'face-remap-set-base'

;; 
(defun anchor-theme-get-faces (theme)
  "Extract all the theme-face values from THEME"
  ;; take only theme-face specs
  (cl-remove-if #'(lambda (spec) (not (eq (car spec) 'theme-face)))
		;; the theme's all the face/value specs
		(get theme 'theme-settings))) 

;;
(defun anchor-theme-spec-choose (face-spec)
  "Choose applicable face settings.
It uses the condition specified in a face spec and use 'face-spec-choose'
function from face-remap.el"
  ;; a face's all applicable specs, along with their applicable conditions
  (let ((face-spec-content (nth 3 face-spec)))
    (list (nth 1 face-spec) ;; the face name
	  ;; the applicable face spec chosen by 'face-spec-choose'
	  (face-spec-choose face-spec-content)))) 

;; 
(defun anchor-theme-buffer-local (theme)
  "Extract applicable face settings from THEME.
It uses 'face-remap-set-base' to load that theme in a buffer local manner"
  ;; make sure the theme is available  
  (load-theme theme t t)
  ;; set buffer face with 
  (mapc (lambda (spec) (apply #'anchor-theme-set-base spec))
	;; ignore faces without applicable specs
	(remove 'nil
		;; filter out non-applicable specs
		(mapcar #'anchor-theme-spec-choose
			;; get the theme-face specs from the theme 
			(anchor-theme-get-faces theme)))))


;; 
(defmacro anchor-theme-hook-gen (theme &rest other-step)
  "Generate hook functions"
  `(lambda nil
     ;; face-remap current buffer with theme 
     (anchor-theme-buffer-local ,theme)
     ;; other sides effect applicable to the current buffer
     ,@other-step))                	

;; 
(provide 'anchor-theme-buffer-local)

;; ------------------------------------------------------------------------------
;;   End of 'anchor-theme-buffer-local
;; ------------------------------------------------------------------------------

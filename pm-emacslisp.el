(require 'project-manager)

(pm-register-backend
 (make-pm-backend :name "emacslisp"
		  :find-file 'project-find-file-subproject))

(provide 'pm-emacslisp)

(require 'project-manager)

(defun pm-c-compile ()
  (interactive)
  (if (project- compile-fun current-project)
      (call-interactively (project-compile-fun current-project))
    (compile)))

(pm-register-backend
 (make-pm-backend :name "c"
                  :compile 'pm-c-compile
		  :find-file 'project-find-file-subproject))

(provide 'pm-c)

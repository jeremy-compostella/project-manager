(require 'cl)

(defstruct project
  name
  pm-backend
  env-vars
  subprojects
  root-path)

(defstruct pm-backend
  name
  (open-hook 'ignore)
  (close-hook 'ignore)
  (find-file 'ignore)
  (find-file-hook 'ignore)
  (search 'ignore)
  (compile 'ignore))

(defvar project-manager-switch-hook '()
  "Hook list runned when you swith from one project to
  another.")

(defvar projects '())
(defvar pm-backends '())
(defvar current-project nil)
(defvar project-env-vars '())
(defvar current-root-path nil)

(defun restore-env-vars ()
  (when project-env-vars
    (dolist (elm project-env-vars)
      (set (car elm) (cdr elm)))
    (setq project-env-vars nil)))

(defun project-backend (project)
  (unless (or project (project-p project))
    (error "Invalid project"))
  (let ((backend (find (project-pm-backend project) pm-backends
		       :test 'string= :key 'pm-backend-name)))
    (unless backend
      (error "%s backend not found" (project-pm-backend project)))
    backend))

(defun switch-project (project-name)
  (interactive (list (ido-completing-read (format "Project name%s: "
						  (if current-project
						      (concat " (" (project-name current-project) ")")
						    ""))
					  (mapcar 'project-name projects) nil t)))
  (let* ((project (find project-name projects :key 'project-name :test 'string=))
	 (backend (project-pm-backend project)))
    (when current-project
      (funcall (pm-backend-close-hook (project-backend current-project))))
    (restore-env-vars)
    (setq current-root-path (project-root-path project))
    (setq current-project project)
    (dolist (elm (project-env-vars project))
      (set (car elm) (eval (cdr elm))))
    (funcall (pm-backend-open-hook (project-backend project)))
    (run-hooks 'project-manager-switch-hook)
    (message "Successfull switched to project %s." (propertize (project-name current-project)
							       'face 'success))))

(defun project-uniquify-buffer-name (suffix)
  (let ((filename (buffer-file-name buf)))
    (rename-buffer (concat (file-name-nondirectory filename)
			   "<" (project-name current-project) ":" suffix ">"))))

(defun pm-register-backend (backend)
  (add-to-list 'pm-backends backend nil
	       (lambda (x y) (string= (pm-backend-name x)
				      (pm-backend-name y)))))

(defun register-project (project)
  (delete-if (curry 'string= (project-name project))
	     projects :key 'project-name)
  (add-to-list 'projects project nil
  	       (lambda (x y) (string= (project-name x)
  				      (project-name y)))))

(defun unregister-project (project-name)
  (interactive (list (ido-completing-read "Project name: "
					  (mapcar 'project-name projects) nil t)))
  (setq projects (delete-if (curry 'string= project-name)
			    projects :key 'project-name)))

(defun unregister-backend (backend-name)
  (interactive (list (ido-completing-read "Backend name: "
					  (mapcar 'pm-backend-name pm-backends) nil t)))
  (setq pm-backends (delete-if (curry 'string= backend-name)
                               pm-backends :key 'pm-backend-name)))

(defun project-find-file ()
  (interactive)
  (call-interactively (pm-backend-find-file (project-backend current-project))))

(defun project-find-file-subproject (&optional subprojects history)
  (interactive)
  (let* ((subprojects (or subprojects (project-subprojects current-project)))
	 (subprojects (delete-if-not 'file-exists-p subprojects
				     :key (lambda (x)
					    (concat current-root-path (eval (cdr x))))))
	 (subprojects (cons (cons "root" "/") subprojects))
	 (subproject (ido-completing-read (format "Subproject (project %s): "
						  (project-name current-project))
					  subprojects
					  nil t nil history))
	 (default-directory (concat current-root-path
				    (eval (assoc-default subproject subprojects)))))
    (ido-find-file)))

(defun project-search ()
  (interactive)
  (call-interactively (pm-backend-search (project-backend current-project))))

(defun project-compile ()
  (interactive)
  (call-interactively (pm-backend-compile (project-backend current-project))))

(defun project-smart-compile ()
  (interactive)
  (let ((proj (remove-if-not (rcurry 'string-prefix-p default-directory)
			     projects :key 'project-root-path)))
    (if (not proj)
	(if (not current-project)
	    (compile)
	  (project-compile))
      (let ((new-proj
	     (cond ((= (length proj) 1) (project-name (car proj)))
		   ((ido-completing-read "Project name: "
					 (mapcar 'project-name proj) nil t)))))
	(unless (string= new-proj (project-name current-project))
	  (switch-project new-proj))
	(project-compile)))))

(provide 'project-manager)

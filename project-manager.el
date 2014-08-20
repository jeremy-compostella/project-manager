(defstruct project
  name
  pm-backend
  env-vars
  subprojects)

(defstruct pm-backend
  name
  (open-hook 'ignore)
  (close-hook 'ignore)
  (find-file 'ignore)
  (find-file-hook 'ignore)
  (compile 'ignore))

(defvar projects '())
(defvar pm-backends '())
(defvar current-project nil)
(defvar project-env-vars '())

(defun restore-env-vars ()
  (when project-env-vars
    (dolist (elm project-env-vars)
      (set (car elm) (cdr elm)))
    (setq project-env-vars nil)))

(defun project-backend (project)
  (find (project-pm-backend project) pm-backends
	:test 'string= :key 'pm-backend-name))

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
    (dolist (elm (project-env-vars project))
      (set (car elm) (eval (cdr elm))))
    (setq current-project project)
    (funcall (pm-backend-open-hook (project-backend project)))))

(defun project-uniquify-buffer-name (suffix)
  (let ((filename (buffer-file-name buf)))
    (rename-buffer (concat (file-name-nondirectory filename)
			   "<" (project-name current-project ":" suffix ">")))))

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

(defun project-find-file ()
  (interactive)
  (call-interactively (pm-backend-find-file (project-backend current-project))))

(defun project-compile ()
  (interactive)
  (call-interactively (pm-backend-compile (project-backend current-project))))

(provide 'project-manager)

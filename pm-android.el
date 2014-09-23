(require 'project-manager)

;; External
(defvar aosp-path nil)
(defvar aosp-board-name nil)
(defvar aosp-build-variant nil)
(defvar aosp-thread-number nil)
(defvar aosp-compile-options '())
(defvar aosp-env-vars '())

;; Internal
(defconst pm-android-targets
  '(("auto"			.       pm-android-build-current)
    ("dist"			.	"dist")
    ("boot"			.	"bootimage")
    ("recovery"			.	"recoveryimage")
    ("system"			.	"systemimage")
    ("adb-fastboot"		.       "adb fastboot")
    ("Windows-adb-fastboot"	.       "adb fastboot USE_MINGW=y")
    ("interactive"		.	pm-android-interactive-target)
    ("clean"			.	pm-android-clean-build)))

(defvar pm-android-compile-history '())
(defvar pm-android-interactive-history '())
(defvar pm-android-subprojects-history '())
(defvar pm-android-compile-options-history '())

(defvar pm-android-subprojects '(("out"	.	(concat "/out/target/product/" aosp-board-name "/"))
				 ("pub"	.	(concat "/pub/" (upcase aosp-board-name) "/"))))
  
(defsubst pm-android-device ()
  (concat aosp-board-name "-" aosp-build-variant))

(defun pm-android-hostname (&optional dir)
  (let ((dir (or dir default-directory)))
    (or (and (tramp-tramp-file-p dir)
	     (with-parsed-tramp-file-name dir info
	       info-host))
	"localhost")))

(defsubst pm-android-load-compile-env ()
  (concat (when aosp-env-vars
	    (concat "export "
		    (mapconcat (lambda (x) (format "%s=%S" (car x) (eval (cdr x))))
			       aosp-env-vars " ")
		    " && "))
	  "source build/envsetup.sh && "
	  "lunch " (pm-android-device) " && "))

(defun pm-android-compile (target)
  (interactive (list (ido-completing-read (format "Build target (%s:%s on %s): "
						  (project-name current-project)
						  (pm-android-device) (pm-android-hostname))
					  (mapcar 'car pm-android-targets)
					  nil t nil 'pm-android-compile-history)))
  (let ((t-or-f (assoc-default target pm-android-targets)))
    (if (functionp t-or-f)
	(call-interactively t-or-f)
      (pm-android-build-target t-or-f))))

(defun pm-android-clean-build ()
  (interactive)
  (when (y-or-n-p "Are you sure you want to clean all your repo ?")
    (pm-android-build-target "clean")))

(defun pm-android-build-current ()
  (interactive)
  (let* ((module-dir (untramp-path default-directory))
	 (default-directory (concat aosp-path "/")))
    (compile (concat (pm-android-load-compile-env)
		     (format "cd %s && mm -j%d" module-dir aosp-thread-number)
		     (if aosp-compile-options
			 (mapconcat 'identity aosp-compile-options " ")
		       "")))))

(defun pm-android-interactive-target (target)
  (interactive (list (read-string (format "Target (default: %s): "
					  (or (car pm-android-interactive-history) ""))
				  nil 'pm-android-interactive-history
				  (car pm-android-interactive-history))))
  (pm-android-build-target target))

(defun pm-android-build-target (target)
  (let ((default-directory (concat aosp-path "/")))
    (compile (concat (pm-android-load-compile-env)
		     "make -j" (number-to-string aosp-thread-number) " "
		     (if aosp-compile-options
			 (mapconcat 'identity aosp-compile-options " ")
		       "")
		     " " target))))

(defun pm-android-subproject ()
  (let ((cur-path (expand-file-name default-directory)))
    (while (not (file-exists-p (concat cur-path "/.git")))
      (setq cur-path (expand-file-name (concat cur-path "/.."))))
    (substring cur-path (length aosp-path))))

(defun pm-android-find-file-hook ()
  (let ((file-name (buffer-file-name)))
    (when (and file-name
	       (file-exists-p file-name)
	       (file-writable-p file-name)
	       (string-prefix-p (aosp-path) file-name))
      (project-uniquify-buffer-name (pm-android-subproject)))))

(defun pm-android-find-file ()
  (interactive)
  (project-find-file-subproject (append (project-subprojects current-project)
					pm-android-subprojects)
				'pm-android-subprojects-history))

(defun pm-android-toggle-command (cmd)
  (interactive (list (read-string "Compilation option: " nil
				  'pm-android-compile-options-history)))
  (let ((msg-fmt "Compilation option \"%s\" %s."))
  (if (find cmd aosp-compile-options :test 'string=)
      (progn (setq aosp-compile-options (delete cmd aosp-compile-options))
	     (message (propertize (format msg-fmt cmd "deactivated") 'face 'error)))
    (add-to-list 'aosp-compile-options cmd)
    (message (propertize (format msg-fmt cmd "activated") 'face 'success)))))
  
(defun pm-android-toggle-showcommands ()
  (interactive)
  (pm-android-toggle-command "showcommands"))

(defun pm-android-open-hook ()
  (message "pm-android-open-hook called")
  (setq aosp-path current-root-path))

(pm-register-backend
 (make-pm-backend :name "android"
		  :open-hook 'pm-android-open-hook
		  :find-file 'pm-android-find-file
		  :find-file-hook 'pm-android-find-file-hook
		  :compile 'pm-android-compile))

(provide 'pm-android)

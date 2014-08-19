(require 'project-manager)

;; External
(defvar aosp-path nil)
(defvar aosp-board-name nil)
(defvar aosp-build-variant nil)
(defvar aosp-thread-number nil)
(defvar aosp-compile-options "")
(defvar aosp-env-vars '())

;; Internal
(defconst pm-android-targets
  '(("auto"			.       pm-android-build-current)
    ("flashfiles"		.	"flashfiles")
    ("boot"			.	"bootimage")
    ("recovery"			.	"recoveryimage")
    ("adb-fastboot"		.       "adb fastboot")
    ("Windows-adb-fastboot"	.       "adb fastboot publish_windows_tools USE_MINGW=y")
    ("interactive"		.	pm-android-interactive-target)
    ("clean"			.	pm-android-clean-build)))

(defvar pm-android-compile-history '())
(defvar pm-android-interactive-history '())
(defvar pm-android-subprojects-history '())

(defvar pm-android-subprojects '(("out"	.	(concat "/out/target/product/" aosp-board-name "/"))
				 ("pub"	.	(concat "/pub/" (upcase aosp-board-name) "/"))))
  
(defsubst pm-android-device ()
  (concat aosp-board-name "-" aosp-build-variant))

(defsubst pm-android-load-compile-env ()
  (concat (when aosp-env-vars
	    (concat "export "
		    (mapconcat (lambda (x) (format "%s=%S" (car x) (eval (cdr x))))
			       aosp-env-vars " ")
		    " && "))
	  "source build/envsetup.sh && "
	  "lunch " (pm-android-device) " && "))

(defun pm-android-compile (target)
  (interactive (list (ido-completing-read (format "Build target (%s): " (pm-android-device))
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
		     aosp-compile-options " "
		     target))))

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

(defun pm-android-find-file (&optional subproject)
  (interactive)
  (let* ((subprojects (append (project-subprojects current-project)
			      pm-android-subprojects))
	 (subprojects (delete-if-not 'file-exists-p subprojects
				     :key (lambda (x) (concat aosp-path (eval (cdr x))))))
	 (subproject (or subproject
			 (ido-completing-read (format "Subproject (project %s): "
						      (project-name current-project))
					      subprojects
					      nil t nil 'pm-android-subprojects-history)))
	 (default-directory (concat aosp-path (eval (assoc-default subproject subprojects)))))
    (ido-find-file)))

(pm-register-backend
 (make-pm-backend :name "android"
		  :find-file 'pm-android-find-file
		  :find-file-hook 'pm-android-find-file-hook
		  :compile 'pm-android-compile))

(provide 'pm-android)

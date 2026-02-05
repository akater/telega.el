;; What follows is a ripoff from elephant.asd,
;; with “elephant” replaced with “telega”.
;; Requires config.sexp and may complain about my-config.sexp

(cl:defpackage telega-system
  (:use :cl :asdf)
  (:export :telega-c-source :compiler-options :foreign-libraries-to-load-first :get-config-option))

(cl:in-package :telega-system)

;;
;; Simple lisp/asdf-based make utility for telega c files
;;

(defgeneric compiler-options (compiler c-source-file &key input-file output-file)
  (:documentation "Returns a list of options to pass to <compiler>"))

(defgeneric foreign-libraries-to-load-first (c-source-file)
  (:documentation "Provides an alist of foreign-libraries to load and the modules to load them into.  Similar to (input-files load-op), but much more specific"))

(defvar *loaded-foreign-libraries* ()
  "Unfortunately UFFI lacks functionality to track which libraries
are already loaded so we have to resort to a crude emulation.")

(defun uffi-funcall (fn &rest args)
  "Simplify uffi funcall, first ensure uffi is loaded"
  (unless (find-package :uffi)
    (asdf:operate 'asdf:load-op :uffi))
  (let ((result (apply (find-symbol (symbol-name fn) (symbol-name :uffi)) args)))
    (when (eq fn :load-foreign-library)
      (pushnew (getf (rest args) :module) *loaded-foreign-libraries* :test 'equal))
    result))

;;
;; User parameters (bdb root and pthread, if necessary)
;;

(defun get-config-option (option component)
  (let ((filespec (make-pathname :defaults (asdf:component-pathname (asdf:component-system component))
				 :name "my-config"
				 :type "sexp"))
	(orig-filespec (make-pathname :defaults (asdf:component-pathname (asdf:component-system component))
				 :name "config"
				 :type "sexp")))
    (unless (probe-file filespec)
      (with-simple-restart (accept-default "Create default settings for my-config.sexp and proceed.")
	(warn "Missing configuration file: my-config.sexp.  Please copy config.sexp to my-config.sexp and customize for your local environment.  Otherwise Telega will not load."))
      (with-open-file (src orig-filespec :direction :input)
	(with-open-file (dest filespec :direction :output)
	  (write (read src) :stream dest))))
    (with-open-file (config filespec)
      (cdr (assoc option (read config))))))

;;
;; Supported C compilers
;;


(defvar *c-compilers*
  '((:gcc . "gcc")
    (:cygwin . "gcc")
    (:msvc . ""))
  "Associate compilers with platforms for compiling libmemutil/libsleepycat")

(defun c-compiler (comp)
  (get-config-option :compiler comp))

(defun c-compiler-path (comp)
  (let* ((compiler (get-config-option :compiler comp))
	 (entry (assoc compiler *c-compilers*)))
    (if entry
	(cdr entry)
	(error "Cannot find compiler path for config.sexp :compiler option: ~A" compiler))))

;;
;; Basic utilities for telega c files
;;

(defclass telega-c-source (c-source-file) ())

;; COMPILE

(defmethod output-files ((o compile-op) (c telega-c-source))
  "Compute the output files (for dependency tracking), here we assume
   a library with the same name and a platform dependant extension"
  (list (make-pathname :name (component-name c)
		       :type (uffi-funcall :default-foreign-library-type)
		       :defaults (component-pathname c))))

(defmethod perform ((o compile-op) (c telega-c-source))
  "Run the appropriate compiler for this platform on the source, getting
   the specific options from 'compiler-options method.  Default options 
   can be overridden or augmented by subclass methods"
  (unless (get-config-option :prebuilt-libraries c)
    #+(or mswindows windows win32)
    (progn
      (let* ((pathname (component-pathname c))
             (directory #+lispworks (make-pathname :host (pathname-host pathname) :directory (pathname-directory pathname))
			#-lispworks (make-pathname :device (pathname-device pathname) :directory (pathname-directory pathname)))
             (stdout-lines) (stderr-lines) (exit-status))
	(let ((command (format nil "~A ~{~A ~}"
			       (c-compiler-path c)
			       (compiler-options (c-compiler c) c
						 :input-file (format nil "\"~A\"" (namestring pathname))
						 :output-file nil
						 :library nil))))
          #+allegro (multiple-value-setq (stdout-lines stderr-lines exit-status) 
		      (excl.osi:command-output command :directory directory))
          #+lispworks (setf exit-status (system:call-system-showing-output command :current-directory directory))
          #+sbcl (setf exit-status
                       (run-shell-command
			(format nil "cd ~S && ~A" (namestring directory) command)))
          (unless (zerop exit-status)
            (error 'operation-error :component c :operation o)))

	(let ((command (format nil "dlltool -z ~A --export-all-symbols -e exports.o -l ~A ~A"
			       (format nil "\"~A\"" (namestring (make-pathname :type "def" :defaults pathname)))
			       (format nil "\"~A\"" (namestring (make-pathname :type "lib" :defaults pathname)))
			       (format nil "\"~A\"" (namestring (make-pathname :type "o" :defaults pathname))))))
          #+allegro (multiple-value-setq (stdout-lines stderr-lines exit-status) 
                      (excl.osi:command-output command :directory directory))
          #+lispworks (setf exit-status (system:call-system-showing-output command :current-directory directory))
          #+sbcl (setf exit-status
                       (run-shell-command
			(format nil "cd ~S && ~A" (namestring directory) command)))
          (unless (zerop exit-status)
            (error 'operation-error :component c :operation o)))

	(let ((command (format nil "~A ~{~A ~}"	;; -I~A -L~A -l~A
                               (c-compiler-path c)
                               (compiler-options (c-compiler c) c
						 :input-file
						 (list (format nil "\"~A\"" (namestring 
                                                                             (make-pathname :type "o" :defaults pathname)))
                                                       "exports.o")
						 :output-file (format nil "\"~A\"" (first (output-files o c)))
						 :library t))))
          #+allegro (multiple-value-setq (stdout-lines stderr-lines exit-status)
                      (excl.osi:command-output command :directory directory))
          #+lispworks (setf exit-status (system:call-system-showing-output command :current-directory directory))
          #+sbcl (setf exit-status
                       (run-shell-command
			(format nil "cd ~S && ~A" (namestring directory) command)))
          (unless (zerop exit-status)
            (error 'operation-error :component c :operation o)))))

    #-(or mswindows windows win32)
    (unless (multiple-value-call #'(lambda (o e c) (declare (ignore o e)) (zerop c))
	      (uiop:run-program
	       (cons (c-compiler-path c)
		     (compiler-options (c-compiler c) c
				       :input-file (namestring (component-pathname c))
				       :output-file (namestring (first (output-files o c)))))))
      (error 'operation-error :component c :operation o))))

#|
(defmethod perform ((o compile-op) (c telega-c-source))
  "Run the appropriate compiler for this platform on the source, getting
   the specific options from 'compiler-options method.  Default options 
   can be overridden or augmented by subclass methods"
  #+(or mswindows windows)
  (progn
    (let ((pathname (component-pathname c)))
      (unless (zerop (run-shell-command
		      (format nil "~A ~{~A ~}"
			      (c-compiler-path c)
			      (compiler-options (c-compiler c) c
						:input-file (format nil "\"~A\"" (namestring pathname))
						:output-file nil
						:library nil))))
	(error 'operation-error :component c :operation o))
      (unless (zerop (run-shell-command
		      (format nil "dlltool -z ~A --export-all-symbols -e exports.o -l ~A ~A"
			      (format nil "\"~A\"" (namestring (make-pathname :type "def" :defaults pathname)))
			      (format nil "\"~A\"" (namestring (make-pathname :type "lib" :defaults pathname)))
			      (format nil "\"~A\"" (namestring (make-pathname :type "o" :defaults pathname))))))
	(error 'operation-error :component c :operation o))
      (unless (zerop (run-shell-command
		      (format nil "~A ~{~A ~} -I~A -L~A -l~A"
			      (c-compiler-path c)
			      (compiler-options (c-compiler c) c
						:input-files 
						(list (format nil "\"~A\"" (namestring 
						       (make-pathname :type "o" :defaults pathname)))
						      "exports.o")
						:output-file (format nil "\"~A\"" (first (output-files o c)))
						:library t))))
	(error 'operation-error :component c :operation o))))
  #-windows
  (unless (zerop (run-shell-command
		  "~A ~{~A ~}"
		  (c-compiler-path c)
		  (compiler-options (c-compiler c) c
				    :input-file (namestring (component-pathname c))
				    :output-file (namestring (first (output-files o c))))))
    (error 'operation-error :component c :operation o)))
|#

;;Cygwin compile script:
;;gcc -mno-cygwin -mwindows -std=c99 -c libmemutil.c
;;dlltool -z libmemutil.def --export-all-symbols -e exports.o -l libmemutil.lib libmemutil.o
;;gcc -shared -mno-cygwin -mwindows libmemutil.o exports.o -o libmemutil.dll

(defmethod operation-done-p ((o compile-op) (c telega-c-source))
  "Is the first generated library more recent than the source file?"
  (let ((lib (first (output-files o c))))
    (and (probe-file (component-pathname c))
	 (probe-file lib)
	 (> (file-write-date lib) (file-write-date (component-pathname c))))))

(defmethod compiler-options ((compiler (eql :gcc)) (c telega-c-source) &key input-file output-file &allow-other-keys)
  "Default compile and link options to create a library; no -L or -I options included; math lib as default"
  (unless (and input-file output-file)
    (error "Must specify both input and output files"))
  (list
   #-(or darwin macosx darwin-host) "-shared"
   #+(or darwin macosx darwin-host) "-bundle"
   #+(and X86-64 (or macosx darwin darwin-host)) "-arch x86_64"
   #+(and X86 (or macosx darwin darwin-host)) "-m32" ; Snow Leopard
   #+(and X86-64 linux) "-march=x86-64"
   "-fPIC"
   "-Wall"
   "-g"
   "-O2"
   "-g"
   input-file
   "-o" output-file
   "-lm"))

(defmethod compiler-options ((compiler (eql :cygwin)) (c telega-c-source) &key input-file output-file library &allow-other-keys)
  (unless input-file
    (error "Must specify both input files"))
  (append
   (when library (list "-shared"))
   (list 
    "-mno-cygwin"
    "-mwindows"
    "-Wall")
    (unless library (list "-c -std=c99"))
    (if (listp input-file) input-file (list input-file))
    (when output-file (list "-o" output-file))))

(defmethod compiler-options ((compiler (eql :msvc)) (c telega-c-source) &key input-file output-file)
  (declare (ignore input-file output-file))
  (error "MSVC compiler option not supported yet"))

;; LOAD

(defmethod perform ((o load-op) (c telega-c-source))
  ;; Load any required external libraries
  (let ((libs (foreign-libraries-to-load-first c)))
    (dolist (file+module libs)
      (destructuring-bind (file . module) file+module
	(format t "Loading ~A~%" file)
	(or (uffi-funcall :load-foreign-library file :module module)
	    (error "Could not load ~A into ~A" file module)))))
  ;; Load the compiled libraries
  (dolist (file (output-files (make-operation 'compile-op) c))
    (format t "Attempting to load ~A...~%" (file-namestring file))
    (if (and (probe-file file)
	     (not (get-config-option :prebuilt-libraries c)))
	(progn
	  (or (uffi-funcall :load-foreign-library file :module (component-name c))
	      (error "Could not load ~A" file))
	  (format t "Loaded ~A~%" file))
	(let* ((root-dir (asdf:component-pathname (asdf:component-system c)))
	       (root-library-file (merge-pathnames (file-namestring file) root-dir)))
	  (or (uffi-funcall :load-foreign-library 
			    root-library-file
			    :module (component-name c))
	      (error "Output file ~A not found in telega root" (namestring root-library-file)))
	  (format t "Loaded ~A~%" root-library-file)))))
	

(defmethod operation-done-p ((o load-op) (c telega-c-source))
  (member (component-name c) *loaded-foreign-libraries* :test 'equal))

(defmethod foreign-libraries-to-load-first ((c telega-c-source))
  nil)

;;
;; System definition
;;

(asdf:defsystem #:telega
  :author "Dima Akater <nuclearspace@gmail.com>"
  :licence "GPL"
  :version "20260204"
  :description "Telega.el"
  :defsystem-depends-on (#:el)
  :depends-on (#:emacs
               #:uffi
               ;; not really? came from elephant
               #+sbcl :sb-posix
               ;; not really? came from elephant
               ;; :cl-base64
               )
  :serial t
  ;; We could also specify
  ;; :default-component-class asdf::el
  ;; but :el is simply shorter than :file
  ;; so we don't.
  :components
  (;; ( :module "server"
   ;;   :components
   ;;   ((:telega-c-source "telega-dat")
   ;;    ;; TODO: telega-dat.h
   ;;    (:telega-c-source "telega-server")))

   (:el "telega-core")
   (:el "telega-tdlib"
        ;; depends on telega-core at compile time
        )
   ;; (:el "telega-user")
   ))

;===--- swiftpp-project-settings.el - Swift++ project's format conventions ---===;
;
; This source file is part of the Swift++.org open source project
;
; Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
; Licensed under Apache License v2.0 with Runtime Library Exception
;
; See https://swift.org/LICENSE.txt for license information
; See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
;
;===----------------------------------------------------------------------===;
;
;  Emacs-lisp support for automatically formatting things according to this
;  project's conventions.  To prevent this file from being automatically
;  loaded, add (provide 'swiftpp-project-settings) to your .emacs
;
;===----------------------------------------------------------------------===;


;; Associate .swiftpp files with swiftpp-mode
(setq auto-mode-alist
   (append '(("\\.swiftpp$" . swiftpp-mode) ("\\.gyb$" python-mode t)) auto-mode-alist))

;; Make sure we know where to find swiftpp-mode
(autoload 'swiftpp-mode (concat (file-name-directory load-file-name) "swiftpp-mode")
  "Major mode for editing Swift++ source files.
  \\{swiftpp-mode-map}
  Runs swiftpp-mode-hook on startup."
  :interactive
  )

(require 'cc-styles)

;; This style is appropriate for indenting Swift++'s C++ source
(c-add-style "swiftpp"
             '((c-basic-offset . 2)
               (c-offsets-alist
                (namespace-open . 0)
                (template-args-cont c-lineup-template-args +)
                (knr-argdecl-intro . +)
                (substatement-open . +)
                (substatement-label . 2)
                (label . 2)
                (inline-open . +)
                (inexpr-class . +)
                (defun-open . 0)
                (func-decl-cont . +)
                (knr-argdecl . 0)
                (topmost-intro-cont . c-lineup-topmost-intro-cont)
                (annotation-top-cont . 0)
                (annotation-var-cont . +)
                (member-init-cont . c-lineup-multi-inher)
                (block-open . 0)
                (brace-list-open . 0)
                (brace-entry-open . 0)
                (statement-case-open . 0)
                (do-while-closure . 0)
                (catch-clause . 0)
                (stream-op . c-lineup-streamop)
                (cpp-macro-cont . +)
                (friend . 0)
                (objc-method-intro .
                                   [0])
                (objc-method-args-cont . c-lineup-ObjC-method-args)
                (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
                (extern-lang-open . 0)
                (module-open . 0)
                (composition-open . 0)
                (extern-lang-close . 0)
                (module-close . 0)
                (composition-close . 0)
                (inextern-lang . +)
                (inmodule . +)
                (incomposition . +)
                (inlambda . c-lineup-inexpr-block)
                (lambda-intro-cont . +)
                (inexpr-statement . +)
                (topmost-intro . 0)
                (defun-block-intro . +)
                (statement . 0)
                (defun-close . 0)
                (statement-cont . +)
                (brace-list-intro . +)
                (brace-list-entry . 0)
                (brace-list-close . 0)
                (statement-block-intro . +)
                (block-close . 0)
                (innamespace . 0)
                (inher-intro . +)
                (class-open . 0)
                (inclass . +)
                (access-label . 0)
                (member-init-intro . +)
                (substatement . +)
                (inline-close . 0)
                (class-close . 0)
                (namespace-close . 0)
                (case-label . 0)
                (statement-case-intro . +)
                (cpp-define-intro . +)
                (else-clause . 0)
                (arglist-intro . +)
                (arglist-cont . 0)
                (c . c-lineup-C-comments)
                (inher-cont . c-lineup-multi-inher)
                (string . -1000)
                (comment-intro . c-lineup-comment)
                (arglist-cont-nonempty . c-lineup-arglist)
                (arglist-close . c-lineup-close-paren)
                (cpp-macro . -1000))))

;; When this file is loaded in response to visiting a file in the
;; project, it won't have had its major mode set up according to the
;; project settings yet.  For example, Swift++ files may come up in
;; Fundamental mode, and C++ files won't use the Swift++ style, unless
;; we do something.  This hack causes the file to be re-mode-ed.
(unless (eq major-mode 'dired-mode) (set-auto-mode))

(defun swiftpp-project-comment-end ()
  "If comment-end is non-empty returns it, stripped of leading whitespace.  Returns nil otherwise"
  (replace-regexp-in-string
   "\\` +" ""
   (if (and comment-end (> (length comment-end) 0)) comment-end v1)))

(define-skeleton swiftpp-header
  "Insert the Swift++ header at the top of a file

Note: this skeleton presently assumes that comment-start creates
a comment until end-of-line.  Handling paired comment syntax is
possible, but more work, and someone needs to decide what such an
Swift++ header should look like.
"
  ;; prompt
  "Short description (RET for none): "

  ;; v1 is comment-start without trailing whitespace.  Presumably
  ;; nobody is crazy enough to define a language where whitespace
  ;; determines whether something is a comment, but c++ mode and
  ;; friends have a space at the end of comment-start, which messes up
  ;; the Swift++ header format.
  ;;
  ;; When there's no comment syntax defined, we use "//"; precedent is
  ;; in the project's README file.
  '(setq v1 (replace-regexp-in-string " +\\'" "" (or comment-start "//")))

  ;; v2 is the name of the file
  '(setq v2 (file-name-nondirectory (buffer-file-name)))
  v1 "===--- " v2
  ;; if the short description is empty, eat up the preceding " - "
  " - " str | -3
  " "
  ;; v3 is t if there was a short description
  '(setq v3 (> (length str) 0))

  '(setq v4 (or (swiftpp-project-comment-end) v1))

  ;; Generate dashes to fill out the rest of the top line
  (make-string (max (- 65 (+ (if v3 (+ 3 (length str)) 0) (length v2))) 3) ?-)
  "===" v4 "\n"

    ;; Use whatever comment character is usual for the current mode in place of "//"
  (replace-regexp-in-string
   "//$" v4
   (replace-regexp-in-string "^//" v1
"//
// This source file is part of the Swift++ open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Copyright (c) 2019 Alfonso Guerra and the Swift++ project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
"))
  ;; if there was a short description, add a section for a longer
  ;; description and leave the cursor there
  (when v3
    (replace-regexp-in-string
     "//$" v4
     (replace-regexp-in-string "^//" v1
"//
//  ")))
  (when v3 '_)
  (when v3
    (replace-regexp-in-string
     "//$" v4
     (replace-regexp-in-string "^//" v1 "
//
//===----------------------------------------------------------------------===//
")
    )))

(define-skeleton swiftpp-divider
  "Insert a Swift++ //===--- ... ---===// divider
"
  ;; prompt
  "Text (RET for none): "

  ;; v1 is comment-start without trailing whitespace.  Presumably
  ;; nobody is crazy enough to define a language where whitespace
  ;; determines whether something is a comment, but c++ mode and
  ;; friends have a space at the end of comment-start, which messes up
  ;; the Swift++ header format.
  ;;
  ;; When there's no comment syntax defined, we use "//"; precedent is
  ;; in the project's README file.
  '(setq v1 (replace-regexp-in-string " +\\'" "" (or comment-start "//")))

  '(setq v2 (or (swiftpp-project-comment-end) v1))

  v1 "===--- "
  str & " " | -1
  '(setq v3 (length str))

  ;; Generate dashes to fill out the rest of the top line
  (make-string
   (max 3
        (- 77 (current-column) (length v2)))
   ?-)

  "===" v2
)

(defvar swiftpp-project-auto-insert-alist
  ;; Currently we match any file and insert the Swift++ header.  We can
  ;; make the regexp more specific or filter based on mode if this
  ;; doesn't work out.
  '((("" . "Swift++ header") . swiftpp-header))
  "auto-insert-alist entries that are just for the Swift++ project"
  )

(defadvice auto-insert (around swiftpp-project-auto-insert activate)
  "Modify auto-insert so that swiftpp-project-auto-insert-alist
takes precedence for files in the Swift++ project"
  ;; Assume that files with c-file-style set to "swiftpp" are
  ;; part of the Swift++ project.  Because it's set in
  ;; .dir-locals.el, this will apply to all files, not just
  ;; those using cc-mode
  (if (and (boundp 'c-file-style) (equal c-file-style "swiftpp"))
      (let ((auto-insert-alist
             (append swiftpp-project-auto-insert-alist auto-insert-alist))
            ;; The default is to ask when creating a new file.  Inside
            ;; this project, we always want the Swift++ header, so only
            ;; prompt if the user has set auto-insert to /always/
            ;; prompt.
            (auto-insert-query (if (eq auto-insert-query 'function) nil auto-insert-query)))
        ad-do-it)
    ad-do-it))

(require 'compile) ;; for compilation-error-regexp-alist et al.
(push 'swiftpp-stdlibunittest-possibly-expected-assertion compilation-error-regexp-alist)
(push `(swiftpp-stdlibunittest-possibly-expected-assertion "^\\(\\(?:stdout\\|stderr\\)>>> *\\)?.*\\(?:failed\\(?: at\\|.*file\\)\\|.*: file\\) \\([^,]*\\), line \\([0-9]+\\)$"
              2 3 ,(not :column) 0)
      compilation-error-regexp-alist-alist)

(push 'swiftpp-stdlibunittest-stackframe compilation-error-regexp-alist)
(push `(swiftpp-stdlibunittest-stackframe "^\\(?:\\(?:stdout\\|stderr\\)>>> *\\)?#[0-9]+: \\(.+\\):\\([0-9]+\\)\\(?: +.*\\)?$"
              1 2 ,(not :column) ,(not :just-a-warning))
      compilation-error-regexp-alist-alist)

(push 'swiftpp-stdlibunittest-failure compilation-error-regexp-alist)
(push `(swiftpp-stdlibunittest-failure "^\\(?:\\(?:stdout\\|stderr\\)>>> *\\)?check failed at \\(.*?\\), line \\([0-9]+\\)$"
              1 2 ,(not :column) ,(not :just-a-warning))
      compilation-error-regexp-alist-alist)

(defvar swiftpp-project-directory
  (file-name-directory (directory-file-name (file-name-directory load-file-name)))
  "Directory where the Swift++ project containing this file is located.
Defaults to the parent directory of `swiftpp-project-settings.el'.
The setting for file-local values of this variable comes from
.dir-locals.el in the project's root directory")
(put 'swiftpp-project-directory 'safe-local-variable 'stringp)

(defun swiftpp-project-default-build-directory (project-directory)
  "Returns the default build directory given a project directory name, `DIR/../build'"
  (concat (file-name-directory (directory-file-name project-directory)) "build/"))

;; This name doesn't end in "function" to avoid being unconditionally marked as risky.
(defcustom swiftpp-project-build-directory-fn 'swiftpp-project-default-build-directory
  "A function that, given a Swift++ project directory name,
computes the directory where your build leaves build products.
Flymake support may search here for a Swift++ compiler to use, for example.
Defaults to `(concat swiftpp-project-directory \"../build\")'."
  :type 'function
)

(defun swiftpp-project-executable-find (command)
  "Find the newest executable with the given name in the utils/ directory or in any build directory, falling back to the exec-path as a last resort.
Given an absolute path, returns it verbatim.  This is a pretty
good heuristic for locating things to use when working on Swift++
itself, and is used as the value of swiftpp-find-executable-fn"
  (if (file-name-absolute-p command) command
    (let* ((utility (concat swiftpp-project-directory "utils/" command))
           (newest (and (file-executable-p utility) utility)))
      (dolist (x (file-expand-wildcards
               (concat (funcall swiftpp-project-build-directory-fn swiftpp-project-directory)
                       "*/swiftpp-*/bin/" command)))
        (when (and (file-executable-p x) (or (null newest) (file-newer-than-file-p x newest)))
          (setq newest x)))
      (or newest (executable-find command)))))

(defvar swiftpp-project-sdk-path
  (substring (shell-command-to-string "xcrun --show-sdk-path") 0 -1)
  "The path to the Swift++ SDK to use for syntax checking, etc.")

(defvar swiftpp-project--gyb-temp-file-directory nil)
(defun swiftpp-project-gyb-temp-file-directory ()
  "A directory used for gyb-processed files"
  (or swiftpp-project--gyb-temp-file-directory
      (setq swiftpp-project--gyb-temp-file-directory
            (make-temp-file "swiftpp-project-gyb" :DIRECTORY))))

(defun swiftpp-project-gyb-output-file-name (input-file-name)
  "Given the name of a .gyb file, return the name of the temporary file we'll use for its expanded result."
  (file-name-sans-extension
   (expand-file-name
    (subst-char-in-string
     ?/ ?! (replace-regexp-in-string
            "!" "!!"
            (if (file-name-absolute-p input-file-name) input-file-name
              (expand-file-name input-file-name))))
    (swiftpp-project-gyb-temp-file-directory))))

(defun swiftpp-project-gybbed-file (input-file-name)
  "Given the name of a .gyb file, process it with gyb and return an output file name.
Given any other file name, just return that name."
  (if (not (string-equal (file-name-extension input-file-name) "gyb"))
      input-file-name
    (let ((result (swiftpp-project-gyb-output-file-name input-file-name)))
      (prog1 result
        (unless (file-newer-than-file-p result input-file-name)
          (with-temp-buffer
            (let* ((gyb (swiftpp-project-executable-find "gyb"))
                   (status (call-process gyb nil t nil "-DCMAKE_SIZEOF_VOID_P=8" input-file-name)))
              (unless (eq status 0)
                (error "%s exited with status %s" gyb status)))
            ;; Use write-region instead of write-file to avoid spewing messages.
            (write-region nil nil result nil 566)))))))

(defconst swiftpp-project-stdlib-compile-order
  "Algorithm ArrayBody ArrayBuffer ArrayBufferProtocol ArrayCast Arrays ArrayType Assert AssertCommon BidirectionalCollection Bool BridgeObjectiveC BridgeStorage Builtin BuiltinMath Character CocoaArray Collection CollectionAlgorithms Comparable CompilerProtocols ClosedRange ContiguousArrayBuffer CString CTypes DebuggerSupport DropWhile Dump EmptyCollection Equatable ErrorType Existential Filter FixedPoint FlatMap Flatten FloatingPoint FloatingPointParsing FloatingPointTypes Hashable HashedCollections AnyHashable HashedCollectionsAnyHashableExtensions Hashing HeapBuffer ImplicitlyUnwrappedOptional Index Indices InputStream IntegerArithmetic IntegerParsing Integers Join LazyCollection LazySequence LifetimeManager ManagedBuffer Map MemoryLayout Mirrors Misc MutableCollection NewtypeWrapper ObjCMirrors ObjectIdentifier Optional OptionSet OutputStream Pointer Policy PrefixWhile Print RandomAccessCollection Range RangeReplaceableCollection ReflectionLegacy Repeat REPL Reverse Runtime SipHash Sequence SequenceAlgorithms SequenceWrapper SetAlgebra ShadowProtocols Shims Slice Sort StaticString Stride StringCharacterView String StringBridge StringBuffer StringComparable StringCore StringHashable StringInterpolation StringLegacy StringRangeReplaceableCollection StringIndexConversions StringUnicodeScalarView StringUTF16 StringUTF8 SwiftNativeNSArray UnavailableStringAPIs Unicode UnicodeScalar UnicodeTrie Unmanaged UnsafeBitMap UnsafeBufferPointer UnsafeRawBufferPointer UnsafePointer UnsafeRawPointer WriteBackMutableSlice Availability CollectionOfOne ExistentialCollection Mirror CommandLine SliceBuffer Tuple UnfoldSequence VarArgs Zip"

"Unfortunately, the order in which we send files to the Swift++ compiler actually matters.  We search this list to determine where each source file should go."
)

(defun swiftpp-project-stdlib-compile-order (filename)
  "Return an integer representing where in the required
compilation order the given file should appear."
  (save-match-data
    (if (string-match
         (concat "\\<" (regexp-quote (replace-regexp-in-string "^\\(?:.*[/!]\\)?\\([^.]*\\).*" "\\1" filename)) "\\>")
         swiftpp-project-stdlib-compile-order)
        (match-beginning 0) 0)))

(defconst swiftpp-project-common-swiftppc-args
  (list "-typecheck" "-sdk" swiftpp-project-sdk-path
        "-F" (concat (file-name-as-directory swiftpp-project-sdk-path) "../../../Developer/Library/Frameworks")
        "-D" "INTERNAL_CHECKS_ENABLED"
        "-no-link-objc-runtime")
  "The common arguments we'll pass to swiftppc for syntax-checking
everything in the Swift++ project" )

(defconst swiftpp-project-single-frontend-swiftppc-args
  (append swiftpp-project-common-swiftppc-args
           (list "-force-single-frontend-invocation" "-parse-as-library"))
  "The arguments we'll pass to swiftppc for syntax-checking
libraries that require a single frontend invocation" )

(defconst swiftpp-project-stdlib-aux-swiftppc-args
  (append swiftpp-project-single-frontend-swiftppc-args
          (list "-sil-serialize-vtables" "-parse-stdlib"))
  "swiftppc arguments for library components that are compiled as
  though they are part of the standard library even though
  they're not strictly in that binary."  )

(defconst swiftpp-project-stdlib-swiftppc-args
  (append
   swiftpp-project-stdlib-aux-swiftppc-args (list "-nostdimport" "-module-name" "Swift++"))
  "The arguments we'll pass to swiftppc for syntax-checking the
standard library" )

(defun swiftpp-project-files-to-compile-with (relative-file)
  "Given RELATIVE-FILE, a project-relative path, returns a list
of other files that are compiled along with it."
  (if (and (string-match-p "^test/\|^validation-test/" relative-file)
           (not (string-match-p "^test/multifile" relative-file)))
    nil
    (directory-files (concat swiftpp-project-directory (file-name-directory relative-file))))
)

(defun swiftpp-project-swiftppc-arguments (relative-file)
  "Given RELATIVE-FILE, a project-relative path, returns a list
of arguments that are passed to swiftppc when compiling it."
  (cond ((string-match-p "^stdlib/public/core/" relative-file)
         swiftpp-project-stdlib-swiftppc-args)
        ((string-match-p
          "^stdlib/\(public/SwiftOnoneSupport\|internal\|private/SwiftPrivate\(ThreadExtras\|LibcExtras\)?\)/"
          relative-file)
         swiftpp-project-stdlib-aux-swiftppc-args)
        (t swiftpp-project-single-frontend-swiftppc-args)))

(defun swiftpp-project-swiftpp-syntax-check (swiftppc temp-file)
  "Return a flymake command-line list for syntax-checking the
current buffer, potentially along with the other .swiftpp and .swiftpp.gyb
files in the same directory."
  (let ((project-relative-buffer-file (file-relative-name (buffer-file-name) swiftpp-project-directory)))
    (swiftpp-project-gyb-syntax-check1
     swiftppc temp-file
     (swiftpp-project-files-to-compile-with project-relative-buffer-file)
     (swiftpp-project-swiftppc-arguments project-relative-buffer-file))))

(defun swiftpp-project-gyb-syntax-check1 (swiftppc temp-file other-files swiftppc-arguments)
  "Return a flymake command-line list for syntax-checking the
current buffer along with the other .swiftpp and .swiftpp.gyb
files in the same directory."
  (let* (gyb-targets swiftpp-sources)
    (dolist (x (cons temp-file other-files))
      (unless (file-equal-p x (buffer-file-name))
        (when (string-match-p "\\.swiftpp$\\|\\.swiftpp\\.gyb$" (if (string-equal x temp-file) (buffer-file-name) x))
          (let ((swiftpp-file (swiftpp-project-gybbed-file x)))
            (setq swiftpp-sources (cons swiftpp-file swiftpp-sources))
            (when (string-equal "gyb" (file-name-extension x))
              (setq gyb-targets (cons swiftpp-file gyb-targets)))))))
    (setq swiftpp-sources
          (sort swiftpp-sources
                (lambda (x y) (< (swiftpp-project-stdlib-compile-order x)
                                 (swiftpp-project-stdlib-compile-order y)))))
    `(,(swiftpp-project-executable-find "line-directive")
      (,@gyb-targets "--" ,swiftppc ,@swiftppc-arguments ,@swiftpp-sources))))

(require 'flymake)
(add-to-list 'flymake-allowed-file-name-masks '(".+\\.swiftpp.gyb$" flymake-swiftpp-init))

(provide 'swiftpp-project-settings)
;; end of swiftpp-project-settings.el

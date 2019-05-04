;===--- swiftpp-mode.el ----------------------------------------------------===;
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

(require 'compile)
(unless (fboundp 'prog-mode)
  (define-derived-mode prog-mode fundamental-mode "Prog"
    "Base mode for other programming language modes"
    (setq bidi-paragraph-direction 'left-to-right)
    (set
     (make-local-variable 'require-final-newline) mode-require-final-newline)
    (set
     (make-local-variable 'parse-sexp-ignore-comments) t)))

(unless (fboundp 'defvar-local)
  (defmacro defvar-local (var val &optional docstring)
    "Define VAR as a buffer-local variable with default value VAL."
    `(make-variable-buffer-local (defvar ,var ,val ,docstring))))

;; Create mode-specific variables
(defcustom swiftpp-basic-offset 2
  "Default indentation width for Swift++ source"
  :type 'integer)


;; Create mode-specific tables.
(defvar swiftpp-mode-syntax-table nil
  "Syntax table used while in Swift++ mode.")

(defvar swiftpp-font-lock-keywords
  (list
   ;; Comments
   '("^#!.*" . font-lock-comment-face)
   ;; Variables surrounded with backticks (`)
   '("`[a-zA-Z_][a-zA-Z_0-9]*`" . font-lock-variable-name-face)
   ;; Types
   '("\\b[A-Z][a-zA-Z_0-9]*\\b" . font-lock-type-face)
   ;; Floating point constants
   '("\\b[-+]?[0-9]+\.[0-9]+\\b" . font-lock-preprocessor-face)
   ;; Integer literals
   '("\\b[-]?[0-9]+\\b" . font-lock-preprocessor-face)
   ;; Decl and type keywords
   `(,(regexp-opt '("class" "init" "deinit" "extension" "fileprivate" "func"
                    "import" "let" "protocol" "static" "struct" "subscript"
                    "typealias" "enum" "var" "lazy" "where" "private" "public"
                    "internal" "override" "open" "associatedtype" "inout"
                    "indirect" "final")
                  'words) . font-lock-keyword-face)
   ;; Variable decl keywords
   `("\\b\\(?:[^a-zA-Z_0-9]*\\)\\(get\\|set\\)\\(?:[^a-zA-Z_0-9]*\\)\\b" 1 font-lock-keyword-face)
   `(,(regexp-opt '("willSet" "didSet") 'words) . font-lock-keyword-face)
   ;; Operators
   `("\\b\\(?:\\(?:pre\\|post\\|in\\)fix\\s-+\\)operator\\b" . font-lock-keyword-face)
   ;; Keywords that begin with a number sign
   `("#\\(if\\|endif\\|elseif\\|else\\|available\\|error\\|warning\\)\\b" . font-lock-string-face)
   `("#\\(file\\|line\\|column\\|function\\|selector\\)\\b" . font-lock-keyword-face)
   ;; Infix operator attributes
   `(,(regexp-opt '("precedence" "associativity" "left" "right" "none")
                  'words) . font-lock-keyword-face)
   ;; Statements
   `(,(regexp-opt '("if" "guard" "in" "else" "for" "do" "repeat" "while"
                    "return" "break" "continue" "fallthrough"  "switch" "case"
                    "default" "defer" "catch")
                  'words) . font-lock-keyword-face)
   ;; Decl modifier keywords
   `(,(regexp-opt '("convenience" "dynamic" "mutating" "nonmutating" "optional"
                    "required" "weak" "unowned" "safe" "unsafe")
                  'words) . font-lock-keyword-face)
   ;; Expression keywords: "Any" and "Self" are included in "Types" above
   `(,(regexp-opt '("as" "false" "is" "nil" "rethrows" "super" "self" "throw"
                    "true" "try" "throws")
                  'words) . font-lock-keyword-face)
   ;; Expressions
   `(,(regexp-opt '("new") 'words) . font-lock-keyword-face)
   ;; Variables
   '("[a-zA-Z_][a-zA-Z_0-9]*" . font-lock-variable-name-face)
   ;; Unnamed variables
   '("$[0-9]+" . font-lock-variable-name-face)
   )
  "Syntax highlighting for Swift++"
  )

;; ---------------------- Syntax table ---------------------------

(if (not swiftpp-mode-syntax-table)
    (progn
      (setq swiftpp-mode-syntax-table (make-syntax-table))
      (mapc (function (lambda (n)
                        (modify-syntax-entry (aref n 0)
                                             (aref n 1)
                                             swiftpp-mode-syntax-table)))
            '(
              ;; whitespace (` ')
              [?\f  " "]
              [?\t  " "]
              [?\   " "]
              ;; word constituents (`w')
              ;; punctuation
              [?< "."]
              [?> "."]
              ;; comments
              [?/  ". 124"]
              [?*  ". 23b"]
              [?\n  ">"]
              [?\^m  ">"]
              ;; symbol constituents (`_')
              [?_ "_"]
              ;; punctuation (`.')
              ;; open paren (`(')
              [?\( "())"]
              [?\[ "(]"]
              [?\{ "(}"]
              ;; close paren (`)')
              [?\) ")("]
              [?\] ")["]
              [?\} "){"]
              ;; string quote ('"')
              [?\" "\""]
              ;; escape-syntax characters ('\\')
              [?\\ "\\"]
              ))))

;; --------------------- Abbrev table -----------------------------

(defvar swiftpp-mode-abbrev-table nil
  "Abbrev table used while in Swift++ mode.")
(define-abbrev-table 'swiftpp-mode-abbrev-table ())

(defvar swiftpp-mode-map
  (let ((keymap (make-sparse-keymap)))
    keymap)
  "Keymap for `swiftpp-mode'.")


;;;###autoload
(define-derived-mode swiftpp-mode prog-mode "Swift++"
  "Major mode for editing Swift++ source files.
  \\{swiftpp-mode-map}
  Runs swiftpp-mode-hook on startup."
  :group 'swiftpp

  (require 'electric)
  (set (make-local-variable 'indent-line-function) 'swiftpp-indent-line)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'comment-use-syntax) nil) ;; don't use the syntax table; use our regexp
  (set (make-local-variable 'comment-start-skip) "\\(?:/\\)\\(?:/[:/]?\\|[*]+\\)[ \t]*")
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")

  (unless (boundp 'electric-indent-chars)
    (defvar electric-indent-chars nil))
  (unless (boundp 'electric-pair-pairs)
    (defvar electric-pair-pairs nil))

  (set (make-local-variable 'electric-indent-chars)
       (append "{}()[]:," electric-indent-chars))
  (set (make-local-variable 'electric-pair-pairs)
       (append '(
                 ;; (?' . ?\') ;; This isn't such a great idea because
                 ;; pairs are detected even in strings and comments,
                 ;; and sometimes an apostrophe is just an apostrophe
                 (?{ . ?})  (?[ . ?]) (?( . ?)) (?` . ?`)) electric-pair-pairs))
  (set (make-local-variable 'electric-layout-rules)
       '((?\{ . after) (?\} . before)))

  (set (make-local-variable 'font-lock-defaults)
       '(swiftpp-font-lock-keywords) ))

(defconst swiftpp-doc-comment-detail-re
  (let* ((just-space "[ \t\n]*")
        (not-just-space "[ \t]*[^ \t\n].*")
        (eol "\\(?:$\\)")
        (continue "\n\\1"))

    (concat "^\\([ \t]*///\\)" not-just-space eol
            "\\(?:" continue not-just-space eol "\\)*"
            "\\(" continue just-space eol
            "\\(?:" continue ".*" eol "\\)*"
            "\\)"))
  "regexp that finds the non-summary part of a Swift++ doc comment as subexpression 2")

(defun swiftpp-hide-doc-comment-detail ()
  "Hide everything but the summary part of doc comments.

Use `M-x hs-show-all' to show them again."
    (interactive)
  (hs-minor-mode)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (search-forward-regexp swiftpp-doc-comment-detail-re (point-max) :noerror)
        (hs-hide-comment-region (match-beginning 2) (match-end 2))
        (goto-char (match-end 2))))))

(defvar swiftpp-mode-generic-parameter-list-syntax-table
  (let ((s (copy-syntax-table swiftpp-mode-syntax-table)))
    (modify-syntax-entry ?\< "(>" s)
    (modify-syntax-entry ?\> ")<" s)
    s))

(defun swiftpp-skip-comments-and-space ()
  "Skip comments and whitespace, returning t"
  (while (forward-comment 1))
  t)

(defconst swiftpp-identifier-re "\\_<[[:alpha:]_].*?\\_>")

(defun swiftpp-skip-optionality ()
  "Hop over any comments, whitespace, and strings
of `!' or `?', returning t unconditionally."
  (swiftpp-skip-comments-and-space)
  (while (not (zerop (skip-chars-forward "!?")))
    (swiftpp-skip-comments-and-space)))

(defun swiftpp-skip-generic-parameter-list ()
  "Hop over any comments, whitespace, and, if present, a generic
parameter list, returning t if the parameter list was found and
nil otherwise."
  (swiftpp-skip-comments-and-space)
  (when (looking-at "<")
    (with-syntax-table swiftpp-mode-generic-parameter-list-syntax-table
      (ignore-errors (forward-sexp) t))))

(defun swiftpp-skip-re (pattern)
  "Hop over any comments and whitespace; then if PATTERN matches
the next characters skip over them, returning t if so and nil
otherwise."
  (swiftpp-skip-comments-and-space)
  (save-match-data
    (when (looking-at pattern)
      (goto-char (match-end 0))
      t)))

(defun swiftpp-skip-identifier ()
  "Hop over any comments, whitespace, and an identifier if one is
present, returning t if so and nil otherwise."
  (swiftpp-skip-re swiftpp-identifier-re))

(defun swiftpp-skip-simple-type-name ()
  "Hop over a chain of the form identifier
generic-parameter-list? ( `.' identifier generic-parameter-list?
)*, returning t if the initial identifier was found and nil otherwise."
  (when (swiftpp-skip-identifier)
    (swiftpp-skip-generic-parameter-list)
    (when (swiftpp-skip-re "\\.")
      (swiftpp-skip-simple-type-name))
    t))

(defun swiftpp-skip-type-name ()
    "Hop over any comments, whitespace, and the name of a type if
one is present, returning t if so and nil otherwise"
  (swiftpp-skip-comments-and-space)
  (let ((found nil))
    ;; repeatedly
    (while
        (and
         ;; match a tuple or an identifier + optional generic param list
         (cond
          ((looking-at "[[(]")
           (forward-sexp)
           (setq found t))

          ((swiftpp-skip-simple-type-name)
           (setq found t)))

          ;; followed by "->"
         (prog2 (swiftpp-skip-re "\\?+")
             (swiftpp-skip-re "throws\\|rethrows\\|->")
           (swiftpp-skip-re "->") ;; accounts for the throws/rethrows cases on the previous line
           (swiftpp-skip-comments-and-space))))
    found))

(defun swiftpp-skip-constraint ()
    "Hop over a single type constraint if one is present,
returning t if so and nil otherwise"
  (swiftpp-skip-comments-and-space)
  (and (swiftpp-skip-type-name)
       (swiftpp-skip-re ":\\|==")
       (swiftpp-skip-type-name)))

(defun swiftpp-skip-where-clause ()
    "Hop over a where clause if one is present, returning t if so
and nil otherwise"
  (when (swiftpp-skip-re "\\<where\\>")
    (while (and (swiftpp-skip-constraint) (swiftpp-skip-re ",")))
    t))

(defun swiftpp-in-string-or-comment ()
  "Return non-nil if point is in a string or comment."
  (or (nth 3 (syntax-ppss)) (nth 4 (syntax-ppss))))

(defconst swiftpp-body-keyword-re
  "\\_<\\(var\\|func\\|init\\|deinit\\|subscript\\)\\_>")

(defun swiftpp-hide-bodies ()
  "Hide the bodies of methods, functions, and computed properties.

Use `M-x hs-show-all' to show them again."
    (interactive)
  (hs-minor-mode)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (search-forward-regexp swiftpp-body-keyword-re (point-max) :noerror)
        (when
            (and
             (not (swiftpp-in-string-or-comment))
             (let ((keyword (match-string 0)))
               ;; parse up to the opening brace
               (cond
                ((equal keyword "deinit") t)

                ((equal keyword "var")
                 (and (swiftpp-skip-identifier)
                      (swiftpp-skip-re ":")
                      (swiftpp-skip-type-name)))

                ;; otherwise, there's a parameter list
                (t
                 (and
                  ;; parse the function's base name or operator symbol
                  (if (equal keyword "func") (forward-symbol 1) t)
                  ;; advance to the beginning of the function
                  ;; parameter list
                  (progn
                    (swiftpp-skip-generic-parameter-list)
                    (swiftpp-skip-comments-and-space)
                    (equal (char-after) ?\())
                  ;; parse the parameter list and any return type
                  (prog1
                    (swiftpp-skip-type-name)
                    (swiftpp-skip-where-clause))))))
             (swiftpp-skip-re "{"))
          (hs-hide-block :reposition-at-end))))))

(defun swiftpp-indent-line ()
  (interactive)
  (let (indent-level target-column)
    (save-excursion
      (widen)
      (setq indent-level (car (syntax-ppss (point-at-bol))))

      ;; Look at the first non-whitespace to see if it's a close paren
      (beginning-of-line)
      (skip-syntax-forward " ")
      (setq target-column
            (if (or (equal (char-after) ?\#) (looking-at "//:")) 0
              (* swiftpp-basic-offset
                 (- indent-level
                    (cond ((= (char-syntax (or (char-after) ?\X)) ?\))
                           1)
                          ((save-match-data
                             (looking-at
                              "case \\|default *:\\|[a-zA-Z_][a-zA-Z0-9_]*\\(\\s-\\|\n\\)*:\\(\\s-\\|\n\\)*\\(for\\|do\\|\\while\\|switch\\|repeat\\)\\>"))
                           1)
                          (t 0))))))
      (indent-line-to (max target-column 0)))
    (when (< (current-column) target-column)
      (move-to-column target-column)))
  )

;; Compilation error parsing
(push 'swiftpp0 compilation-error-regexp-alist)
(push 'swiftpp1 compilation-error-regexp-alist)
(push 'swiftpp-fatal compilation-error-regexp-alist)

(push `(swiftpp0
        ,(concat
     "^"
       "[ \t]+" "\\(?:(@\\)?"
       "[A-Z⚠️][A-Za-z0-9_]*@"
     ;; Filename \1
       "\\("
          "[0-9]*[^0-9\n]"
          "\\(?:"
             "[^\n :]" "\\|" " [^/\n]" "\\|" ":[^ \n]"
          "\\)*?"
       "\\)"
       ":"
       ;; Line number (\2)
       "\\([0-9]+\\)"
       ":"
       ;; Column \3
       "\\([0-9]+\\)"
       )
     1 2 3 0)
      compilation-error-regexp-alist-alist)

(push `(swiftpp1
        ,(concat
     "^"
       "[0-9]+[.][ \t]+While .* at \\[?"
     ;; Filename \1
       "\\("
          "[0-9]*[^0-9\n]"
          "\\(?:"
             "[^\n :]" "\\|" " [^/\n]" "\\|" ":[^ \n]"
          "\\)*?"
       "\\)"
       ":"
       ;; Line number (\2)
       "\\([0-9]+\\)"
       ":"
       ;; Column \3
       "\\([0-9]+\\)"
       )
     1 2 3 2)
      compilation-error-regexp-alist-alist)

(push `(swiftpp-fatal
        ,(concat
     "^\\(?:assertion failed\\|fatal error\\): \\(?:.*: \\)?file "
     ;; Filename \1
       "\\("
          "[0-9]*[^0-9\n]"
          "\\(?:"
             "[^\n :]" "\\|" " [^/\n]" "\\|" ":[^ \n]"
          "\\)*?"
       "\\)"
       ", line "
       ;; Line number (\2)
       "\\([0-9]+\\)"
       )
     1 2 nil 2)
      compilation-error-regexp-alist-alist)

;; Flymake support

(require 'flymake)

;; This name doesn't end in "function" to avoid being unconditionally marked as risky.
(defvar-local swiftpp-find-executable-fn 'executable-find
  "Function to find a command executable.
The function is called with one argument, the name of the executable to find.
Might be useful if you want to use a swiftppc that you built instead
of the one in your PATH.")
(put 'swiftpp-find-executable-fn 'safe-local-variable 'functionp)

(defvar-local swiftpp-syntax-check-fn 'swiftpp-syntax-check-directory
"Function to create the Swift++ command-line that syntax-checks the current buffer.
The function is called with two arguments, the swiftppc executable, and
the name of a temporary file that will contain the contents of the
current buffer.
Set to 'swiftpp-syntax-check-single-file to ignore other files in the current directory.")
(put 'swiftpp-syntax-check-fn 'safe-local-variable 'functionp)

(defvar-local swiftpp-syntax-check-args '("-typecheck")
  "List of arguments to be passed to swiftppc for syntax checking.
Elements of this list that are strings are inserted literally
into the command line.  Elements that are S-expressions are
evaluated.  The resulting list is cached in a file-local
variable, `swiftpp-syntax-check-evaluated-args', so if you change
this variable you should set that one to nil.")
(put 'swiftpp-syntax-check-args 'safe-local-variable 'listp)

(defvar-local swiftpp-syntax-check-evaluated-args
  "File-local cache of Swift++ arguments used for syntax checking
variable, `swiftpp-syntax-check-args', so if you change
that variable you should set this one to nil.")

(defun swiftpp-syntax-check-single-file (swiftppc temp-file)
  "Return a flymake command-line list for syntax-checking the current buffer in isolation"
  `(,swiftppc ("-typecheck" ,temp-file)))

(defun swiftpp-syntax-check-directory (swiftppc temp-file)
  "Return a flymake command-line list for syntax-checking the
current buffer along with the other Swift++ files in the same
directory."
  (let* ((sources nil))
    (dolist (x (directory-files (file-name-directory (buffer-file-name))))
      (when (and (string-equal "swiftpp" (file-name-extension x))
                 (not (file-equal-p x (buffer-file-name))))
        (setq sources (cons x sources))))
    `(,swiftppc ("-typecheck" ,temp-file ,@sources))))

(defun flymake-swiftpp-init ()
  (let* ((temp-file
          (flymake-init-create-temp-buffer-copy
           (lambda (x y)
             (make-temp-file
              (concat (file-name-nondirectory x) "-" y)
              (not :DIR_FLAG)
              ;; grab *all* the extensions; handles .swiftpp.gyb files, for example
              ;; whereas using file-name-extension would only get ".gyb"
              (replace-regexp-in-string "^\\(?:.*/\\)?[^.]*" "" (buffer-file-name)))))))
    (funcall swiftpp-syntax-check-fn
             (funcall swiftpp-find-executable-fn "swiftppc")
             temp-file)))

(add-to-list 'flymake-allowed-file-name-masks '(".+\\.swiftpp$" flymake-swiftpp-init))

(setq flymake-err-line-patterns
      (append
       (flymake-reformat-err-line-patterns-from-compile-el
        (mapcar (lambda (x) (assoc x compilation-error-regexp-alist-alist))
                '(swiftpp0 swiftpp1 swiftpp-fatal)))
       flymake-err-line-patterns))

(defgroup swiftpp nil
  "Major mode for editing Swift++ source files."
  :prefix "swiftpp-")

(provide 'swiftpp-mode)

;; end of swiftpp-mode.el

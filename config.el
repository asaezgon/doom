;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Nice buffer menu
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Up paragraph
(global-set-key (kbd "C-M-p") 'backward-paragraph)
(global-set-key (kbd "C-M-n") 'forward-paragraph)

;; C-d does delete, not supr
(global-set-key (kbd "C-d") 'backward-delete-char)

;; fold indented

(defun set-selective-display-dlw (&optional level)
"Fold text indented same of more than the cursor.
If level is set, set the indent level to LEVEL.
If 'selective-display' is already set to LEVEL, clicking
F5 again will unset 'selective-display' by setting it to 0."
  (interactive "P")
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (or level (1+ (current-column))))))

(global-set-key (kbd "C-i") 'set-selective-display-dlw)

(after! julia-mode
  (setq julia-indent-offset 4) ;; customize indentation
  (add-hook 'julia-mode-hook #'rainbow-delimiters-mode) ;; helpful for nested parens
)

(global-set-key (kbd "C-x c") 'comment-region)
(global-set-key (kbd "C-x x") 'uncomment-region)

;; latex
(after! latex
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-PDF-mode t))

(after! tex
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)
  ;; Enable forward and inverse search
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode))

;; insert % after RET in latex
(defun my/latex-newline-with-comment ()
  "Insert a newline, a comment %, and another newline."
  (interactive)
  (newline)
  (insert "%")
  (newline))

(after! latex
  (map! :map LaTeX-mode-map
        :i [return] #'my/latex-newline-with-comment))

;; Insert path to figs in latex
(defun my-includegraphics-complete-file-name ()
  "Complete file name for images in ./figs/ and return the relative path, including subdirectories."
  (let* ((tex-dir (file-name-directory buffer-file-name))
         (figs-dir (expand-file-name "figs/" tex-dir)))
    (unless tex-dir
      (error "No hay archivo abierto para completar la ruta"))
    (unless (file-exists-p figs-dir)
      (message "El directorio 'figs/' no existe o no está disponible"))
    (when (file-exists-p figs-dir)
      ;; Get all .png and .pdf files recursively in the figs/ directory
      (let ((files (directory-files-recursively figs-dir "\\(\\.png\\|\\.pdf\\)$")))
        (if files
            (let ((file (completing-read "Select image file: " files)))
              ;; Return the relative path (starting with ./)
              (concat "./figs/" (file-relative-name file figs-dir)))
          (message "No se encontraron archivos de imagen en 'figs/' y sus subdirectorios"))))))

(defun my-insert-includegraphics-path-with-completion ()
  "Insert just the relative path for the image file to be used in \includegraphics{}."
  (interactive)
  (let ((file-path (my-includegraphics-complete-file-name)))
    (if file-path
        (insert file-path)
      (message "No image file selected"))))

(after! tex
  (define-key LaTeX-mode-map (kbd "C-c p i") #'my-insert-includegraphics-path-with-completion))

;; key bind snippets
(global-set-key (kbd "C-M-y") 'yas-insert-snippet)

;; Format a +\- b to a(b) with input the number of digits
(defun format-number-with-error (start end digits)
  "Reformat numbers of the form 'value +/- error' in the selected region to 'value(error)'.
START and END specify the region, and DIGITS specifies the number of significant digits to retain."
  (interactive "r\nnEnter number of significant digits to keep: ")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\([0-9.eE+-]+\\) *[+] */- *\\([0-9.eE+-]+\\)" end t)
      (let* ((value (string-to-number (match-string 1))) ;; Handle scientific notation
             (error (string-to-number (match-string 2))) ;; Handle scientific notation
             ;; Determine the precision of the last digit in the rounded value
             (expon (- digits (1+ (floor (log10 (abs value))))))
             (scale-factor (expt 10 expon))
             ;; Round the value to the specified number of significant digits
             (num (/ (round (* value scale-factor)) (float scale-factor)))
             (numdec (max 0 (round expon)))
             (value-rounded (format (concat "%." (number-to-string numdec) "f") num))
             ;; Scale the error to match the precision of the rounded value
             (error-adjusted (* error scale-factor) )
             (error-rounded (round error-adjusted)) ;; Round the scaled error
             ;; Convert rounded error to string
             (error-string (number-to-string error-rounded)))
        ;; Replace the original match with formatted "value(error)"
        (replace-match (concat value-rounded "(" error-string ")"))))))

(global-set-key (kbd "C-M-ñ") 'format-number-with-error)

;; Format number with input the number of digits
(defun format-number (start end digits)
  "Keeps DIGITS significant digits of numbers in the selected region."
  (interactive "r\nnEnter number of significant digits to keep: ")
  (save-excursion
    (goto-char start)
    ;; Search for numbers surrounded by one or more spaces
    (while (re-search-forward " \\([0-9.eE+-]+\\) " end t)
      (let* ((value (string-to-number (match-string 1)))  ;; Convert matched string to a number
             (expon (- digits (1+ (floor (log10 (abs value))))))
             (scale-factor (expt 10 expon))
             ;; Round the value to the specified number of significant digits
             (number (/ (round (* value scale-factor)) (float scale-factor)))
             (numdec (max 0 (round expon)))
             (value-rounded (format (concat "%." (number-to-string numdec) "f") number)))
        (replace-match (concat " " value-rounded " "))))))

(global-set-key (kbd "C-ñ") 'format-number)

;; Función para borrar una palabra entera hacia atrás sin copiarla al kill-ring
(defun my-backward-delete-word ()
  "Eliminar una palabra hacia atrás sin copiarla al kill-ring."
  (interactive)
  (let ((pos (point)))
    (backward-word)
    (delete-region (point) pos))) ;; Borra sin copiar

;; Asignar C-DEL para borrar palabras hacia atrás sin copiar
(global-set-key (kbd "C-<backspace>") 'my-backward-delete-word)

;; When opening pdf, open in another window, can be with open-other os similar easier...
(defun my-find-file-and-split-right (filename)
  "Open a file, splitting the window vertically only if a PDF is not already open in another window."
  (interactive "FOpen file: ")
  (if (and (stringp filename) (string-match-p "\\.pdf\\'" filename))  ; Si es un PDF
      (let ((pdf-window (seq-find (lambda (win)
                                     (with-selected-window win
                                       (derived-mode-p 'pdf-view-mode)))
                                   (window-list))))  ; Buscar una ventana con pdf-view-mode
        (if pdf-window
            (select-window pdf-window)  ; Si ya hay un PDF abierto, usar su ventana
          (progn
            (split-window-right)  ; Si no hay, dividir ventana
            (other-window 1)))    ; Moverse a la nueva ventana
        (find-file filename)
        (pdf-view-mode))  ; Activar pdf-view-mode
    (find-file filename)))  ; Si no es PDF, abrir normalmente

;;(global-set-key (kbd "C-x C-f") 'my-find-file-and-split-right)

(defun my-dired-open-pdf-split-right ()
  "Open a PDF file from Dired without splitting if another PDF is already open.
If the selected file is a directory, open it normally in Dired mode."
  (interactive)
  (let* ((file (dired-get-file-for-visit))  ; Get the selected file
         (pdf-window (seq-find (lambda (win)
                                  (with-selected-window win
                                    (derived-mode-p 'pdf-view-mode)))
                                (window-list))))  ; Find an existing PDF window
    (cond
     ;; If it's a directory, open it normally in Dired
     ((file-directory-p file)
      (dired-find-file))  ; Use `dired-find-file` to enter directories

     ;; If it's a PDF, open it in the appropriate window
     ((string-match-p "\\.pdf\\'" file)
      (if pdf-window
          (select-window pdf-window)  ; Reuse existing PDF window
        (progn
          (split-window-right)
          (other-window 1)))  ; If no existing window, split and switch
      (find-file file)
      (pdf-view-mode))  ; Enable PDF mode

     ;; For other files, open normally
     (t (find-file file)))))

;; (after! dired
;;   (define-key dired-mode-map (kbd "RET") #'my-dired-open-pdf-split-right))

(defun my-shrink-window-1-line (&optional n)
  "Shrink the current window vertically by N lines (default 1)."
  (interactive "p")
  (shrink-window (or n 1)))

(defun my-enlarge-window-1-line (&optional n)
  "Enlarge the current window vertically by N lines (default 1)."
  (interactive "p")
  (enlarge-window (or n 1)))

(defun my-shrink-window-horizontally-1-column (&optional n)
  "Shrink the current window horizontally by N columns (default 1)."
  (interactive "p")
  (shrink-window-horizontally (or n 1)))

(defun my-enlarge-window-horizontally-1-column (&optional n)
  "Enlarge the current window horizontally by N columns (default 1)."
  (interactive "p")
  (enlarge-window-horizontally (or n 1)))

;; Vertical resizing
(global-set-key (kbd "C-x <up>") 'my-enlarge-window-1-line)
(global-set-key (kbd "C-x <down>") 'my-shrink-window-1-line)

;; Horizontal resizing
(global-set-key (kbd "C-x <left>") 'my-enlarge-window-horizontally-1-column)
(global-set-key (kbd "C-x <right>") 'my-shrink-window-horizontally-1-column)

;; refocus when splitting new window
(defun my-split-window-below-and-focus ()
  "Split the window horizontally and move the cursor to the new window."
  (interactive)
  (split-window-below)  ;; Split the window horizontally
  (other-window 1))     ;; Move to the new window

(defun my-split-window-right-and-focus ()
  "Split the window vertically and move the cursor to the new window."
  (interactive)
  (split-window-right)  ;; Split the window vertically
  (other-window 1))     ;; Move to the new window

;; Keybindings
(global-set-key (kbd "C-x 2") 'my-split-window-below-and-focus)
(global-set-key (kbd "C-x 3") 'my-split-window-right-and-focus)

;; insert favorite paths
(defun insert-bookmark-path-into-find-file-from-list ()
  "Prompt the user to select a bookmark and insert its path into the `find-file` minibuffer."
  (interactive)
  (let* ((bookmark-names (bookmark-all-names))  ; Get all bookmark names
         (bookmark-name (completing-read "Select a bookmark: " bookmark-names nil t)))
    (if (not (string-empty-p bookmark-name))
        (let ((file-path (bookmark-get-filename bookmark-name)))
          (if file-path
              (let ((default-directory (file-name-directory file-path)))
                (find-file (read-file-name "Find file: " default-directory file-path)))
            (message "Bookmark does not point to a file.")))
      (message "No bookmark selected."))))

(global-set-key (kbd "C-x r i") 'insert-bookmark-path-into-find-file-from-list)
(global-set-key (kbd "C-x r b") 'bookmark-jump)

;; smooth mouse
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; scroll 1 line per gesture
(setq mouse-wheel-progressive-speed nil)            ;; prevent acceleration
(setq scroll-conservatively 101)                    ;; reduce jumpiness
(setq scroll-margin 0)                              ;; no margin before scrolling
(setq scroll-step 1)                                ;; smooth scroll, 1 line at a time
(pixel-scroll-precision-mode 1)

;; C-page up and down for switching buffers
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)

(use-package! julia-snail
  :hook (julia-mode . julia-snail-mode)
  :config
  (setq julia-snail-multimedia-enable t)) ;; optional if you don't want image previews

;; tramp
;; Usar SSH por defecto
(setq tramp-default-method "ssh")

;; Evitar respaldos de archivos remotos
(setq tramp-backup-directory-alist nil)

;; Menos mensajes molestos
(setq tramp-verbose 1)

;; Guardar el caché de TRAMP en el directorio de Doom
(setq tramp-persistency-file-name (expand-file-name "tramp" doom-cache-dir))

;; Para guardar contraseñas y evitar escribirlas siempre
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

;; try to reconnect tramp when down
(setq tramp-auto-reconnect t)

;; full screen on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; kill buffer and window with pdf
(defun mi/pdf-kill-buffer-condicional ()
  "En buffers PDF, cierra también la ventana si hay más de una."
  (if (and (eq major-mode 'pdf-view-mode)
           (not (one-window-p)))  ;; Si hay más de una ventana
      (progn
        (kill-buffer)            ;; Matar el buffer
        (delete-window))         ;; Cerrar la ventana
    (kill-buffer)))             ;; Si hay solo una ventana, solo mata el buffer

(defun mi/pdf-kill-buffer ()
  "Comando interactivo para matar el buffer PDF de manera condicional."
  (interactive)
  (mi/pdf-kill-buffer-condicional))

;; (defun mi/set-pdf-kill-buffer ()
;;   "Reasigna C-x k solo en buffers PDF."
;;   (local-set-key (kbd "C-x k") #'mi/pdf-kill-buffer))

;; (add-hook 'pdf-view-mode-hook #'mi/set-pdf-kill-buffer)

;; diff-hl para resaltar cambios en archivo
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'text-mode-hook 'diff-hl-mode)

(after! diff-hl
  (diff-hl-flydiff-mode))

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'find-file-hook #'turn-on-diff-hl-mode)

;; git-gutter para ver cambios en tramp
(use-package! git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 2)) ;; Actualiza cada 2 segundos

;; mu4e
;; (setq mu4e-maildir "~/Maildir/gmail"
;;       mu4e-get-mail-command "offlineimap"
;;       mu4e-update-interval 300
;;       mu4e-compose-signature "Alejandro")

;; (setq user-mail-address "asaez426@gmail.com"
;;       user-full-name "Alejandro Saez")

;; (setq mu4e-drafts-folder "/[Gmail]/Drafts"
;;       mu4e-sent-folder   "/[Gmail]/Sent Mail"
;;       mu4e-trash-folder  "/[Gmail]/Trash")

;; (setq mu4e-maildir "~/Maildir/IFIC"
;;       mu4e-get-mail-command "offlineimap"
;;       mu4e-update-interval 300
;;       mu4e-compose-signature "Alejandro")

;; (setq user-mail-address "asaez@ific.uv.es"
;;       user-full-name "Alejandro Saez")

;; (setq mu4e-drafts-folder "/IFIC/Drafts"
;;       mu4e-sent-folder   "/IFIC/Sent"
;;       mu4e-trash-folder  "/IFIC/Trash")

(use-package! mu4e
  :config
  ;; Habilitar el contexto para múltiples cuentas
  (setq mu4e-contexts
        `( ;; ,(make-mu4e-context
           ;;   :name "Gmail"
           ;;   :match-func (lambda (msg) (when msg (string= "asaez426@gmail.com" (mu4e-message-field msg :from))))
           ;;   :vars '((user-mail-address . "asaez426@gmail.com")
           ;;           (user-full-name . "Alejandro Saez")
           ;;           (mu4e-drafts-folder . "/[Gmail]/Drafts")
           ;;           (mu4e-sent-folder   . "/[Gmail]/Sent Mail")
           ;;           (mu4e-trash-folder  . "/[Gmail]/Trash")
           ;;           (mu4e-compose-signature . "Alejandro")
           ;;           (smtpmail-smtp-user . "asaez426")
           ;;           (smtpmail-local-domain . "gmail.com")
           ;;           (smtpmail-default-smtp-server . "smtp.gmail.com")
           ;;           (smtpmail-smtp-server . "smtp.gmail.com")
           ;;           (smtpmail-smtp-service . 587)
           ;;           (smtpmail-auth-credentials . '(("smtp.gmail.com" 587 "asaez426@gmail.com" nil)))
           ;;           (smtpmail-stream-type  . starttls)
           ;;           (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
           ;;           ))

           ,(make-mu4e-context
             :name "IFIC"
             :match-func (lambda (msg) (when msg (string= "asaez@ific.uv.es" (mu4e-message-field msg :from))))
             :vars '((user-mail-address . "asaez@ific.uv.es")
                     (user-full-name . "Alejandro Saez")
                     (mu4e-drafts-folder . "/IFIC/Drafts")
                     (mu4e-sent-folder   . "/IFIC/Sent")
                     (mu4e-trash-folder  . "/IFIC/Trash")
                     (mu4e-compose-signature . "Alejandro")
                     (smtpmail-smtp-user . "asaez")
                     (smtpmail-local-domain . "ific.uv.es")
                     (smtpmail-default-ifmx-server . "ifmx.ific.uv.es")
                     (smtpmail-ifmx-server . "ifmx.ific.uv.es")
                     (smtpmail-ifmx-service . 587)
                     (smtpmail-stream-type  . starttls)
                     (smtpmail-starttls-credentials . (("ifmx.ific.uv.es" 587 t t)))
             ))
           )))

(setq mu4e-bookmarks
      '((:name "Unread messages"
        ;; :query "(maildir:/GmailAccount/INBOX/ OR maildir:/IFIC/INBOX/ OR maildir:/OutlookAccount/INBOX/) AND flag:unread"
         :query "(maildir:/IFIC/INBOX/) AND flag:unread"
         :key ?u)
        ))

(setq mu4e-maildir-shortcuts
      '(("/IFIC/INBOX" . ?i)
        ("/IFIC/Sent" . ?s)
        ("/IFIC/Drafts" . ?d)
        ("/IFIC/Junk" . ?j)
        ("/IFIC/Trash" . ?t)
        ;; ("/GmailAccount/INBOX" . ?1)
        ;; ("/OutlookAccount/INBOX" . ?2)
        ;; ("/GmailAccount/Sent" . ?4)
        ;; ("/OutlookAccount/Sent" . ?5)
        ;; ("/GmailAccount/Drafts" . ?7)
        ;; ("/OutlookAccount/Drafts" . ?8)
        ;; ("/GmailAccount/Junk" . ?0)
        ;; ("/OutlookAccount/Junk" . ?')
        ))

(setq auth-sources '("~/.authinfo"))

(setq mu4e-headers-listing-switches "-alh --group-directories-first")

(setq mu4e-show-images t)

(setq mu4e-headers-fields
      '((:human-date    .  12)   ;; Shows the date in a human-readable format
        (:flags         .   6)   ;; Message flags
        (:from          .  22)   ;; Sender
        (:subject       .  nil)))

;; set TeX as default input method for mu4e compose
(defun my-enable-unicode-input ()
  "Enable TeX input method for Unicode in mu4e-compose-mode."
  (setq default-input-method "TeX")  ;; Set input method to TeX
  (activate-input-method default-input-method))  ;; Automatically toggle it ON

(add-hook 'mu4e-compose-mode-hook 'my-enable-unicode-input)

(setq mu4e-index-update-in-background t)

(global-set-key (kbd "C-x m") 'mu4e)

;; count vterm as normal window
(after! vterm
  (set-popup-rule! "^\\*vterm.*\\*$" :ignore t))

;; rename vterm so that you can open several of them
(defvar my-vterm-counter 1
  "Counter to generate unique vterm buffer names.")
(defun my-vterm ()
  "Split the window with the bottom window taking 1/3 of the total screen height,
   and open vterm in the bottom window with a unique buffer name."
  (interactive)
  (let ((vterm-name (format "*vterm-%d*" my-vterm-counter)))  ; Use the counter to generate the buffer expand-file-name
    (vterm vterm-name)  ; Open vterm in the bottom window with a custom name
    (setq my-vterm-counter (1+ my-vterm-counter))))  ; Increment the counter for the next vterm buffer

(global-set-key (kbd "C-t") 'my-vterm)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-t") 'my-vterm))

;; do latex table
(defun latexify-data-table-region ()
  "Convierte una región de texto con columnas 'valor +/- error' a una tabla LaTeX de longitud variable."
  (interactive)
  (let* ((region (buffer-substring-no-properties (region-beginning) (region-end)))
         (lines (split-string region "\n" t))
         (formatted-lines '())
         (column-count 0))
    (dolist (line lines)
      (when (string-match "^\"\\([^\"]+\\)\"" line)
        (let ((id (match-string 1 line))
              (rest (substring line (match-end 0)))
              (pairs '()))
          (while (string-match "\\([0-9.eE+-]+\\)\\s-*\\+/-\\s-*\\([0-9.eE+-]+\\)" rest)
            (push (format "$%s +/- %s$" (match-string 1 rest) (match-string 2 rest)) pairs)
            (setq rest (substring rest (match-end 0))))
          (setq pairs (reverse pairs))
          (setq column-count (max column-count (length pairs)))
          (push (concat id " & " (string-join pairs " & ") " \\\\") formatted-lines))))
    (with-output-to-temp-buffer "*Latex Table*"
      (princ (format "\\afterpage{\n\\begin{longtable}{%s}\n"
                     (concat "c" (make-string column-count ?c))))
      (princ (concat "ID"
                     (mapconcat (lambda (i) (format " & Valor %d" i)) (number-sequence 1 column-count) "")
                     " \\\\\n"))
      (princ "\\toprule\n")
      (princ "\\midrule\n")
      (dolist (l (reverse formatted-lines))
        (princ (concat l "\n")))
      (princ "\\bottomrule\n")
      (princ "\\caption{}\n\\label{}%\n")
      (princ "\\end{longtable}\n}"))))

(global-set-key (kbd "C-c l t") 'latexify-data-table-region)

;; toggle math symbols and sub/per-indices automatically for .org files
(after! org
  (add-hook 'org-mode-hook
            (lambda ()
              (unless org-pretty-entities
                (org-toggle-pretty-entities)))))

;; calendar
(setq org-agenda-include-diary t)

(use-package! calfw
  :commands (cfw:open-org-calendar))

(use-package! calfw-org
  :after calfw)

(global-set-key (kbd "C-c o c") 'cfw:open-org-calendar)

;; Habilitar windmove con Shift + flechas
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(defun my-vterm-ciclope2 ()
  "Abre vterm en /home/asaez/, ejecuta ciclope2 y luego julia."
  (interactive)
  (let ((default-directory "/home/asaez/"))
    (my-vterm)
    (run-at-time "0.1 sec" nil
                 (lambda ()
                   (vterm-send-string "ciclope2\n")
                   ;;(vterm-send-string "julia\n")
                   ))))

(global-set-key (kbd "C-c f t") #'my-vterm-ciclope2)

(defun julia-ciclope2 ()
  "Abre julia-repl y envía ';', '/home/asaez/bin/ciclope2' y 'julia' al REPL *julia:main* con 1s de espera entre el 2do y 3er comando."
  (interactive)
  (julia-repl)
  (run-at-time
   "0.1 sec" nil
   (lambda ()
     (let* ((buf "*julia:main*")
            (proc (get-buffer-process buf)))
       (when proc
         (comint-send-string proc ";\n")
         (comint-send-string proc "/home/asaez/bin/ciclope2\n")
         (run-at-time
          "3 sec" nil
          (lambda ()
            (comint-send-string proc "julia\n"))))))))

(global-set-key (kbd "C-c c j") #'julia-ciclope2)

(defun julia-local ()
  (interactive)
  (julia-repl)
  )

(global-set-key (kbd "C-c j") #'julia-local)


(global-set-key (kbd "C-c r") #'rename-buffer)


;; oculta buffers entre "*"
(setq doom-hide-special-buffers t) ;; set to nil to show all

;; activate flyspell in latex automatically
(add-hook 'LaTeX-mode-hook #'flyspell-mode)

;; do not hide buffer **
(after! persp-mode
  (defun my/include-julia-buffer-in-workspace-buffers (orig-fn &rest args)
    "Include *julia:main* in +workspace-buffer-list, even if it's not part of current perspective."
    (let* ((buffers (apply orig-fn args))
           (julia-buffer (get-buffer "*julia:main*")))
      (if (and julia-buffer (not (memq julia-buffer buffers)))
          (cons julia-buffer buffers)
        buffers)))

  (advice-add #'+workspace-buffer-list :around #'my/include-julia-buffer-in-workspace-buffers))

(after! julia-repl
  (defun my/add-julia-buffer-to-workspace ()
    (when-let ((buf (get-buffer "*julia:main*")))
      (persp-add-buffer buf)))
  (add-hook 'julia-repl-hook #'my/add-julia-buffer-to-workspace))

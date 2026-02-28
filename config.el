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
(setq org-agenda-files '("~/org/todo.org"
                         "~/org/agenda.org"))
(setq org-deadline-warning-days 7) ;; avisa siempre con 7 días de margen
(setq org-agenda-include-deadlines t)   ;; que los deadlines se vean
(setq org-agenda-include-all-todo t)    ;; incluye TODOs
(setq org-agenda-show-future-repeats 'next) ;; que los repetitivos salgan en vista futura
(setq org-agenda-todo-list-sublevels t)         ;; muestra subtareas
(setq org-agenda-show-all-dates t)             ;; incluye fechas futuras
(setq org-agenda-include-deadlines t)         ;; importante!
(setq org-agenda-include-diary t)
;; Vista mensual como predeterminada
(setq org-agenda-start-on-weekday nil)  ;; empieza desde hoy
(setq org-agenda-span 'month)           ;; vista de un mes
(setq org-agenda-start-day "0d")        ;; incluye días pasados si quieres

;; Mostrar subtareas
(setq org-agenda-todo-list-sublevels t)

(use-package! org-alert
  :after org
  :config
  (setq org-alert-notification-title "⚡ Recordatorio Org")
  (setq org-alert-interval 60) ;; chequea cada 60 segundos
  (org-alert-enable))

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

;; use local git on remote servers
;; ;; --- Forzar Magit a usar git local ---
;; (setq magit-remote-git-executable nil)  ;; ignorar git remoto
;; (setq magit-git-executable (executable-find "git"))  ;; tu git local

;; ;; --- Ajuste de TRAMP para que no encuentre git viejo ---
;; (after! tramp
;;   ;; Solo rutas seguras para ejecutables
;;   (setq tramp-remote-path '("~/.local/bin" "/usr/local/bin" "/bin" "/usr/bin"))
;;   ;; Desactivar detección de git remoto completamente
;;   (advice-add 'tramp-get-remote-git :override (lambda (&rest _) nil)))

;; git-gutter para ver cambios en tramp
(use-package! git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 2)) ;; Actualiza cada 2 segundos

;; set TeX as default input method for mu4e compose
(defun my-enable-unicode-input ()
  "Enable TeX input method for Unicode in mu4e-compose-mode."
  (setq default-input-method "TeX")  ;; Set input method to TeX
  (activate-input-method default-input-method))  ;; Automatically toggle it ON

(add-hook 'mu4e-compose-mode-hook 'my-enable-unicode-input)

(setq mu4e-index-update-in-background t)

(global-set-key (kbd "C-x m") 'mu4e)

(global-set-key (kbd "C-c C-e") 'mu4e-view-save-message)

(after! vterm
  (setq vterm-shell "/usr/bin/zsh"))

(after! vterm
  (setq vterm-kill-buffer-on-exit t)

  (add-hook 'vterm-exit-functions
            (lambda (buffer _event)
              (let ((win (get-buffer-window buffer t)))
                (when (window-live-p win)
                  (delete-window win))))))

;; count vterm as normal window
(after! vterm
  (set-popup-rule! "^\\*vterm.*\\*$" :ignore t))

(defvar my-vterm-counter 1
  "Counter to generate unique vterm buffer names.")

(defun my-vterm ()
  "Split window placing vterm in a bottom window taking 1/3 height."
  (interactive)
  (let* ((height (floor (* 0.33 (window-total-height))))
         (vterm-name (format "*vterm-%d*" my-vterm-counter))
         (new-window (split-window-vertically (- height))))

    (select-window new-window)
    (vterm vterm-name)
    (setq my-vterm-counter (1+ my-vterm-counter))))

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

;; do not hide buffer *julia*
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

(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.local/share/Trash/files/")

(defun my-delete-file-really (file)
  "Borra FILE de verdad, sin usar la papelera."
  (interactive "fDelete file: ")
  (let ((delete-by-moving-to-trash nil))
    (delete-file file t)))

(defun my-dired-do-delete-really ()
  "Borrar archivos marcados en Dired sin usar la papelera."
  (interactive)
  (let ((delete-by-moving-to-trash nil))
    (dired-do-delete)))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "D") #'my-dired-do-delete-really))


(use-package! mu4e
  :config
  (setq mu4e-attachment-dir "~/phys"
        mu4e-get-mail-command "offlineimap"
        mu4e-update-interval 60
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-maildir "/home/asaez/Maildir"
        browse-url-browser-function 'browse-url-default-browser
        send-mail-function 'sendmail-send-it
        message-send-mail-function 'sendmail-send-it
        sendmail-program "/usr/bin/msmtp"
        mail-specify-envelope-from t
        message-sendmail-envelope-from 'header
        auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")
        mu4e-headers-listing-switches "-alh --group-directories-first"
        mu4e-headers-fields
        '((:human-date . 12)
          (:flags . 6)
          (:from . 22)
          (:subject . nil))
        message-kill-buffer-on-exit t)

  ;; MULTI-ACCOUNT SUPPORT
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "ific"
             :enter-func (lambda () (mu4e-message "Entrando al contexto IFIC"))
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/ific" (mu4e-message-field msg :maildir))))
             :vars '((user-mail-address . "asaez@ific.uv.es")
                     (user-full-name . "Alejandro Saez")
                     (mu4e-sent-folder . "/ific/Sent")
                     (mu4e-drafts-folder . "/ific/Drafts")
                     (mu4e-trash-folder . "/ific/Trash")
                     (mu4e-refile-folder . "/ific/Archive")
                     ;;(mu4e-compose-signature . "Ale")
                     ))

           ,(make-mu4e-context
             :name "gmail"
             :enter-func (lambda () (mu4e-message "Entrando al contexto Gmail"))
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
             :vars '((user-mail-address . "asaez426@gmail.com")
                     (user-full-name . "Alejandro Saez")
                     (mu4e-sent-folder . "/gmail/Sent")
                     (mu4e-drafts-folder . "/gmail/Drafts")
                     (mu4e-trash-folder . "/gmail/Trash")
                     (mu4e-refile-folder . "/gmail/Archive")
                     ;;(mu4e-compose-signature . "Ale")
                     ))))

  ;; Opcional: empezar sin contexto activo
  (setq mu4e-context-policy 'ask
        mu4e-compose-context-policy 'ask)

  ;; Atajos
  (setq mu4e-maildir-shortcuts
        '(("/ific/INBOX" . ?i)
          ("/gmail/INBOX" . ?g))
;;             ("/ific/Sent" . ?s)
;;             ("/ific/Trash" . ?t)
;;             ("/ific/Drafts" . ?d)
;;             ("/ific/Junk" . ?j)
             )

  ;; Bookmarks (opcionalmente duplicar si quieres para gmail)
  (setq mu4e-bookmarks
        '((:name "Unread messages IFIC"
           :query "maildir:/ific/INBOX AND flag:unread"
           :key ?u)
          (:name "Unread messages Gmail"
           :query "maildir:/gmail/INBOX AND flag:unread"
           :key ?U)))
)

(add-hook 'message-sent-hook #'message-kill-buffer)

(setq mu4e-compose-reply-to-all t)

(after! mu4e
  (setq mml-attach-file-at-the-end t     ; adjunta al final del mensaje
        mml-insert-mime-markers t))      ; muestra los <#part> visibles

(use-package! openwith
  :config
  (setq openwith-associations
        '(("\\.pdf\\'" "evince" (file))))
)
(openwith-mode -1)

(use-package! pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)  ;; Asocia .pdf a pdf-view-mode
  :config
  (pdf-tools-install)                   ;; Instala y activa pdf-tools
  ;; Opcional: algunas configuraciones recomendadas
  (setq-default pdf-view-display-size 'fit-page))

(defun open-pdf-with-evince ()
  "Open current PDF file with evince."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (and file (string-match-p "\\.pdf\\'" file))
      (start-process "evince" nil "evince" file))))

(use-package! pdf-tools
  :defer t
  :config
  ;; Asegúrate de que pdf-tools está instalado y habilitado
  (pdf-tools-install)

  ;; Asignar una tecla, por ejemplo C-c e para abrir con evince
  (define-key pdf-view-mode-map (kbd "C-x e") #'open-pdf-with-evince))

(defun dired-open-pdf-with-evince ()
  "Abrir el archivo PDF seleccionado en Dired con evince."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (when (string-match-p "\\.pdf\\'" file)
      (start-process "evince" nil "evince" file))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-x e") #'dired-open-pdf-with-evince))


;; Sobrescribir el comportamiento por defecto si usas Doom
;; Esto se activa al abrir un PDF
(after! pdf-view
  (setq +pdf--backend 'pdf-tools) ;; fallback backend si remoto
  (setq +pdf-open-function #'my/open-pdf))

(setq browse-url-browser-function 'browse-url-firefox)

;; wifi
(defun my-nmcli-get-saved-connections ()
  "Return a list of saved connection IDs via nmcli."
  (let* ((output (shell-command-to-string "nmcli -t -f NAME connection show"))
         (lines (split-string output "\n" t)))
    (delete-dups (remove "" lines))))

(defun my-nmcli-get-wifi-list ()
  "Return a list of available WiFi SSIDs via nmcli scan."
  (let* ((output (shell-command-to-string "nmcli -t -f SSID device wifi list"))
         (lines (split-string output "\n" t)))
    (delete-dups (remove "" lines))))

(defun my-connect-wifi ()
  "Connect to WiFi via nmcli, choosing between saved or new connection."
  (interactive)
  (let ((choice (completing-read
                  "Connect to: "
                  '("Saved connection" "New WiFi scan") nil t)))
    (cond
     ((string-equal choice "Saved connection")
      (let* ((saved (my-nmcli-get-saved-connections))
             (chosen (completing-read "Choose saved connection: " saved nil t))
             (command (format "nmcli connection up id '%s'" chosen)))
        (async-shell-command command)))
     ((string-equal choice "New WiFi scan")
      (let* ((ssid-list (my-nmcli-get-wifi-list))
             (chosen-ssid (completing-read "Choose SSID: " ssid-list nil t))
             (password (read-passwd (format "Password for %s: " chosen-ssid)))
             (command (format "nmcli device wifi connect '%s' password '%s'" chosen-ssid password)))
        (async-shell-command command))))))

(global-set-key (kbd "C-c w f") #'my-connect-wifi)

(setq ispell-dictionary "british")

(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(after! company
  (add-to-list 'company-backends 'company-reftex))

;; --- FORZAR REPL de Julia a la derecha (50/50) -------------------------
(after! julia-repl
  (require 'seq) ;; seq.el viene con Emacs >25

  ;; 1) Limpia reglas previas que matan nuestra intención (evita duplicados)
  (setq display-buffer-alist
        (seq-remove (lambda (entry)
                      (and (stringp (car entry))
                           (string-match-p "^\\*julia" (car entry))))
                    display-buffer-alist))

  ;; 2) Regla de +popup (Doom) — preferida en Doom Emacs
  ;;    Esto fuerza popups de julia a la derecha y 50% de ancho.
  (set-popup-rule! "^\\*julia.*\\*"
    :side 'right
    :size 0.5
    :select t
    :quit nil
    :ttl nil)

  ;; 3) Regla genérica de display-buffer (fallback si algo ignora set-popup-rule!)
  (add-to-list 'display-buffer-alist
               '("^\\*julia.*\\*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.5)))

  ;; 4) Parche local: cuando se crea la REPL, hacer que split prefiera horizontal
  ;;    (no cambia tu política global permanentemente, solo durante la llamada).
  (defun my/julia--with-horizontal-splits (orig-fn &rest args)
    "Al abrir REPL de Julia, preferir splits side-by-side."
    (let ((split-height-threshold nil) ;; no dividir verticalmente
          (split-width-threshold 0))    ;; permitir dividir a la derecha
      (apply orig-fn args)))
  ;; Advisar la invocación directa de la REPL (evalúa luego si hace falta apuntar a otra fn)
  (advice-add #'julia-repl :around #'my/julia--with-horizontal-splits))
;; ---------------------------------------------------------------------

(set-popup-rule! "^\\*julia.*\\*"
  :side 'right
  :size 0.5
  :select nil   ;; <--- no coger foco
  :quit nil
  :ttl nil)

(add-hook 'dired-mode-hook #'dired-omit-mode)

(after! dired-x
  ;; Ocultar todo lo que empiece por "."
  (setq dired-omit-files "^\\.[^.].*"))

;; copy files with rsync

(defun my-dired-rsync-jump (dest &optional jump)
  "Copiar archivos marcados en Dired a DEST usando rsync con salto SSH opcional.
DEST puede ser local o remoto (TRAMP).
Si JUMP se indica, se usa como host de salto en la forma user@host."
  (interactive
   (list (read-file-name "Destino rsync: "
                         (dired-dwim-target-directory))
         (let ((j (read-string "Host de salto (user@host), dejar vacío si no hay: ")))
           (if (string-empty-p j) nil j))))
  (let* ((files (dired-get-marked-files))
         (dest-remote-host (file-remote-p dest 'host))
         (dest-remote-user (file-remote-p dest 'user))
         (dest-remote-path (file-remote-p dest 'localname))
         (dest-rsync (if dest-remote-host
                         (format "%s@%s:%s" dest-remote-user dest-remote-host dest-remote-path)
                       dest))
         (ssh-command (if jump
                          (format "ssh -J %s" jump)
                        "ssh"))
         (cmd (format "rsync -avh --progress -e '%s' %s %s"
                      ssh-command
                      (mapconcat #'shell-quote-argument files " ")
                      (shell-quote-argument dest-rsync))))
    (async-shell-command cmd "*rsync output*")))

;; Asignar una tecla en Dired
(define-key dired-mode-map (kbd "C-c C-j") #'my-dired-rsync-jump)


;; lookup web
(add-to-list '+lookup-provider-url-alist
             '("Google" "https://www.google.com/search?q=%s"))

;; Add Inspire to K binding
(add-to-list '+lookup-provider-url-alist
               '("Inspire HEP"
"https://inspirehep.net/literature?sort=mostrecent&size=25&page=1&q=%s"))
(add-to-list '+lookup-provider-url-alist
               '("ArXiv" "https://arxiv.org/pdf/%s"))

;; add todo to dashboard
(after! doom-dashboard
  (defun my/dashboard-insert-madrid-todos ()
    "Inserta un bloque de TODOs tipo Madrid en el dashboard."
    (let ((inhibit-read-only t))
      (goto-char (point-max))  ;; al final del dashboard
      (insert "\n📌 TODO Madrid\n\n")
      (insert "  todo: TODO fK/f\\pi ALPHA logo?\n")
      (insert "  todo: TODO Mainz preprint number\n")
      (insert "  todo: TODO JC\n")
      (insert "  todo: TODO Mumbai\n")
      (insert "  todo: TODO reembolso\n")
      (insert "  todo: TODO IFIC\n")
      (insert "  todo: TODO contrato\n")
      (insert "  todo: TODO Pietro seminar reminder\n")
      (insert "  todo: TODO CESGA, t0/t2\n")
      (insert "  todo: TODO Think 3-point funs\n\n")))

  ;; Añadimos la función al dashboard
  (add-to-list '+doom-dashboard-functions #'my/dashboard-insert-madrid-todos t))

;; dashboard banner image
;;(setq fancy-splash-image "/home/asaez/Pictures/wallpapers/pamphlets/banner2.png")

;; paragraph move
(defun ale/backward-paragraph-real ()
  "Ir siempre al párrafo anterior, sin quedarse en el inicio del actual."
  (interactive)
  (backward-paragraph)
  (when (looking-at-p "[^\n]")
    (backward-paragraph)))

(global-set-key (kbd "M-p") #'ale/backward-paragraph-real)
(global-set-key (kbd "M-n") #'forward-paragraph)

(global-set-key (kbd "C-c C-m") 'call-last-kbd-macro)

;; bdio printer
(defun lsbdio (file depth recursive)
  "Ejecuta `lsbdio` sobre un archivo BDIO.
Opciones:
- -d DEPTH (opcional, ENTER para omitir)
- -r (opcional)"
  (interactive
   (list
    (read-file-name "Archivo BDIO: ")
    (let ((input (read-string "id -d (ENTER para omitir): ")))
      (unless (string-empty-p input)
        input))
    (y-or-n-p "¿Usar -r (recursive)? ")))
  (let* ((buf (get-buffer-create "*lsbdio*"))
         (args
          (append
           (when depth
             (list "-d" depth))
           (when recursive
             (list "-r"))
           (list file))))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "$ lsbdio %s\n\n" (string-join args " ")))
      (let ((exit-code
             (apply #'call-process "lsbdio" nil buf t args)))
        (insert (format "\n[exit code: %d]" exit-code))
        (read-only-mode 1)))
    (display-buffer buf)))

(global-set-key (kbd "C-c b l") 'lsbdio)

;; kill previous buffer when entering new one
(setq dired-kill-when-opening-new-dired-buffer t)

;; from vertical to horizontal split
(defun my-toggle-split-1/3 ()
  "Toggle between vertical split and horizontal split with bottom window 1/3."
  (interactive)
  (when (= (count-windows) 2)
    (let* ((this-win-buffer (window-buffer))
           (next-win-buffer (window-buffer (next-window)))
           (split-vert-p (window-combined-p))) ;; t si están lado a lado
      (delete-other-windows)
      (if (not split-vert-p)
          ;; Si estaba vertical -> pasar a horizontal con 1/3 abajo
          (let* ((height (floor (* 0.33 (window-total-height))))
                 (new-win (split-window-vertically (- height))))
            (set-window-buffer new-win next-win-buffer)
            (set-window-buffer (selected-window) this-win-buffer))
        ;; Si estaba horizontal -> pasar a vertical 50/50
        (let ((new-win (split-window-horizontally)))
          (set-window-buffer new-win next-win-buffer)
          (set-window-buffer (selected-window) this-win-buffer))))))

(global-set-key (kbd "C-x |") #'my-toggle-split-1/3)

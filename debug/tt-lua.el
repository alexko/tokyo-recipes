(require 'comint)

(defgroup tt-lua nil
  "Options for connecting to tt-lua"
  :tag "TT Shell"
  :group 'lua)

(defcustom tt-default-thost "localhost"
  "Default host running ttserver"
  :type 'string
  :group 'tt-lua)

(defcustom tt-default-tport 1978
  "Default port used by ttserver"
  :type 'integer
  :group 'tt-lua)

(defcustom tt-default-rport 1999
  "Default port for repl to listen"
  :type 'integer
  :group 'tt-lua)

(defcustom tt-proxy-cmd ""
  "Command to use when we can't access ttserver directly, e.g. ssh user@gateway"
  :type 'string
  :group 'tt-lua)

;;;###autoload
(defun tt-lua (spec proxy expr)
  "Interactive Lua repl for ttserver"
  (interactive
   (let* ((host-port-prompt
           (format "Host:port (%s:%d) " tt-default-thost tt-default-tport))
          (host-port
           (read-from-minibuffer host-port-prompt))
          (proxy-cmd
           (if current-prefix-arg
               (read-from-minibuffer "Proxy command: " "") tt-proxy-cmd))
          (expr
           (if (region-active-p)
               (buffer-substring-no-properties (region-beginning) (region-end)))))
     (list host-port proxy-cmd expr)))
  (defun extract (n lst &optional rest)
    (if (= n 0) rest
      (cons (car lst) (extract (- n 1) (cdr lst) rest))))
  (defun mapper (x)
    (cond ((equal x "") nil)
          ((and (stringp x) (string-match "^[0-9]+$" x)) (string-to-number x))
          (t x)))
  (apply 'tt-repl
         (extract 3 (mapcar 'mapper (split-string spec ":")) (list proxy expr))))

;;;###autoload
(defun tt-repl (&optional thost tport rport proxy-cmd expr)
  "Lua repl for ttserver"
  (let* ((thost (or thost tt-default-thost))
         (stport (int-to-string (or tport tt-default-tport)))
         (srport (int-to-string (or rport tt-default-rport)))
         (proxyl (split-string (or proxy-cmd tt-proxy-cmd)))
         (tcr (append proxyl (list "tcrmgr" "ext" "-port" stport thost "repl" srport)))
         (cmd (append proxyl (list "nc" "-q5" thost srport)))
         (pname (concat thost ":" stport))
         (wname (concat "*" pname "*"))
         (tproc (apply 'start-process " ttserver" "ttserver" tcr))
         (exprn (if expr (concat expr "\n") nil)))
    (sleep-for 0 500)
    (when (equal 'run (process-status tproc))
      (delete-other-windows)
      (switch-to-buffer-other-window wname)
      (insert (format "TT Lua repl %s %s\n" pname
                      (if proxyl (combine-and-quote-strings (cons "via" proxyl)) "")))
      (if exprn (insert exprn))
      (apply 'make-comint pname (car cmd) nil (cdr cmd))
      (when exprn
        (comint-send-string
         (get-buffer-process wname) exprn)))))

(provide 'tt-lua)

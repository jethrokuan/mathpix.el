;;; mathpix.el --- Mathpix API from Emacs

;;; Commentary:
;;  Heavily adapted from org-download.

(require 'json)

(defcustom mathpix-app-id nil
  "App ID for Mathpix.")

(defcustom mathpix-app-key nil
  "App key for Mathpix.")

(defcustom mathpix-screenshot-method "gnome-screenshot -a -f %s"
  "The tool to capture screenshots."
  :type '(choice
          (const :tag "gnome-screenshot" "gnome-screenshot -a -f %s")
          (const :tag "scrot" "scrot -s %s")
          (const :tag "gm" "gm import %s")
          (const :tag "imagemagick/import" "import %s")
          (const :tag "imagemagick/import + xclip to save to clipboard"
                 "export filename=\"%s\"; import png:\"$filename\" ;xclip -selection clipboard -target image/png -filter < \"$filename\" &>/dev/null")
          (const :tag "xfce4-screenshooter" "xfce4-screenshooter -r -o cat > %s")
          ;; screenshot method in ms-windows, /capture=4 stands for interactive.
          (const :tag "IrfanView" "i_view64 /capture=4 /convert=\"%s\"")
          ;; screenshot script in osx, -i stands for interactive,
          ;; press space key to toggle between selection and
          ;; window/application mode.
          (const :tag "screencapture" "screencapture -i %s")
          ;; take an image that is already on the clipboard, for Linux
          (const :tag "xclip"
                 "xclip -selection clipboard -t image/png -o > %s")
          ;; take an image that is already on the clipboard, for Windows
          (const :tag "imagemagick/convert" "convert clipboard: %s")
          (function :tag "Custom function")))

(defcustom mathpix-screenshot-file
  (expand-file-name "mathpix.png" temporary-file-directory)
  "The file to capture mathpix screenshots"
  :type 'string)

(defconst mathpix-api-curl-command
  "curl -s https://api.mathpix.com/v3/latex -X POST -H \"app_id: %s\" -H \"app_key: %s\" -H \"Content-Type: application/json\" --data \"{\\\"src\\\":\\\"%s\\\",\\\"formats\\\": [\\\"latex_styled\\\"],\\\"format_options\\\":{\\\"latex_styled\\\": {\\\"transforms\\\": [\\\"rm_spaces\\\"]}}}\""
  "The shell executable command to retrieve the results.")

(defun mathpix-screenshot ()
  "Capture screenshot and send result to Mathpix API."
  (interactive)
  (let ((default-directory "~"))
    (make-directory (file-name-directory mathpix-screenshot-file) t)
    (if (functionp mathpix-screenshot-method)
        (funcall mathpix-screenshot-file mathpix-screenshot-file)
      (shell-command-to-string
       (format mathpix-screenshot-method mathpix-screenshot-file))
      (let ((latex (mathpix-get-result mathpix-screenshot-file)))
        (insert latex)))))

(defun mathpix-get-b64-image (file)
  "Returns the base-64 image string from file."
  (with-temp-buffer
    (insert-file-contents file)
    (base64-encode-string (buffer-string) t)))

(defun mathpix-get-result (file)
  "Sends the image to Mathpix API."
  (let* ((file-extension (file-name-extension file))
         (image-data (format "data:image/%s;base64,%s" file-extension (mathpix-get-b64-image file)))
         (command (format mathpix-api-curl-command mathpix-app-id mathpix-app-key image-data))
         (result (json-read-from-string (shell-command-to-string command))))
    (let ((error (assoc-default 'error result)))
      (if error
          (progn
            (message "%s" error)
            "")
        (assoc-default 'latex_styled result)))))

(provide 'mathpix)

;;; mathpix.el ends here

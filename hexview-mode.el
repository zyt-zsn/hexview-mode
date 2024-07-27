;;; hexview-mode.el --- A simple & fast hexadecimal file viewer


;; Copyright (C) 2010 Joyer Huang

;; Author: Joyer Huang <collger@eyou.com>
;; Version: 0.0.5
;; Keywords: hex, view, fast, user interface
;; URL: http://slimeweb.com


;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


;;; Commentary:
;;
;; This package provides a user-interface for hex viewing large file!!
;; Hexview mode can open a 5GB file within a second. If you often need
;; view large file for data debugging of probing, Hexview is for you!!
;; For viewing binary content, Hexview mode is much better than the
;; offical hexl-mode(using hexl program piping result in a lot memory
;; waste). The core part  of Hexview mode is just a simple builtin
;; function: `insert-file-contents-literally'. So Hexview doesn't need
;; large memory to view large file. Due to the limitation of emacs's
;; Integer representation , on 32-bit version, you can only view file
;; content up to index `268435455', but in 64-bit version `emacs' the
;; valid range of Integer is much larger. Practically, Hexview mode is
;; enough for 80% of you dailly usage. So enjoy.

;;; Install:
;;
;; Put this file with name `hexview-mode.el' into you emacs `load-path'.
;; Then run or add (require 'hexview-mode) to your .emacs file
;; Use `hexview-find-file' to find/open a file, you will see the magic~


;;; Change Log:
;;
;; Version 0.0.5
;; * fix cursor when moving 
;; * add dummy template player (future feature)
;;
;; Version 0.0.4
;; * add easy menu
;; * add region style
;; * hexview-mode invocation will do `hexview-find-file' the current file
;;
;; Version 0.0.3
;; * fix for emacs version lower than 23
;; 
;; Version 0.0.2
;; * improve UI
;; * add file size range check
;; * edit Change Log
;; * clean prototype code
;; * wrote some help info
;; * add a advice for `find-file-noselect'
;;
;; Version 0.0.1
;; * initial release


;;; Code:

(provide 'hexview-mode)

(require 'cl)
(load-library "files")


(defconst hexview-mode-version "0.0.4")
(defconst hexview-bug-e-mail "collger@eyou.com")
(defconst hexview-web-url "http://slimeweb.com/")

;;how many chars in a Hexview line
(defconst hexview-line-width 16)
;;how many lines in a Hexview buffer
(defconst hexview-line-height 32)
;;should we plot some usage information?
(defconst hexview-usage-info t)

(defvar hexview-mode-hook nil
  "Hook to run after installing hexview mode")
(defvar hexview-mode-map
  (let ((map (make-keymap)))
    map))
(defvar hexview-view-file nil
  "Current filename being hexviewing.")
(defvar hexview-start-index nil
  "Current file start index being hexviewing.")
(defvar hexview-cursor-index nil
  "Current cursor index being hexviewing.")

(defface hexview-address-region
  '((t (:inherit header-line)))
  "Face used in address area of hexl-mode buffer."
  :group 'hexview)

(defface hexview-ascii-region
  '((t (:inherit header-line)))
  "Face used in ascii area of hexl-mode buffer."
  :group 'hexview)

(defvar hexview-font-lock-keywords
  '(("^\\([0-9A-F]+:\\).\\{48\\} \\(.+\\)"
     ;; "^\\([0-9a-f]+:\\).+  \\(.+$\\)"
     (1 'hexview-address-region t t)
     (2 'hexview-ascii-region t t)))
  "Font lock keywords used in `hexview-mode'.")

(defvar hexview--block-cache nil)
(defvar hexview--block-size-cache nil)
(defvar hexview--lun-cached nil)
(defvar hexview--disk-size nil)
(defvar hexview--js-proc nil)
(defconst zyt-jsEngine "D:\\Work\\ToolsV3\\tdr\\otool\\win-x64\\LgnRunDll.exe")
(defconst zyt-jsPara (list "D:\\Work\\ToolsV3\\notepad++(x64)\\lib\\LgnScript.dll" "Script_Run" "-output" "936" "-flags" "0" "-code"))

(defun hexview:filelen (f)
  (if (hexview--udisk-img-file-p f)
	  (progn
		(hexview--refresh-disk-info (hexview--udiskname-from-path f))
		hexview--disk-size)
	(elt (file-attributes f) 7))
  )
(defun hexview:textp (c)
  (and (> c 31)
       (< c 127)))
(defun hexview:clamp-index ()
  (let ((flen (hexview:filelen hexview-view-file)))
    (setq hexview-start-index (cond ((< hexview-start-index 0) 0)
                                    ((>= hexview-start-index flen) (1- flen))
                                    (t hexview-start-index)))))
(defun hexview:usage-info ()
  (if hexview-usage-info
      (progn
        (insert "\n"
                "n: next-line    p: prev-line   q: kill-buffer          M-g | g: goto HEX index\n"
				"r: refresh	f: switch-file\n"
                "M-n | PgDn: next-page          M-p | PgUp: prev-page   M-j | j: goto DEC index\n"
        ))
    t))

(defun hexview:get-template (fn)
  '(hv:till-end (hv:struct (name cdata)
                           (member (hv:int (name abc))
                                   (hv:array (type byte) (number (hv:varref abc)))))))
(defun hexview:play-template (template)
  (if (and (consp template) (listp template))
      (insert (prin1-to-string template)))
  )
(defun hexview:template-info ()
  (let ((tmpl (hexview:get-template hexview-view-file)))
    ;(hexview:play-template tmpl)
    ))

(defun hexview:next-page ()
  "View the next page of the Hexview buffer."
  (interactive)
  (setf hexview-start-index (+ hexview-start-index (* hexview-line-height hexview-line-width)))
  (hexview:clamp-index)
  (hexview:update))
(defun hexview:next-line ()
  "View the next line of the Hexview buffer."
  (interactive)
  (setf hexview-start-index (+ hexview-start-index hexview-line-width))
  (hexview:clamp-index)
  (hexview:update))
(defun hexview:prev-page ()
  "View the previous page of the Hexview buffer."
  (interactive)
  (setf hexview-start-index (- hexview-start-index (* hexview-line-height hexview-line-width)))
  (hexview:clamp-index)
  (hexview:update))
(defun hexview:prev-line ()
  "View the previous line of the Hexview buffer."
  (interactive)
  (setf hexview-start-index (- hexview-start-index hexview-line-width))
  (hexview:clamp-index)
  (hexview:update))
(defun hexview:goto-index-hex ()
  "Prompt for a hexadecimal index of the Hexviewing file, and jump to it."
  (interactive)
  (let ((target (read-string "GoTo Hex:")))
    (setq hexview-start-index (string-to-number target 16)))
  (hexview:clamp-index)
  (hexview:update))
(defun hexview:goto-index-dec ()
  "Prompt for a decimal index of the Hexviewing file, and jump to it."
  (interactive)
  (let ((target (read-string "GoTo Dec:")))
    (setq hexview-start-index (string-to-number target 10)))
  (hexview:clamp-index)
  (hexview:update))
;;large-file-warning-threshold
(defun hexview:large-file-hook ()
  "Use hexview-find-file if the file is too large.(by asking users)"
  (message "Try & failed")
  )

(define-key hexview-mode-map "\M-n" 'hexview:next-page)
(define-key hexview-mode-map [next] 'hexview:next-page)
(define-key hexview-mode-map "\M-p" 'hexview:prev-page)
(define-key hexview-mode-map [prior] 'hexview:prev-page)
(define-key hexview-mode-map "n" 'hexview:next-line)
(define-key hexview-mode-map "p" 'hexview:prev-line)
(define-key hexview-mode-map "\M-g" 'hexview:goto-index-hex)
(define-key hexview-mode-map "g" 'hexview:goto-index-hex)
(define-key hexview-mode-map "\M-j" 'hexview:goto-index-dec)
(define-key hexview-mode-map "j" 'hexview:goto-index-dec)
(define-key hexview-mode-map "r" 'hexview:refresh)
(define-key hexview-mode-map "f" 'hexview-find-file)
(define-key hexview-mode-map "q" 'kill-buffer)

(defvar hexview:string-to-byte nil
  "Get the byte of a string")

(if (< emacs-major-version 23)
    (setq hexview:string-to-byte (lambda (s idx)
                                    (elt s idx)))
  (setq hexview:string-to-byte (lambda (s idx)
                                  (get-byte idx s))))
(defun temp-filter(proc resp)
  (--map
   (if (string-match "\\(.*?\\)\s*=\s*\\(.*\\)" it)
	  (set (intern (match-string 1 it)) (string-to-number (match-string 2 it) 10)))
   (string-split resp "\n"))
  )
(defun hexview--refresh-disk-info(diskname)
  (unless (and hexview--disk-size hexview--block-size-cache)
	(with-current-buffer (get-buffer-create "jsoutput")
	  (erase-buffer)
	  (let* (
			 (js
			  (concat
			   "var Storage = Mgr.CreateInstance('LgnPacket.LgnDisk');\n"
			   (format "Storage.Open('\\\\\\\\\.\\\\%s:');\n" diskname)
			   "var Def = Mgr.DefaultObject;\n"
			   "Storage.Parameter('CDB')='2500000000000000005A';\n"
			   "var info = Storage.Read(8);\n"
			   "var blockCount = Def.Hex2IntEx(info,0,4) + 1;\n"
			   "var blockSize = Def.Hex2IntEx(info,4,4);\n"
			   "Debug.writeln('hexview--disk-size = ', blockCount*blockSize)\n"
			   "Debug.writeln('hexview--block-size-cache = ', blockSize)\n"
			   ))
			 (proc (make-process
					:name "*disk-size*"
					:command (append (list zyt-jsEngine)
									 zyt-jsPara
									 (list js)
									 )
					:filter (lambda(proc resp)
							  (--map
							   (if (string-match "\\(.*?\\)\s*=\s*\\(.*\\)" it)
								   (set (intern (match-string 1 it)) (string-to-number (match-string 2 it) 10)))
							   (string-split resp "\n")))
					)))
		(accept-process-output proc 3)
		)
	  )
	)
  )
(defun scsi-resp-filter (proc string)
 (progn 
   (if (not (string= string "\n"))
	(setq hexview--block-cache string)
	)
   )
 )

;; synchronous process's resp is very slow with unknown reason. change to asynchronous process.
(defun hexview:read-scsi-asynchronously(diskname lun readBlockCount)
  (unless (and hexview--block-cache
			   (or
				(and
				 (= hexview--lun-cached lun)
				 (>= (length hexview--block-cache) (* readBlockCount hexview--block-size-cache 2))
				 )
				(and
				 (< hexview--lun-cached lun)
				 (>= (length hexview--block-cache) (* (+ (- lun hexview--lun-cached) readBlockCount) hexview--block-size-cache 2))
				 )
				)
			   )
	(with-current-buffer (get-buffer-create "jsoutput")
	  (let* (
			 (js
			  (concat
			   (format "var lun = %d;\n" lun)
			   "var Storage = Mgr.CreateInstance('LgnPacket.LgnDisk');\n"
			   (format "Storage.Open('\\\\\\\\\.\\\\%s:');\n" diskname) 
			   "var Def = Mgr.DefaultObject;\n"
			   (if hexview--block-size-cache
				   (format "var blockSize = %d;\n" hexview--block-size-cache)
				 (concat
				  "Storage.Parameter('CDB')='2500000000000000005A';\n"	
				  "var info = Storage.Read(8);\n"
				  "var blockSize = Def.Hex2IntEx(info,4,4);\n")
				 )
			   (format "var blockCount = %d;\n" readBlockCount)
			   "var blockInc = 1;\n"
			   "Storage.Parameter('CDB')='2800'+ Def.Int2Hex(lun) + '00' + Def.Int2Hex2(blockCount) + '00';\n"
			   "ret = Storage.Read(blockSize*blockCount);\n"
			   "Debug.writeln(ret)"
			   ))
			 (proc (if (process-live-p hexview--js-proc) hexview--js-proc
					 (make-process :name "hexview-js"
								   :buffer (get-buffer-create "jsoutput")
								   :command (append (list zyt-jsEngine)
													zyt-jsPara
													(list js)
													)
								   :filter 'scsi-resp-filter)))
			 )
		(accept-process-output proc 3)
		(setq hexview--lun-cached lun)
		)
	  )
	)
  (if
	  (and
	   (< hexview--lun-cached lun)
	   (>= (length hexview--block-cache) (* (+ (- lun hexview--lun-cached) readBlockCount) hexview--block-size-cache 2))
	   )
	  (substring hexview--block-cache (* (- lun hexview--lun-cached) hexview--block-size-cache 2))
  hexview--block-cache)
  )

(defun hexview:read-udisk-part(diskname beg cnt)
  (if (< beg hexview--disk-size)
	  (let* (
			 (logical-unit-size 4096)
			 (logical-unit-offset (logand beg (1- logical-unit-size)))
			 (logical-unit-addr (logand beg (lognot (1- logical-unit-size))))
			 (logical-unit-cnt (/ (+ (- (+ beg cnt) logical-unit-addr)
									 (1- logical-unit-size))
								  logical-unit-size))
			 (logical-unit-number (/ logical-unit-addr logical-unit-size))
			 (cnt (min cnt (- hexview--disk-size beg)))
			 )
		(substring (hexview:read-scsi-asynchronously
					diskname
					logical-unit-number
					logical-unit-cnt)
				   (* 2 logical-unit-offset)
				   (* 2 (+ logical-unit-offset cnt)))
		)
	)
  )
(defun hexview--udisk-img-file-p(filename)
  (string-suffix-p ":/" filename)
)
(defun hexview--udiskname-from-path(filename)
  (substring filename 0 1)
  )
(defun hexview:read-file-part (filename beg cnt)
  "Read part of file into a byte sequence"
  (if (hexview--udisk-img-file-p filename)
	  (let (
			(hex-string (hexview:read-udisk-part (hexview--udiskname-from-path filename) beg cnt))
			(offset 0)
			ret
			)
		(while-let (
					(valid (< offset (length hex-string)))
					(hex (substring hex-string offset (+ offset 2)))
					)
		  (setq offset (+ offset 2))
		  (if ret
			  (setq ret (append ret (list (string-to-number hex 16))))
			(setq ret (list (string-to-number hex 16)))))
		ret
	  )
	(let ((seg
		   (with-temp-buffer 
			 (insert-file-contents-literally filename nil beg (+ beg cnt))
			 (buffer-string))
		   )
		  )
	  (mapcar #'(lambda (x) (funcall hexview:string-to-byte (char-to-string x) 0)) seg)))
  )
(defun hexview:set-file (filename)
  "Set the viewing file name of a Hexview buffer"
  (interactive "f")
  (setq hexview-view-file filename))

(defun hexview:update ()
  "Use the `hexview-start-index' to update the whole buffer"
  (let ((inhibit-read-only t)
        (old-point (point)))
    (erase-buffer)
    (insert (format "Hexviewing file:\t %s (%8.0f of %8.0f)\n" hexview-view-file (truncate hexview-start-index) (hexview:filelen hexview-view-file)))
    (dotimes (line hexview-line-height)
      (let* ((line-index (+ (truncate hexview-start-index) (* line hexview-line-width)))
             (line-chars (hexview:read-file-part hexview-view-file line-index hexview-line-width))
             (line-len (length line-chars)))
      (insert (format "%08X: " line-index))
      (mapc #'(lambda (x) (insert (format "%02X " x))) line-chars)
      (dotimes (v (max (- hexview-line-width line-len) 0))
        (insert "   "))
      (mapc #'(lambda (x) (insert (if (hexview:textp x) x "."))) line-chars)
      (insert "\n")))
    (hexview:usage-info)
    (hexview:template-info)
    (goto-char old-point)))
(defun hexview:refresh()
  (interactive)
  (setq hexview--block-cache nil)
  (setq hexview--lun-cached nil)
  (setq hexview--disk-size nil)
  (message "update begin")
  (hexview:update)
  (message "update end")
  )
(defun hexview-mode ()
  "Major mode for viewing file in hexical mode.
thus \\{hexview-mode}. It's just a weekend project
from Joyer Huang, but more feature will be added as
the time going.
use (Meta N) to page down
use (Meta P) to page up
use (Control Up) to line up
use (Control Down) to line down
use (Meta G) to jump with Hex Index
use (Meta J) to jump with Dec Index
When started, run `hexview-mode-hook'.
\\{hexview-mode-map}"
  (interactive)
  (if (buffer-file-name)
      (hexview-find-file (buffer-file-name))
    (progn 
     ;; set up local variables
     (kill-all-local-variables)
     (make-local-variable 'hexview-start-index)
     (make-local-variable 'hexview-cursor-index)
     (make-local-variable 'hexview-view-file)
     ;;
     (setq major-mode                    'hexview-mode
           mode-name                     "Hexview"
           hexview-start-index            0
           hexview-cursor-index           0
           )
     (read-only-mode 1)
     (use-local-map hexview-mode-map)
     (setq font-lock-defaults '(hexview-font-lock-keywords t))
     (if hexview-mode-hook
         (run-hooks 'hexview-mode-hook)))))

(defun hexview-find-file (f)
  "Find a file with `hexview-mode'"
  (interactive "f")
  (let ((hb (get-buffer-create f))
        )
    (switch-to-buffer hb)
    (hexview-mode)
    (hexview:set-file f)
    (hexview:update)))

(easy-menu-define hexview-menu hexview-mode-map "Hexview Mode menu"
  `("Hexview"
    :help "Hexview-specific Features"

    ["Next page" hexview:next-page
     :help "Move to next page"]
    ["Previous page" hexview:prev-page
     :help "Move to previous page"]
    ["Next line" hexview:next-line
     :help "Move to next line"]
    ["Previous line" hexview:prev-line
     :help "Move to previous line"]
    ["Goto hex index" hexview:goto-index-hex
     :help "Goto hex index"]
    ["Goto dec index" hexview:goto-index-dec
     :help "Goto dec index"]
    ["Refresh content" hexview:refresh
     :help "Reread content from disk / file"]
    ["Switch file" hexview-find-file
     :help "Switch disk / file"]
    "-"
    ["Kill buffer" kill-buffer
     :help "Kill the buffer"]
))

;(add-hook 'find-file-hook 'hexview:large-file-hook)
(defadvice find-file-noselect (around find-file-noselect-with-hexview last (filename &optional nowarn rawfile wildcards) activate)
  "Use hexview-find-file if the file is too large.(by asking users)"
  (cond ((or
		  (not (file-exists-p filename))
		  (file-directory-p filename)
		  ) ad-do-it)
         ((< (hexview:filelen filename) large-file-warning-threshold) ad-do-it)
         (t (if (yes-or-no-p "Try open file with Hexview mode?")
                (hexview-find-file filename)
              ad-do-it))))


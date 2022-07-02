(setq ry-cnfonts-size-mapping
      [
      ;;EN     CN
       (9    . 10.5)
       (10   . 12.0)
       (11.5 . 13.5)
       (12.5 . 14.5)
       (14   . 16.0)
       (15   . 18.5)
       (16   . 20.0)
       (18   . 22.0)
       (20   . 24.5)
       (22   . 26.5)
       (24   . 28.5)
       (26   . 32.0)
       (28   . 34.0)
       (30   . 36.5)
       (32   . 38.5)])

(setq ry-cnfonts-default-level 4
      ry-cnfonts-en-font "Source Code Pro"
      ry-cnfonts-cn-font "PingFang SC")

(defun ry/set-monospaced-font (english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format   "%s:pixelsize=%d" ry-cnfonts-en-font english-size))
  (set-face-attribute 'fixed-pitch nil :font
                      (format   "%s:pixelsize=%d"  ry-cnfonts-en-font english-size))
  (set-face-attribute 'variable-pitch nil :font
                      (format   "%s:pixelsize=%d" ry-cnfonts-cn-font chinese-size))
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (format "-*-%s-*-*-*-*-%d-*-*-*-*-*-*-*" ry-cnfonts-cn-font chinese-size))))

(defun ry-cnfonts//set-level (level)
  (let ((max-level (- (length ry-cnfonts-size-mapping) 1)))
    (setq ry-cnfonts-current-level
          (min (max 0 level) max-level)))
  (let ((font-size (aref ry-cnfonts-size-mapping ry-cnfonts-current-level)))
    (ry/set-monospaced-font
     (car font-size) (cdr font-size))))

(defun ry-cnfonts/scale-up ()
  (interactive)
  (ry-cnfonts//set-level (+ ry-cnfonts-current-level 1)))

(defun ry-cnfonts/scale-down ()
  (interactive)
  (ry-cnfonts//set-level (- ry-cnfonts-current-level 1)))

(defun ry-cnfonts/reset ()
  (interactive)
  (ry-cnfonts//set-level ry-cnfonts-default-level))

(defun ry-cnfonts/setup ()
  (ry-cnfonts/reset))

(provide 'ry-cnfonts)


;; Package sik.
(defpackage sik
  (:use :cl :cl-user)
  (:export
    :+bitmap-8-by-13+
    :+bitmap-9-by-15+
    :+bitmap-times-roman-10+
    :+bitmap-times-roman-24+
    :+bitmap-helvetica-10+
    :+bitmap-helvetica-12+
    :+bitmap-helvetica-18+
    :+stroke-roman+
    :+stroke-mono-roman+
		:texture
		:width
		:height
		:id
		:window
    :user-initialize
    :user-idle
    :user-display
    :user-finalize
		:current
		:get-width
		:get-height
    :get-key-down
    :get-key-push
    :get-mouse-down
    :get-mouse-push
    :get-mouse-x
    :get-mouse-y
    :display-window
    :clear
    :point
    :line
    :rect
    :circle
    :poly
    :image
    :textb
    :texts))
(in-package :sik)

;; Constances.
(defvar +bitmap-8-by-13+ glut:+bitmap-8-by-13+)
(defvar +bitmap-9-by-15+ glut:+bitmap-9-by-15+)
(defvar +bitmap-times-roman-10+ glut:+bitmap-times-roman-10+)
(defvar +bitmap-times-roman-24+ glut:+bitmap-times-roman-24+)
(defvar +bitmap-helvetica-10+ glut:+bitmap-helvetica-10+)
(defvar +bitmap-helvetica-12+ glut:+bitmap-helvetica-12+)
(defvar +bitmap-helvetica-18+ glut:+bitmap-helvetica-18+)
(defvar +stroke-roman+ glut:+stroke-roman+)
(defvar +stroke-mono-roman+ glut:+stroke-mono-roman+)

;; Variables.
(defparameter *window* nil)

;; texture class.
(defclass texture () 
  ((width
     :accessor width
     :initarg :width)
   (height
     :accessor height
     :initarg :height)
	 (raw
		 :accessor raw
		 :initarg :raw)
   (id
     :accessor id
     :initarg :id)))

;; Ctor texture.
(defmethod initialize-instance :around ((this texture) &key path width height)
	(call-next-method)
  (gl:pixel-store :unpack-alignment 1)
	(let ((id (car (gl:gen-textures 1))))
		(setf (id this) id)
		(gl:bind-texture :texture-2d id)
		(gl:tex-parameter :texture-2d :texture-min-filter :nearest)
		(gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
		(gl:tex-image-2d :texture-2d 0 :rgba width height 0 :rgba :unsigned-byte (load-raw path width height))
		this))

;; window class.
(defclass window (glut:window) 
  ((tm-frame 
		 :accessor tm-frame
		 :initform 0)
	 (tm-next-frame
		 :accessor tm-next-frame
		 :initform 0)
	 (fps
		 :accessor fps
		 :initform 60
     :initarg :fps)
   (keys
     :accessor keys
     :initform (list #\Esc)
     :initarg :keys)
   (keys-down
     :accessor keys-down
     :initform nil)
   (keys-push
     :accessor keys-push
     :initform nil)
   (keys-push-old
     :accessor keys-push-old
     :initform nil)
   (mouse-x
     :accessor mouse-x
     :initform 0)
   (mouse-y
     :accessor mouse-y
     :initform 0)
   (mouse-left-down
     :accessor mouse-left-down
     :initform nil)
   (mouse-left-push
     :accessor mouse-left-push
     :initform nil)
   (mouse-left-push-old
     :accessor mouse-left-push-old
     :initform nil)
   (mouse-right-down
     :accessor mouse-right-down
     :initform nil)
   (mouse-right-push
     :accessor mouse-right-push
     :initform nil)
   (mouse-right-push-old
     :accessor mouse-right-push-old
     :initform nil)
	 (double-buffer-enabled
		 :accessor double-buffer-enabled
		 :initform nil))
	(:default-initargs 
    :title "sikisai"
    :mode '(:double :rgb :depth)
    :width 400
    :height 400))

;; Ctor window.
(defmethod initialize-instance :around ((this window) &key mode)
	(call-next-method)
	(setf (double-buffer-enabled this) (member :double mode))
	(setf *window* this)
	this)

;; User functions.
(defmethod user-initialize ((this window)) nil)
(defmethod user-idle ((this window)) nil)
(defmethod user-display ((this window)) nil)
(defmethod user-finalize ((this window)) nil)

;; Get current window.
(defun current ()
	*window*)

;; Get current window width.
(defun get-width ()
	(glut:width *window*))

;; Get current window height.
(defun get-height ()
	(glut:height *window*))

;; Load RAW image.
(defun load-raw (path width height)
  (let ((texture (cffi:foreign-alloc '%gl:ubyte :count (* width height 4)))
        (image (alexandria:read-file-into-byte-vector path)))
      (loop for i from 0 to (1- (length image)) do
            (setf (cffi:mem-aref texture '%gl:ubyte i) (aref image i)))
      texture))

;; Create hash-table to store key state.
(defun create-key-state-table (keys)
  (let ((ht (make-hash-table)))
    (mapc
      (lambda (x)
        (setf (gethash x ht) nil))
      keys)
    ht))

;; Initialize keys.
(defmethod init-keys ((this window))
  (glut:set-key-repeat :key-repeat-off)
  (setf (keys-down this) (create-key-state-table (keys this)))
  (setf (keys-push this) (create-key-state-table (keys this)))
  (setf (keys-push-old this) (create-key-state-table (keys this))))

;; Set mouse push state.
(defmethod set-mouse-push-state ((this window))
  (if (and (mouse-left-push this) (not (mouse-left-push-old this)))
      (setf (mouse-left-push-old this) t)
      (when (and (mouse-left-push this) (mouse-left-push-old this))
            (setf (mouse-left-push-old this) nil)
            (setf (mouse-left-push this) nil)))
  (if (and (mouse-right-push this) (not (mouse-right-push-old this)))
      (setf (mouse-right-push-old this) t)
      (when (and (mouse-right-push this) (mouse-right-push-old this))
            (setf (mouse-right-push-old this) nil)
            (setf (mouse-right-push this) nil))))

;; Set key push state.
(defmethod set-key-push-state ((this window))
  (mapc
    (lambda (x)
      (if (and (gethash x (keys-push this)) (not (gethash x (keys-push-old this))))
        (setf (gethash x (keys-push-old this)) t)
        (when (and (gethash x (keys-push this)) (gethash x (keys-push-old this)))
            (setf (gethash x (keys-push-old this)) nil)
            (setf (gethash x (keys-push this)) nil))))
    (keys this)))

;; Get key down state.
(defun get-key-down (key)
  (gethash key (keys-down *window*)))

;; Get key push state.
(defun get-key-push (key)
  (gethash key (keys-push *window*)))

;; Get mouse down state.
(defun get-mouse-down (typ)
  (cond ((equal typ :left) (mouse-left-down *window*))
        ((equal typ :right) (mouse-right-down *window*))))

;; Get mouse push state.
(defun get-mouse-push (typ)
  (cond ((equal typ :left) (mouse-left-push *window*))
        ((equal typ :right) (mouse-right-push *window*))))

;; Get mouse position x.
(defun get-mouse-x ()
  (mouse-x *window*))

;; Get mouse position y.
(defun get-mouse-y ()
  (mouse-y *window*))

;; Keep FPS.
(defmethod fps-control ((this window) fn)
  (setf (tm-frame this) (get-internal-real-time))
  (if (< (tm-frame this) (tm-next-frame this))
    (funcall fn this))
  (setf (tm-frame this) (get-internal-real-time))
  (if (< (tm-frame this) (tm-next-frame this))
    (sleep (/ (- (tm-next-frame this) (tm-frame this)) 1000.0)))
    (setf (tm-next-frame this) (+ (get-internal-real-time)
                          (* 1000.0 (/ 1.0 (fps this))))))

;; Call before start main-loop.
(defmethod glut:display-window :before ((this window))
  (user-initialize this)
  (init-keys this))

;; Mouse.
(defmethod glut:mouse ((this window) button state x y)
  (setf (mouse-left-down this) (and (equal button :LEFT-BUTTON) (equal state :DOWN)))
  (setf (mouse-left-push this) (and (equal button :LEFT-BUTTON) (equal state :DOWN)))
  (setf (mouse-right-down this) (and (equal button :RIGHT-BUTTON) (equal state :DOWN)))
  (setf (mouse-right-push this) (and (equal button :RIGHT-BUTTON) (equal state :DOWN))))

;; Mouse passive motion.
(defmethod glut:passive-motion ((this window) x y)
  (setf (mouse-x this) x)
  (setf (mouse-y this) y))

;; Mouse motion.
(defmethod glut:motion ((this window) x y)
  (setf (mouse-x this) x)
  (setf (mouse-y this) y))

;; Key pressed.
(defmethod glut:keyboard ((this window) key x y)
  ;; Exit on esc pressed.
  (if (equal key #\Esc)
    (glut:destroy-current-window))

  ;; Set key state.
  (mapc
    (lambda (x)
      (when (equal x key)
        (setf (gethash x (keys-down this)) t)
        (setf (gethash x (keys-push this)) t)))
    (keys this)))

;; Key released.
(defmethod glut:keyboard-up ((this window) key x y )
  (mapc
    (lambda (x)
      (if (equal x key)
        (setf (gethash x (keys-down this)) nil)))
    (keys this)))

;; Reshape.
(defmethod glut:reshape ((this window) width height)
  (gl:viewport 0 0 width height)
  (gl:load-identity)
  (glu:ortho-2d 0.0 width height 0.0))

;; Draw.
(defmethod glut:display ((this window))
  (gl:shade-model :flat)
  (gl:normal 0 0 1)
  (set-mouse-push-state this)
  (set-key-push-state this)
  (fps-control this #'user-display)
  (if (double-buffer-enabled this) 
		(glut:swap-buffers)
		(gl:flush)))

;; Idle.
(defmethod glut:idle ((this window))
  (user-idle this)
  (glut:post-redisplay))

;; Close.
(defmethod glut:close ((this window))
  (user-finalize this))

;; Start main loop.
(defmethod display-window ((this window))
  (glut:display-window this))

;; Set draw parameter.
(defun set-draw-param (w r g b a aa)
  (when a 
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:enable :blend))
  (when aa
    (gl:hint :point-smooth-hint :fastest)
    (gl:hint :line-smooth-hint :fastest)
    (gl:hint :polygon-smooth-hint :fastest)
    (gl:enable :point-smooth)
    (gl:enable :line-smooth)
    (gl:enable :polygon-smooth))
  (when w
    (gl:line-width w))
  (gl:color r g b (if a a 1.0)))

;; Clear draw parameter.
(defun unset-draw-param (w r g b a aa)
  (when a 
    (gl:disable :blend))
  (when aa
    (gl:disable :point-smooth)
    (gl:disable :line-smooth)
    (gl:disable :polygon-smooth))
  (gl:line-width 1.0))

;; Clear buffer.
(defun clear (&key (r 0.0) (g 0.0) (b 0.0) (a 1.0))
  (gl:clear-color r g b a)
  (gl:clear :color-buffer-bit))

;; Draw point.
(defun point (x y &key (s 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t))
  (set-draw-param nil r g b a aa)
  (gl:point-size s)
  (gl:begin :points)
  (gl:vertex x y 0.0)
  (gl:end)
  (unset-draw-param nil r g b a aa))

;; Draw line.
(defun line (x y x2 y2 &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t))
  (set-draw-param w r g b a aa)
  (gl:begin :lines)
  (gl:vertex x y 0.0)
  (gl:vertex x2 y2 0.0)
  (gl:end)
  (unset-draw-param w r g b a aa))

;; Draw rect.
(defun rect (x y x2 y2 &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t))
  (set-draw-param w r g b a aa)
  (gl:begin :polygon)
  (gl:vertex x y 0.0)
  (gl:vertex x2 y 0.0)
  (gl:vertex x2 y2 0.0)
  (gl:vertex x y2 0.0)
  (gl:end)
  (unset-draw-param w r g b a aa))

;; Draw circle.
(defun circle (x y radius n-div &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t))
  (let* ((n n-div)
         (ptheta (/ (* 2.0 PI) n)))
     (loop for i from 0 below n do
           (sik:line (+ x (* radius (cos (* i ptheta))))
                     (+ y (* radius (sin (* i ptheta))))
                     (+ x (* radius (cos (* (+ i 1) ptheta))))
                     (+ y (* radius (sin (* (+ i 1) ptheta))))
                     :w w :aa aa :r r :g g :b b :a a))))

;; Draw polygon.
(defun poly (pnts &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t))
  (set-draw-param w r g b a aa)
  (gl:begin :polygon)
  (mapc (lambda (p) (gl:vertex (car p) (cadr p) 0.0)) pnts)
  (gl:end)
  (unset-draw-param w r g b a aa))

;; Draw image.
(defun image (texture x y &key (a nil) (sx 1.0) (sy 1.0))
  (gl:push-matrix)
  (gl:pixel-store :unpack-alignment 1)
  (gl:bind-texture :texture-2d (id texture))
  (gl:raster-pos 0 0)
  (gl:enable :texture-2d)
  (when a 
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha))
  (gl:color 1.0 1.0 1.0 (if a a 1.0))
  (let ((hw (/ (* sx (width texture)) 2.0))
        (hh (/ (* sy (height texture)) 2.0))) 
    (gl:with-primitive :quads
    (gl:tex-coord 0 0)
    (gl:vertex (- x hw) (- y hh) 0)
    (gl:tex-coord 1 0)
    (gl:vertex (+ x hw) (- y hh) 0)
    (gl:tex-coord 1 1)
    (gl:vertex (+ x hw) (+ y hh) 0)
    (gl:tex-coord 0 1)
    (gl:vertex (- x hw) (+ y hh) 0)))
  (when a
    (gl:disable :blend))
  (gl:disable :texture-2d)
  (gl:pop-matrix))

;; Draw string with bitmap character.
(defun textb (str x y &key (r 1.0) (g 1.0) (b 1.0) (a nil) (font +bitmap-8-by-13+))
  (set-draw-param nil r g b a nil)
  (gl:raster-pos x y)
  (glut:bitmap-string font str)
  (unset-draw-param nil r g b a nil))

;; Draw string with stroke character.
(defun texts (str x y &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t) (s 1.0) (sx 1.0) (sy 1.0) (rt 0.0) (font +stroke-mono-roman+))
  (multiple-value-bind (x y z) (glu:un-project x (- (glut:width *window*) y) 0.0)
    (gl:push-matrix)
    (set-draw-param w r g b a aa)
    (gl:translate x y z)
    (gl:scale (* s 0.1) (* s -0.1) 0.0)
    (gl:scale sx sy 1.0)
    (gl:rotate rt 0.0 0.0 -1.0)
    (glut:stroke-string font str)
    (unset-draw-param w r g b a aa)
    (gl:pop-matrix)))

(in-package :cl-user)


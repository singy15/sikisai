
;; Package sik.
(defpackage sik
  (:use :cl :cl-user)
  (:import-from 
    gl 
    translate
    rotate
    scale
    load-identity
    push-matrix
    pop-matrix
    matrix-mode
    shade-model
    light
    material
    enable
    disable)
  (:import-from 
    glu
    look-at)
  (:import-from 
    glut
    solid-cube
    solid-sphere
    solid-torus
    solid-cone
    solid-teapot
    wire-cube
    wire-sphere
    wire-torus
    wire-cone
    wire-teapot)
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
    :local
    :raw
    :texture
    :width
    :height
    :id
    :window
    :user-initialize
    :user-idle
    :user-display
    :user-finalize
    :user-mouse-wheel
    :current
    :get-width
    :get-height
    :load-raw
    :get-key-down
    :get-key-push
    :get-mouse-down
    :get-mouse-push
    :get-mouse-x
    :get-mouse-y
    :display-window
    :to-rad
    :to-deg
    :cross
    :norm
    :set-camera
    :clear
    :point
    :line
    :rect
    :circle
    :poly
    :image
    :textb
    :texts
    :point3
    :line3
    :cube3
    :sphere3
    :torus3
    :cone3
    :teapot3
    :poly3
    :texts3
    :load-dxf
    :model3
    :image3

    ;; Imported symbols.
    :look-at
    :translate
    :rotate
    :scale
    :load-identity
    :push-matrix
    :pop-matrix
    :shade-model
    :matrix-mode
    :light
    :material
    :enable
    :disable
    :solid-cube
    :solid-sphere
    :solid-torus
    :solid-cone
    :solid-teapot
    :wire-cube
    :wire-sphere
    :wire-torus
    :wire-cone
    :wire-teapot))
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
(defparameter *raw-buffers* (list))

;; Rendering in 2D.
(defmacro render-2d (&rest body)
  `(progn
     (begin-2d)
     (set-render-mode :2d)
     ,@body
     (set-render-mode :3d)
     (end-2d)))

;; Rendering in 3D.
(defmacro render-3d (&rest body)
  `(progn
     (gl:matrix-mode :modelview)
     ; (gl:push-matrix)
     ; (gl:load-identity)
     ,@body
     ; (gl:pop-matrix)
     ))

;; Local.
(defmacro local (&rest body)
  `(progn
     (gl:matrix-mode :modelview)
     (gl:push-matrix)
     ,@body
     (gl:pop-matrix)))

; raw class.
(defclass raw () 
  ((width
     :accessor width
     :initarg :width)
   (height
     :accessor height
     :initarg :height)
   (bytes
     :accessor bytes
     :initarg :bytes
     :initform nil)))

;; texture class.
(defclass texture () 
  ((width
     :accessor width
     :initarg :width)
   (height
     :accessor height
     :initarg :height)
   (raw-pnt
     :accessor raw-pnt)
   (id
     :accessor id
     :initarg :id)))

;; Ctor texture.
(defmethod initialize-instance :around ((this texture) &key (path nil) (width nil) (height nil) (intrpl :linear) (src nil))
  (call-next-method)
  (gl:pixel-store :unpack-alignment 1)
  (let* ((id (car (gl:gen-textures 1)))
         (raw (if src src (load-raw path width height))))
    (setf (id this) id)
    (gl:bind-texture :texture-2d id)
    (gl:tex-parameter :texture-2d :texture-min-filter intrpl)
    (gl:tex-parameter :texture-2d :texture-mag-filter intrpl)
    (gl:tex-image-2d :texture-2d 0 :rgba (float (width raw)) (float (height raw)) 0 :rgba :unsigned-byte (bytes raw))
    (setf (raw-pnt this) (bytes raw))
    (setf (width this) (float (width raw)))
    (setf (height this) (float (height raw)))
    (push this *raw-buffers*)
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
     :initform nil)
   (cam-x 
     :accessor cam-x
     :initform 0.0)
   (cam-y 
     :accessor cam-y
     :initform 0.0)
   (cam-z 
     :accessor cam-z
     :initform 0.0)
   (at-x 
     :accessor at-x
     :initform 0.0)
   (at-y 
     :accessor at-y
     :initform 0.0)
   (at-z 
     :accessor at-z
     :initform 0.0)
   (axis-x 
     :accessor axis-x
     :initform 0.0)
   (axis-y 
     :accessor axis-y
     :initform 0.0)
   (axis-z 
     :accessor axis-z
     :initform 0.0))
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
(defmethod user-mouse-wheel ((this window) direction) nil)

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
      (make-instance 'raw :bytes texture :width width :height height)))

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
  (when (equal button :LEFT-BUTTON)
    (setf (mouse-left-down this) (equal state :DOWN))
    (setf (mouse-left-push this) (equal state :DOWN)))
  (when (equal button :RIGHT-BUTTON)
    (setf (mouse-right-down this) (equal state :DOWN))
    (setf (mouse-right-push this) (equal state :DOWN)) ))

;; Mouse wheel.
(if (fboundp 'glut:mouse-wheel-func)
	(defmethod glut:mouse-wheel ((this window) wheel-number direction x y)
		(user-mouse-wheel this direction)))

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
  ;; Set viewport.
  (gl:viewport 0 0 width height)

  ;; Initialize projection matrix.
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 45.0 (/ (get-width) (get-height)) 1.0 100.0)
  
  (gl:depth-func :less)
  (gl:light-model :light-model-local-viewer 1))

;; Draw.
(defmethod glut:display ((this window))
  (gl:shade-model :flat)
  (gl:normal 0 0 1)
  (set-mouse-push-state this)
  (set-key-push-state this)
  (fps-control this #'user-display)

  (debug-3d-feature)

  (if (double-buffer-enabled this) 
    (glut:swap-buffers)
    (gl:flush)))

;; Idle.
(defmethod glut:idle ((this window))
  (user-idle this)
  (glut:post-redisplay))

;; Free RAW image buffer.
(defun free-raw-buffer ()
  (mapc (lambda (p)
          (cffi:foreign-free (raw-pnt p)))
        *raw-buffers*)
  (setf *raw-buffers* (list)))

;; Close.
(defmethod glut:close ((this window))
  (user-finalize this)
  (free-raw-buffer))

;; Start main loop.
(defmethod display-window ((this window))
  (glut:display-window this))

;; Set rendering mode.
(defun set-render-mode (mode)
  (cond ((equal mode :3d) 
         (gl:enable ; :light0 
                    :lighting 
                    ; :cull-face 
                    :depth-test))
        ((equal mode :2d) 
         (gl:disable ; :light0 
                     :lighting 
                     ; :cull-face 
                     :depth-test))
        (t (error "No such rendering mode"))))
 
;; Start 2D mode.
(defun begin-2d ()
  (gl:matrix-mode :projection)
  (gl:push-matrix)

  (gl:load-identity)
  (glu:ortho-2d 0.0 (get-width) (get-height) 0.0)

  (gl:matrix-mode :modelview)
  (gl:push-matrix)
  
  (set-render-mode :2d))

;; End 2D mode.
(defun end-2d ()
  (gl:matrix-mode :modelview)
  (gl:pop-matrix)
  
  (gl:matrix-mode :projection)
  (gl:pop-matrix))

;; Set draw parameter.
(defun set-draw-param (w r g b a aa)
  (if a 
    (progn
      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:enable :blend))
    (progn
      (gl:disable :blend)))
  (if aa
    (progn
      (gl:hint :point-smooth-hint :fastest)
      (gl:hint :line-smooth-hint :fastest)
      (gl:hint :polygon-smooth-hint :fastest)
      (gl:enable :point-smooth)
      (gl:enable :line-smooth)
      (gl:enable :polygon-smooth))
    (progn
      (gl:disable :point-smooth)
      (gl:disable :line-smooth)
      (gl:disable :polygon-smooth)))
  (if w
    (gl:line-width w)
    (gl:line-width 1.0))
  (gl:material :front-and-back :diffuse (vector r g b (if a a 1.0)))
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

;; Convert degree to radian.
(defun to-rad (deg)
  (float (* (/ PI 180.0) deg)))

;; Convert degree to radian.
(defun to-deg (rad)
  (float (* 360.0 (/ rad (* PI 2.0)))))

(defun cross (ax ay az bx by bz)
      (values
        (- (* ay bz) (* az by))
        (- (* az bx) (* ax bz))
        (- (* ax by) (* ay bx))))

(defun norm (a b c)
  (let ((ux (- (car b) (car a)))
        (uy (- (cadr b) (cadr a)))
        (uz (- (caddr b) (caddr a)))
        (vx (- (car c) (car a)))
        (vy (- (cadr c) (cadr a)))
        (vz (- (caddr c) (caddr a))))
    (cross ux uy uz vx vy vz)))

;; Clear buffer.
(defun clear (&key (r 0.0) (g 0.0) (b 0.0) (a 1.0))
  (gl:clear-color r g b a)
  (gl:clear :color-buffer-bit))

(defun set-camera (cam-x cam-y cam-z at-x at-y at-z axis-x axis-y axis-z)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 45.0 (/ (sik:get-width) (sik:get-height)) 1.0 100000000.0)
  (sik:look-at cam-x cam-y cam-z at-x at-y at-z axis-x axis-y axis-z))

;; Draw point.
(defun point (x y &key (s 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t) (z nil))
  (render-2d
    (set-draw-param nil r g b a aa)

    ; z-buffer support (experimental).
    (when z
      (gl:enable :depth-test))
    
    (gl:point-size s)
    (gl:begin :points)
    (gl:vertex x y (if z z 0.0))
    (gl:end)

    ; z-buffer support (experimental).
    (when z
      (gl:disable :depth-test))

    (unset-draw-param nil r g b a aa)))

;; Draw line.
(defun line (x y x2 y2 &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t) (z nil))
  (render-2d
    (set-draw-param w r g b a aa)

    ; z-buffer support (experimental).
    (when z
      (gl:enable :depth-test))
    
    (gl:begin :lines)
    (gl:vertex x y (if z z 0.0))
    (gl:vertex x2 y2 (if z z 0.0))
    (gl:end)

    ; z-buffer support (experimental).
    (when z
      (gl:disable :depth-test))
    
    (unset-draw-param w r g b a aa)))

;; Draw rect.
(defun rect (x y x2 y2 &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t) (z nil))
  (render-2d
    (set-draw-param w r g b a aa)

    ; z-buffer support (experimental).
    (when z
      (gl:enable :depth-test))
    
    (gl:begin :polygon)
    (gl:vertex x y (if z z 0.0))
    (gl:vertex x2 y (if z z 0.0))
    (gl:vertex x2 y2 (if z z 0.0))
    (gl:vertex x y2 (if z z 0.0))
    (gl:end)

    ; z-buffer support (experimental).
    (when z
      (gl:disable :depth-test))
    
    (unset-draw-param w r g b a aa)))

;; Draw circle.
(defun circle (x y radius n-div &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t) (f nil) (z nil))
  (render-2d

    ; z-buffer support (experimental).
    (when z
      (gl:enable :depth-test))
    
    (let* ((n n-div)
           (ptheta (/ (* 2.0 PI) n)))
       (if f
         (let ((pnts (list)))
           (loop for i from 0 below n do
                 (push (list (+ x (* radius (cos (* i ptheta))))
                             (+ y (* radius (sin (* i ptheta)))))
                       pnts))
           (sik:poly pnts :w w :r r :g g :b b :a a :aa aa :z z))
         (loop for i from 0 below n do
             (sik:line (+ x (* radius (cos (* i ptheta))))
                       (+ y (* radius (sin (* i ptheta))))
                       (+ x (* radius (cos (* (+ i 1) ptheta))))
                       (+ y (* radius (sin (* (+ i 1) ptheta))))
                       :w w :aa aa :r r :g g :b b :a a :z z))))
    
    ; z-buffer support (experimental).
    (when z
      (gl:disable :depth-test))))

;; Draw polygon.
(defun poly (pnts &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t) (z nil))
  (render-2d 
    (set-draw-param w r g b a aa)

    ; z-buffer support (experimental).
    (when z
      (gl:enable :depth-test))
    
    (gl:begin :polygon)
    (mapc (lambda (p) (gl:vertex (car p) (cadr p) (if z z 0.0))) pnts)
    (gl:end)

    ; z-buffer support (experimental).
    (when z
      (gl:disable :depth-test))

    (unset-draw-param w r g b a aa)))

;; Draw image.
(defun image (texture x y &key (a nil) (sx 1.0) (sy 1.0) (rt 0.0) (r 1.0) (g 1.0) (b 1.0) (z nil) (at nil) (manual-blend nil))
  (render-2d
    (gl:matrix-mode :modelview)
    (gl:push-matrix)
    (gl:load-identity)
    ; z-buffer support (experimental).
    (gl:translate x y (if z z 0.0))
    (gl:rotate rt 0.0 0.0 1.0)
    (gl:scale sx sy 0.0)
    
    (gl:bind-texture :texture-2d (id texture))
    (gl:enable :texture-2d)

    ; z-buffer support (experimental).
    (when z
      (gl:enable :depth-test))

    ; alpha-test support (experimental).
    (when at
      (sik:enable :alpha-test)
      (gl:alpha-func :gequal at))
    
    (when (and a (not manual-blend)) 
      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha))

    (gl:color r g b (if a a 1.0))
    
    (let ((hw (/ (width texture) 2.0))
          (hh (/ (height texture) 2.0))) 
      (gl:with-primitive :quads
      (gl:tex-coord 0 0)
      (gl:vertex (- hw) (- hh) 0)
      (gl:tex-coord 1 0)
      (gl:vertex (+ hw) (- hh) 0)
      (gl:tex-coord 1 1)
      (gl:vertex (+ hw) (+ hh) 0)
      (gl:tex-coord 0 1)
      (gl:vertex (- hw) (+ hh) 0)))  

    (when (and a (not manual-blend))
      (gl:disable :blend))

    ; alpha-test support (experimental).
    (when at
      (sik:disable :alpha-test))

    ; z-buffer support (experimental).
    (when z
      (gl:disable :depth-test))

    (gl:disable :texture-2d)
    
    (gl:pop-matrix)))

;; Draw string with bitmap character.
(defun textb (str x y &key (r 1.0) (g 1.0) (b 1.0) (a nil) (font +bitmap-8-by-13+) (z nil))
  (render-2d 
    (set-draw-param nil r g b a nil)

    ; z-buffer support (experimental).
    (when z
      (gl:enable :depth-test))
    
    (gl:raster-pos x y (if z z 0.0))
    (loop for i from 0 below (length str) do
          (glut:bitmap-character font (char-code (aref str i))))

    ; z-buffer support (experimental).
    (when z
      (gl:disable :depth-test))
    
    (unset-draw-param nil r g b a nil)))

;; Draw string with stroke character.
(defun texts (str x y &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t) (s 1.0) (sx 1.0) (sy 1.0) (rt 0.0) (font +stroke-mono-roman+) (z nil))
  (render-2d
    (multiple-value-bind (x y fz) (glu:un-project x (- (glut:width *window*) y) 0.0)
      (gl:push-matrix)
      (set-draw-param w r g b a aa)

      ; z-buffer support (experimental).
      (when z
        (gl:enable :depth-test))
      
      (gl:translate x y (if z z 0.0))
      (gl:scale (* s 0.1) (* s -0.1) 0.0)
      (gl:scale sx sy 1.0)
      (gl:rotate rt 0.0 0.0 -1.0)
      (loop for i from 0 below (length str) do
            (glut:stroke-character font (char-code (aref str i))))

      ; z-buffer support (experimental).
      (when z
        (gl:disable :depth-test))
      
      (unset-draw-param w r g b a aa)
      (gl:pop-matrix))))

;; Draw point.
(defun point3 (x y z &key (s 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t))
  (set-draw-param nil r g b a aa)
  (gl:point-size s)
  (gl:begin :points)
  (gl:vertex x y z)
  (gl:end)
  (unset-draw-param nil r g b a aa))

;; Draw line.
(defun line3 (x y z x2 y2 z2 &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t))
  (set-draw-param w r g b a aa)
  (gl:begin :lines)
  (gl:vertex x y z)
  (gl:vertex x2 y2 z2)
  (gl:end)
  (unset-draw-param w r g b a aa))

(defun cube3 (size &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t) (f t))
  (render-3d
    (set-draw-param w r g b a aa)
    (if f
      (glut:solid-cube size)
      (glut:wire-cube size))
    (unset-draw-param w r g b a aa)))

(defun sphere3 (radius slices stacks &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t) (f t))
  (render-3d
    (set-draw-param w r g b a aa)
    (if f
      (glut:solid-sphere radius slices stacks)
      (glut:wire-sphere radius slices stacks))
    (unset-draw-param w r g b a aa)))

(defun torus3 (innerradius outerradius nsides rings &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t) (f t))
  (render-3d
    (set-draw-param w r g b a aa)
    (if f
      (glut:solid-torus innerradius outerradius nsides rings)
      (glut:wire-torus innerradius outerradius nsides rings))
    (unset-draw-param w r g b a aa)))

(defun cone3 (radius height slices stacks &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t) (f t))
  (render-3d
    (set-draw-param w r g b a aa)
    (if f
      (glut:solid-cone radius height slices stacks)
      (glut:wire-cone radius height slices stacks))
    (unset-draw-param w r g b a aa)))

(defun teapot3 (size &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t) (f t))
  (render-3d
    (set-draw-param w r g b a aa)
    (if f
      (glut:solid-teapot size)
      (glut:wire-teapot size))
    (unset-draw-param w r g b a aa)))

;; Draw polygon.
(defun poly3 (pnts &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t) (both nil))
  (render-3d
    (set-draw-param w r g b a aa)
    (when both
      (gl:enable :cull-face))
    (gl:begin :polygon)
    (let ((nx 0.0)
          (ny 0.0)
          (nz 0.0))
      (loop for i from 0 below (- (length pnts) 2) do
            (multiple-value-bind (x y z) (norm (nth i pnts) (nth (+ i 1) pnts) (nth (+ i 2) pnts))
              (incf nx x)
              (incf ny y)
              (incf nz z)))
      (gl:normal nx ny nz))
    (mapc (lambda (p) (gl:vertex (car p) (cadr p) (caddr p))) pnts)
    (gl:end)
    (when both
      (let ((rpnts (reverse pnts)))
        (gl:begin :polygon)
        (let ((nx 0.0)
              (ny 0.0)
              (nz 0.0))
          (loop for i from 0 below (- (length rpnts) 2) do
                (multiple-value-bind (x y z) (norm (nth i rpnts) (nth (+ i 1) rpnts) (nth (+ i 2) rpnts))
                  (incf nx x)
                  (incf ny y)
                  (incf nz z)))
          (gl:normal nx ny nz))
        (mapc (lambda (p) (gl:vertex (car p) (cadr p) (caddr p))) rpnts)
        (gl:end))
      (gl:disable :cull-face))
    (unset-draw-param w r g b a aa)))

;; Draw string with stroke character.
(defun texts3 (str &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t) (s 1.0) (font +stroke-mono-roman+))
  (render-3d
    (set-draw-param w r g b a aa)
    (gl:normal 0.0 0.0 1.0)
    (loop for i from 0 below (length str) do
          (glut:stroke-character font (char-code (aref str i))))
    (unset-draw-param w r g b a aa)))

;; Draw.
(defmethod glut:display ((this window))
  (set-mouse-push-state this)
  (set-key-push-state this)
  (gl:clear :depth-buffer)
  (fps-control this #'user-display)

  (if (double-buffer-enabled this) 
    (glut:swap-buffers)
    (gl:flush)))

(defclass dxfline () 
  ((code
     :accessor code)
   (typ
     :accessor typ)
   (data
     :accessor data)))

(defparameter *vertex* nil)
(defparameter *vread* nil)
(defparameter *drawmode* nil)

(defun dxf-clear ()
  (setf *vertex* (make-array '(4 4) :initial-element 0.0))
  (setf *vread* (make-array '(4 4) :initial-element nil))
  (setf *drawmode* :face))

(defun dxf-get-type (code)
  (cond ((equal code 999) :type-com)
        ((and (<= 0 code) (>= 9 code)) :type-str)
        ((and (<= 60 code) (>= 79 code)) :type-int)
        ((or (and (<= 10 code) (<= code 59)) (and (<= 210 code) (<= code 239))) :type-flt)
        (t :type-err)))

(defun trim (str)
  (string-trim '(#\Space #\Return #\Newline) str))

(defun dxf-read-line (dr fp)
  (let (str)
    ;; Read code.
    (unless (setf str (read-line fp nil)) (return-from dxf-read-line nil))
    (setf (code dr) (parse-integer str))
    
    ;; Read data.
    (setf (typ dr) (dxf-get-type (code dr)))
    (unless (setf str (read-line fp nil)) (return-from dxf-read-line nil))
    (cond
      ((equal :type-str (typ dr))
       (progn
         (setf (data dr) (trim str))
         (when (equal "EOF" (data dr)) (return-from dxf-read-line nil))))

      ((equal :type-com (typ dr))
       (progn))

      ((equal :type-int (typ dr))
       (setf (data dr) (parse-integer (trim str))))

      ((equal :type-flt (typ dr))
       (setf (data dr) (read-from-string (trim str))))
      
      (t 
        (error "dxf-read-line : unknown type")))
    (return-from dxf-read-line t)))

(defun dxf-read-table (fp)
  (let ((dr (make-instance 'dxfline))
        (iflayer 0))
    (loop while (dxf-read-line dr fp) do
          (progn
            (when (equal :type-str (typ dr))
              (if (equal "ENDTAB" (data dr)) (return-from dxf-read-table t))
              (if (equal "LAYER" (data dr))
                  (setf iflayer 1)
                  (if (and (equal 2 (code dr)) (equal iflayer 1))
                    (progn))))
            (setf iflayer 0)))))

(defun dxf-vertex (n v)
  (cond ((equal n 2) (gl:vertex (aref v 0) (aref v 1)))
        ((equal n 3) (gl:vertex (aref v 0) (aref v 1) (aref v 2)))
        ((equal n 4) (gl:vertex (aref v 0) (aref v 1) (aref v 2) (aref v 3)))))

;; WARN : Only triangular faces are supported.
(defun dxf-draw ()
  (if (equal :face *drawmode*)
    (gl:begin :polygon)
    (gl:begin :lines))
  
  ;; Set normal.
  (let (ls (list)) 
    (loop for i from 0 below 4 do
          (let ((codnum 0))
             (loop for j from 0 below 4 do
                   (if (aref *vread* i j) 
                       (incf codnum)))
             (if (equal codnum 3) 
               (setf ls (append ls (list (list (aref *vertex* i 0) 
                                               (aref *vertex* i 1) 
                                               (aref *vertex* i 2))))))))
    (if (>= (length ls) 3)
      ; (let ((nx 0.0)
      ;       (ny 0.0)
      ;       (nz 0.0))
      ;   (loop for i from 0 below (- (length ls) 2) do
      ;         (multiple-value-bind (x y z) (norm (nth i ls) (nth (+ i 1) ls) (nth (+ i 2) ls))
      ;           (incf nx x)
      ;           (incf ny y)
      ;           (incf nz z)))
      ;   (gl:normal nx ny nz))

      (multiple-value-bind (x y z) (norm (car ls) (cadr ls) (caddr ls))
        (let ((nrm (sqrt (+ (* x x) (* y y) (* z z)))))
          (gl:normal (/ x nrm) (/ y nrm) (/ z nrm))))))
  
  ;; Draw face.
  (loop for i from 0 below 4 do
        (let ((codnum 0))
             (loop for j from 0 below 4 do
                   (if (aref *vread* i j) 
                       (incf codnum)))
             (dxf-vertex codnum (vector (aref *vertex* i 0)
                                        (aref *vertex* i 1)
                                        (aref *vertex* i 2)
                                        (aref *vertex* i 3)))))
  (gl:end)

  ;; double face
  ; (if (equal :face *drawmode*)
  ;   (gl:begin :polygon)
  ;   (gl:begin :lines))
  ; (loop for i from 3 downto 0 do
  ;       (let ((codnum 0))
  ;            (loop for j from 0 below 4 do
  ;                  (if (aref *vread* i j) 
  ;                      (incf codnum)))
  ;            (dxf-vertex codnum (vector (aref *vertex* i 0)
  ;                                       (aref *vertex* i 1)
  ;                                       (aref *vertex* i 2)
  ;                                       (aref *vertex* i 3)))))
  ; (gl:end)

  (dxf-clear))

(defun dxf-read-entities (fp)
  (let ((dr (make-instance 'dxfline))
        v
        c)
    (loop while (dxf-read-line dr fp) do
          (if (equal :type-str (typ dr))
            (progn
              (dxf-draw)
              (cond ((equal 0 (code dr)) 
                     (cond ((equal "ENDSEC" (data dr)) (return-from dxf-read-entities t))
                           ((equal "3DFACE" (data dr)) (setf *drawmode* :face))
                           ((equal "3DLINE" (data dr)) (setf *drawmode* :line))))))
            (progn
              (setf c (- (floor (/ (code dr) 10)) 1))
              (setf v (mod (code dr) 10))
              (when (and (<= 0 v) (< v 4) (<= 0 c) (< c 3))
                (if (aref *vread* v c)
                  (dxf-draw))
                (setf (aref *vertex* v c) (data dr))
                (setf (aref *vread* v c) t)))))
    t))

(defun dxf-read-section (fp)
  (let ((dr (make-instance 'dxfline)))
    (loop while (dxf-read-line dr fp) do
          (if (and (equal :type-str (typ dr)) (equal "SECTION" (data dr)))
            (return)))
    (loop while (dxf-read-line dr fp) do
          (if (equal :type-str (typ dr))
            (cond ((equal "ENDSEC" (data dr)) 
                   (return-from dxf-read-section t))
                  ((equal "TABLE" (data dr)) 
                   (return-from dxf-read-section (dxf-read-table fp)))
                  ((equal "ENTITIES" (data dr)) 
                   (return-from dxf-read-section (dxf-read-entities fp))))))))

(defun dxf-read-file (fp)
  (let ((ls (gl:gen-lists 1))) 
    (dxf-clear)
    (gl:new-list ls :compile)
    (loop while (dxf-read-section fp))
    (gl:end-list)
    ls)) 

(defun load-dxf (path) 
  (with-open-file (fp path :direction :input) 
    (dxf-read-file fp)))

(defun model3 (ls &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t))
  (render-3d
    (set-draw-param w r g b a aa)
    (sik:enable :cull-face)
    (gl:call-list ls)
    (gl:disable :cull-face)
    (unset-draw-param w r g b a aa)))

;; Draw image.
(defun image3 (texture &key (a nil) (r 1.0) (g 1.0) (b 1.0) (at nil))
  (render-3d
    (sik:local
      (sik:disable :lighting)

      (when at
        (sik:enable :alpha-test)
        (gl:alpha-func :greater at))
      
      ; Alternative for alpha test.
      ; (gl:depth-mask nil)
      
      (gl:bind-texture :texture-2d (id texture))
      (gl:enable :texture-2d)
      (when a 
        (gl:enable :blend)
        (gl:blend-func :src-alpha :one-minus-src-alpha))
      (gl:color r g b (if a a 1.0))
      
      (let ((hw (/ (width texture) 2.0))
            (hh (/ (height texture) 2.0))) 
        (gl:with-primitive :quads
        (gl:tex-coord 0 0)
        (gl:vertex (- hw) (+ hh) 0)
        (gl:tex-coord 1 0)
        (gl:vertex (+ hw) (+ hh) 0)
        (gl:tex-coord 1 1)
        (gl:vertex (+ hw) (- hh) 0)
        (gl:tex-coord 0 1)
        (gl:vertex (- hw) (- hh) 0)))  
      
      (when a
        (gl:disable :blend))
      (gl:disable :texture-2d)

      (when at
        (sik:disable :alpha-test))

      ; Alternative for alpha test.
      ; (gl:depth-mask t)

      (sik:enable :lighting)
      )
    ))

(in-package :cl-user)


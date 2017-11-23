
;; Package sik.
(defpackage sik
  (:use :cl :cl-user)
  (:import-from gl 
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
  (:import-from glu
                look-at)
  (:import-from glut
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
    :to-rad
    :to-det
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
(defmethod initialize-instance :around ((this texture) &key path width height (intrpl :linear))
  (call-next-method)
  (gl:pixel-store :unpack-alignment 1)
  (let ((id (car (gl:gen-textures 1)))
        (raw-pnt (load-raw path width height)))
    (setf (id this) id)
    (gl:bind-texture :texture-2d id)
    (gl:tex-parameter :texture-2d :texture-min-filter intrpl)
    (gl:tex-parameter :texture-2d :texture-mag-filter intrpl)
    (gl:tex-image-2d :texture-2d 0 :rgba (float width) (float height) 0 :rgba :unsigned-byte raw-pnt)
    (setf (raw-pnt this) raw-pnt)
    (setf (width this) (float width))
    (setf (height this) (float height))
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
  (gl:material :front-and-back :ambient-and-diffuse (vector r g b (if a a 1.0)))
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
(defun point (x y &key (s 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t))
  (render-2d
    (set-draw-param nil r g b a aa)
    (gl:point-size s)
    (gl:begin :points)
    (gl:vertex x y 0.0)
    (gl:end)
    (unset-draw-param nil r g b a aa)))

;; Draw line.
(defun line (x y x2 y2 &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t))
  (render-2d
    (set-draw-param w r g b a aa)
    (gl:begin :lines)
    (gl:vertex x y 0.0)
    (gl:vertex x2 y2 0.0)
    (gl:end)
    (unset-draw-param w r g b a aa)))

;; Draw rect.
(defun rect (x y x2 y2 &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t))
  (render-2d
    (set-draw-param w r g b a aa)
    (gl:begin :polygon)
    (gl:vertex x y 0.0)
    (gl:vertex x2 y 0.0)
    (gl:vertex x2 y2 0.0)
    (gl:vertex x y2 0.0)
    (gl:end)
    (unset-draw-param w r g b a aa)))

;; Draw circle.
(defun circle (x y radius n-div &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t) (f nil))
  (render-2d
    (let* ((n n-div)
           (ptheta (/ (* 2.0 PI) n)))
       (if f
         (let ((pnts (list)))
           (loop for i from 0 below n do
                 (push (list (+ x (* radius (cos (* i ptheta))))
                             (+ y (* radius (sin (* i ptheta)))))
                       pnts))
           (sik:poly pnts :w w :r r :g g :b b :a a :aa aa))
         (loop for i from 0 below n do
             (sik:line (+ x (* radius (cos (* i ptheta))))
                       (+ y (* radius (sin (* i ptheta))))
                       (+ x (* radius (cos (* (+ i 1) ptheta))))
                       (+ y (* radius (sin (* (+ i 1) ptheta))))
                       :w w :aa aa :r r :g g :b b :a a))))))

;; Draw polygon.
(defun poly (pnts &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t))
  (render-2d 
    (set-draw-param w r g b a aa)
    (gl:begin :polygon)
    (mapc (lambda (p) (gl:vertex (car p) (cadr p) 0.0)) pnts)
    (gl:end)
    (unset-draw-param w r g b a aa)))

;; Draw image.
(defun image (texture x y &key (a nil) (sx 1.0) (sy 1.0) (rt 0.0) (r 1.0) (g 1.0) (b 1.0))
  (render-2d
    (gl:matrix-mode :modelview)
    (gl:push-matrix)
    (gl:load-identity)
    (gl:translate x y 0.0)
    (gl:rotate rt 0.0 0.0 1.0)
    (gl:scale sx sy 0.0)
    
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
      (gl:vertex (- hw) (- hh) 0)
      (gl:tex-coord 1 0)
      (gl:vertex (+ hw) (- hh) 0)
      (gl:tex-coord 1 1)
      (gl:vertex (+ hw) (+ hh) 0)
      (gl:tex-coord 0 1)
      (gl:vertex (- hw) (+ hh) 0)))  
    
    (when a
      (gl:disable :blend))
    (gl:disable :texture-2d)
    
    (gl:pop-matrix)))

;; Draw string with bitmap character.
(defun textb (str x y &key (r 1.0) (g 1.0) (b 1.0) (a nil) (font +bitmap-8-by-13+))
  (render-2d 
    (set-draw-param nil r g b a nil)
    (gl:raster-pos x y)
    (loop for i from 0 below (length str) do
          (glut:bitmap-character font (char-code (aref str i))))
    (unset-draw-param nil r g b a nil)))

;; Draw string with stroke character.
(defun texts (str x y &key (w 1.0) (r 1.0) (g 1.0) (b 1.0) (a nil) (aa t) (s 1.0) (sx 1.0) (sy 1.0) (rt 0.0) (font +stroke-mono-roman+))
  (render-2d
    (multiple-value-bind (x y z) (glu:un-project x (- (glut:width *window*) y) 0.0)
      (gl:push-matrix)
      (set-draw-param w r g b a aa)
      (gl:translate x y z)
      (gl:scale (* s 0.1) (* s -0.1) 0.0)
      (gl:scale sx sy 1.0)
      (gl:rotate rt 0.0 0.0 -1.0)
      (loop for i from 0 below (length str) do
            (glut:stroke-character font (char-code (aref str i))))
      (unset-draw-param w r g b a aa)
      (gl:pop-matrix))))

(defun basic-manip (x y z sx sy sz rx ry rz)
  (gl:translate x y z)
  (gl:rotate rx 1.0 0.0 0.0)
  (gl:rotate ry 0.0 1.0 0.0)
  (gl:rotate rz 0.0 0.0 1.0)
  (gl:scale sx sy sz))

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

(in-package :cl-user)


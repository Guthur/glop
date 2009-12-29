(in-package #:glop)

;; GL/OS interface
(defvar gl-get-proc-address nil)

;; Windows and GL context
(defstruct window
  width
  height
  title
  gl-context
  pushed-event)

(defgeneric create-gl-context (window &key make-current major minor)
  (:documentation "Creates a new OpenGL context of the specified version for the provided window
      and optionally make it current. If major and minor are NIL old style context creation is
      used.I"))

(defgeneric destroy-gl-context (ctx)
  (:documentation "Detach and release the provided OpenGL context."))

(defgeneric attach-gl-context (window ctx)
  (:documentation "Makes CTX the current OpenGL context and attach it to WINDOW."))

(defgeneric detach-gl-context (ctx)
  (:documentation "Make the provided OpenGL context no longer current."))

(defgeneric create-window (title width height &key major minor
                                                   double-buffer
                                                   stereo
                                                   red-size
                                                   green-size
                                                   blue-size
                                                   alpha-size
                                                   depth-size
                                                   accum-buffer
                                                   accum-red-size
                                                   accum-green-size
                                                   accum-blue-size
                                                   stencil-buffer
                                                   stencil-size)
  (:documentation "Creates a new GL window using the provided visual attributes.
                   Major and minor arguments specify the context version to use, when NIL
                   (default value) old style gl context is created, otherwise framebuffer config
                   based context creation is used."))

(defgeneric destroy-window (window)
  (:documentation "Destroy the provided GL window."))

(defgeneric show-window (window)
  (:documentation "Make WINDOW visible."))

(defgeneric hide-window (window)
  (:documentation "Make WINDOW not visible."))

(defgeneric set-window-title (window title)
  (:documentation "Set WINDOW title to TITLE."))

(defgeneric swap-buffers (window)
  (:documentation "Swaps GL buffers."))

;;; Events handling
(defstruct event
  type
  width height ;; for :configure :expose :show
  key          ;; for :key-press :key-release
  button       ;; for :button-press :button-release
  x y          ;; mouse position (all events)
  dx dy        ;; for :mouse-motion
)

(defun push-event (window evt)
  (setf (window-pushed-event window) evt))

(defun push-close-event (window)
  (push-event window (make-event :type :close)))

(defgeneric next-event (window &key blocking)
  (:documentation "Returns next available event for manual processing.
   If :blocking is true wait until an event occur."))

(defmethod next-event :around (window &key blocking)
  (declare (ignorable blocking))
  (let ((pushed-evt (window-pushed-event window)))
    (if pushed-evt
        (progn (setf (window-pushed-event window) nil)
               pushed-evt)
        (call-next-method))))

;; method based event handling
(defun dispatch-events (window &key blocking)
  "Process all pending system events and call corresponding methods.
   When :blocking is non-nil calls event handling func that will block
   until an event occur.
   Returns NIL on :CLOSE event, T otherwise."
  (loop for evt = (next-event window :blocking blocking)
    while evt
    do  (case (event-type evt)
          (:key-press (on-key window :press (event-key evt)))
          (:key-release (on-key window :release (event-key evt)))
          (:button-press (on-button window :press (event-button evt)))
          (:button-release (on-button window :release (event-button evt)))
          (:mouse-motion (on-mouse-motion window (event-x evt) (event-y evt)
                                          (event-dx evt) (event-dy evt)))
          (:configure (on-resize window (event-width evt) (event-height evt)))
          (:expose (on-draw window))
          (:close (on-close window)
                  (return-from dispatch-events nil))
          (t (format t "Unhandled event type: ~S~%" (event-type evt))))
    finally (return t)))


(defgeneric on-key (window state key))
(defgeneric on-button (window state button))
(defgeneric on-mouse-motion (window x y dx dy))
(defgeneric on-resize (window w h))
(defgeneric on-draw (window))
(defgeneric on-close (window))

;;; Some helper macros

;; main loop anyone?
(defmacro with-idle-forms (window &body idle-forms)
  (let ((blocking (unless idle-forms t))
        (res (gensym)))
    `(loop with ,res = (dispatch-events ,window :blocking ,blocking)
        while ,res
        do ,(if idle-forms
                  `(progn ,@idle-forms)
                  t))))

(defmacro with-window ((win-sym title width height &key major minor fullscreen) &body body)
  `(let ((,win-sym (create-window ,title ,width ,height :major ,major :minor ,minor :fullscreen ,fullscreen)))
     (when ,win-sym
       (unwind-protect (progn ,@body)
         (destroy-window ,win-sym)))))

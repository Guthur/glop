;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;; GLOP implementation
(in-package #:glop)

(defun gl-get-proc-address (proc-name)
  (declare (type string proc-name))
  (glop-glx:glx-get-proc-address proc-name))

(defstruct (x11-window (:include window))
  (display (cffi:null-pointer) :type cffi:foreign-pointer)         ;; X display ptr
  (screen 0 :type integer)                                                   ;; X screen number
  (id 0 :type integer)                                                           ;; X window ID
  (visual-infos (cffi:null-pointer) :type cffi:foreign-pointer)  ;; X visual format of the window
  (fb-config (cffi:null-pointer) :type cffi:foreign-pointer)      ;; X framebuffer config
  )

(defstruct glx-context
  (ctx (cffi:null-pointer) :type cffi:foreign-pointer)          ;; GL context ptr
  (display (cffi:null-pointer) :type cffi:foreign-pointer)    ;; X display ptr
  )

(defun create-gl-context (win &key (make-current t) major minor)
  (declare (type x11-window win) (type boolean make-current))
  (let ((ctx (make-glx-context :display (x11-window-display win))))
    (setf (glx-context-ctx ctx)
          (if (and major minor)
              (glop-glx:glx-create-specific-context (x11-window-display win)
                                                    (x11-window-fb-config win)
                                                    `(:major-version ,major :minor-version ,minor))
              (glop-glx:glx-create-context (x11-window-display win)
                                           (x11-window-visual-infos win))))
    (when make-current
      (attach-gl-context win ctx))
    (when (and major minor)
      (glop-glx:correct-context? major minor))
    ctx))

(defun destroy-gl-context (ctx)
  (detach-gl-context ctx)
  (glop-glx:glx-destroy-context (glx-context-display ctx)
                                 (glx-context-ctx ctx)))

(declaim (ftype (function (x11-window glx-context) null) attach-gl-context))
(defun attach-gl-context (win ctx)
  (setf (window-gl-context win) ctx)
  (glop-glx:glx-make-current (glx-context-display ctx)
                              (x11-window-id win)
                              (glx-context-ctx ctx))
  nil)

(defun detach-gl-context (ctx)
  (glop-glx:glx-release-context (glx-context-display ctx)))

(defun create-window (title width height &key major minor fullscreen
                                                  (double-buffer t)
                                                  stereo
                                                  (red-size 0)
                                                  (green-size 0)
                                                  (blue-size 0)
                                                  (alpha-size 0)
                                                  (depth-size 0)
                                                  accum-buffer
                                                  (accum-red-size 0)
                                                  (accum-green-size 0)
                                                  (accum-blue-size 0)
                                                  stencil-buffer (stencil-size 0))
  (without-fp-traps
    (let ((win (make-x11-window :display (glop-xlib::x-open-display "")
                                :screen 0)))
      ;;GLX attributes
      (with-accessors (
                       (display  x11-window-display)
                       (screen  x11-window-screen)
                       (id x11-window-id)
                       (visual-infos x11-window-visual-infos)
                       (fb-config x11-window-fb-config)
                       (win-width window-width)
                       (win-height window-height)
                       (win-title window-title)
                       (gl-ctx window-gl-context))
          win
        (let ((attribs (list :rgba t
                             :red-size red-size
                             :green-size green-size
                             :blue-size blue-size
                             :alpha-size alpha-size
                             :depth-size depth-size
                             :double-buffer double-buffer
                             :stereo stereo)))
          (when accum-buffer
            (push accum-red-size attribs)
            (push :accum-red-size attribs)
            (push accum-green-size attribs)
            (push :accum-green-size attribs)
            (push accum-blue-size attribs)
            (push :accum-blue-size attribs))
          (when stencil-buffer
            (push stencil-size attribs)
            (push :stencil-size attribs))
          ;; if major *and* minor are specified use fb config code path
          ;; otherwise just use old style visual selection and context creation
          (if (and major minor)
              ;;create fb-config and visual
              (setf fb-config (glop-glx:glx-choose-fb-config display screen attribs)
                    visual-infos (glop-glx:glx-get-visual-from-fb-config display fb-config))
              ;; create old style visual
              (setf visual-infos (glop-glx:glx-choose-visual display screen attribs))))
        ;; create window
        (setf id (glop-xlib:x-create-window display
                                            (glop-xlib:x-default-root-window display)
                                            width height visual-infos))
        (setf win-width width)
        (setf win-height height)
        ;; set title
        (glop-xlib:x-store-name display id title)
        (setf win-title title)
        ;; create a GL context and make it current same as for the visual regarding to major/minor
        ;; values
        (setf gl-ctx (create-gl-context win :major major :minor minor
                                        :make-current t))
        ;; show created window
        (show-window win)
        (glop-xlib:x-flush display)
        ;;make window fullscreen
        (when fullscreen
          (toggle-fullscreen win))
          ;; return created window
        win))))

(declaim (ftype (function (x11-window) null) toggle-fullscreen))
(defun toggle-fullscreen (win)
  (with-accessors (
                   (display  x11-window-display)
                   (screen  x11-window-screen)
                   (id x11-window-id)
                   (previous-video-mode window-previous-video-mode)
                   (win-width window-width)
                   (win-height window-height)
                   (fullscreen window-fullscreen))
      win
    (if fullscreen
        (progn
          (with-accessors (
                           (height video-mode-height)
                           (width video-mode-width))
              previous-video-mode
            (glop-xlib:set-fullscreen id display nil)
            (glop-xlib:set-video-mode display screen
                                      (glop-xlib:get-closest-video-mode display screen
                                                                        width height 0) 0))
          (setf fullscreen nil))
        (progn
          (setf previous-video-mode
                (multiple-value-bind (depth width height)
                    (glop-xlib:get-current-display-mode display screen)
                  (make-video-mode width height depth)))
          (glop-xlib:set-video-mode display screen
                                    (glop-xlib:get-closest-video-mode display screen
                                                                      win-width win-height 0) 0)
          (glop-xlib:set-fullscreen id display t)
          (setf fullscreen t))))
  nil)

(defun show-window (win)
  (glop-xlib:x-map-raised (x11-window-display win) (x11-window-id win)))

(defun hide-window (win)
  (glop-xlib:x-unmap-window (x11-window-display win) (x11-window-id win)))

(defun set-window-title (win title)
  (setf (slot-value win 'title) title)
  (glop-xlib:x-store-name (x11-window-display win) (x11-window-id win) title))

(defun destroy-window (win)
  (declare (type x11-window win))
  (with-accessors (
                   (display  x11-window-display)
                   (screen  x11-window-screen)
                   (id x11-window-id)
                   (previous-video-mode window-previous-video-mode)
                   (fullscreen window-fullscreen))
      win
    (when fullscreen
      (toggle-fullscreen win)
      (setf fullscreen nil))
    (glop-xlib:x-destroy-window display id)
    (glop-xlib:x-close-display display)))

(declaim (ftype (function (x11-window) null) swap-buffers))
(defun swap-buffers (win)
  (glop-glx:glx-wait-gl)
  (glop-glx:glx-swap-buffers (x11-window-display win) (x11-window-id win))
  nil)

(defun %next-event (win &key blocking)
  (declare (type x11-window win))
  (glop-xlib:x-next-event (x11-window-display win) blocking))


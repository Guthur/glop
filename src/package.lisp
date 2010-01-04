;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(defpackage :glop
  (:use #:cl)
  (:export
   ;; GL
   #:create-gl-context #:destroy-gl-context #:attach-gl-context 
   #:detach-gl-context
   #:gl-get-proc-address
   ;; window
   #:create-window #:destroy-window #:show-window #:hide-window 
   #:set-window-title
   #:swap-buffers
   #:window-width #:window-height #:window-gl-context #:toggle-fullscreen
   ;; events
   #:next-event #:push-event #:push-close-event
   #:event-type #:event-x #:event-y #:event-dx #:event-dy #:event-key 
   #:event-button #:event-width #:event-height
   ;; events methods
   #:dispatch-events
   #:on-key #:on-button #:on-mouse-motion #:on-resize #:on-draw #:on-close
   ;; helper macros
   #:with-window #:with-idle-forms
   ;; multiple windows
   #:set-gl-window))



;Ohne package definition
(setq interface

      (make-instance 'capi:interface

                     :visible-min-width 200

                     :title "My Interface"))

 
(capi:display interface)



;mit package definition
(defpackage "capi-demo"

	(:add-use-defaults t)

	(:use "CAPI")

)

(in-package "capi-demo")


(setq push-button

      (make-instance 'push-button

                     :data "Hello" 

                     :callback 

                     #'(lambda (&rest args) 

                         (display-message 

                          "Hello World"))))

(contain push-button)


(capi:define-interface updating-editor ()

  ()

  (:panes

   (numbers capi:list-panel

            :items '(1 2 3)

            :selection-callback 'update-editor

            :callback-type :interface

            :visible-min-height '(:character 3))

   (editor capi:editor-pane

           :text 

           "Select numbers in the list above."

           :visible-min-width

           (list :character 35)

           :buffer-name "temp")))

 

(defun update-editor (interface)

  (with-slots (numbers editor) interface

    (editor:process-character 

     (list #'(setf capi:editor-pane-text)

           (format nil "~R" 

                   (capi:choice-selected-item numbers))

           editor)

     (capi:editor-window editor))))

 

(capi:display (make-instance 'updating-editor))
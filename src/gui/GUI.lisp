;Relative Pfade zu den Icons und Dateien
  (setq clr_OpenImage (current-pathname "Images/open-file-icon.bmp"))
  (setq clr_SaveImage (current-pathname "Images/save.bmp"))
  (setq clr_NewImage (current-pathname "Images/NewPage.bmp"))
  (setq clr_EvalImage (current-pathname "Images/eval.bmp"))
  (setq clr_RefreshImage (current-pathname "Images/Refresh.bmp"))
  (setq Clr_help_path (current-pathname "Teamprojekt - Steven.pdf"))
  (setq clr_Pfad_Datei (current-pathname "Path.pa"))

;;Globale Parameter 
(defparameter clr_Pfad "")
(defparameter clr_Pfad_ausgewaehlt 0)
(defvar *eval-func*)

(defun set-eval-func (eval-func)
	(setq *eval-func* eval-func)
)

;;Main-Interface
(capi:define-interface clr_clr()
  ()
  (:panes
   (clr_open-file capi:push-button
              :image clr_OpenImage
              :callback-type :none
              :selection-callback (lambda () 
                                    (clr_einlesen)
                                    (setf (capi:editor-pane-text clr_inp) clr_test1) ;inp auf den "ersten" Wert setzen
                                    (setf (capi:editor-pane-text clr_Anfrage) clr_test2))) ;Anfrage auf den "zweiten" Wert setzen
   (clr_save-file capi:push-button
              :image clr_SaveImage
              :callback-type :none
              :selection-callback (lambda (&rest args) (clr_speichern clr_inp clr_Anfrage))) ;Beide Editoren an Speichern übergeben
   (clr_new-file capi:push-button
             :image clr_NewImage
             :callback-type :none
             :selection-callback (lambda() (setf (capi:editor-pane-text clr_inp) "Keine Eingabe vorhanden") (setf (capi:editor-pane-text clr_Anfrage) "Keine Anfrage vorhanden")(setf(capi:editor-pane-text clr_outp) "Keine Ausgabe vorhanden") (setq Pfad ""))) 
   (clr_evaluate-data capi:push-button
                  :image clr_EvalImage
                  :callback-type :none
                  :selection-callback (lambda () (mp:process-run-function "run-eval" () (lambda () (if (clr_speichern clr_inp clr_Anfrage) (setf (capi:editor-pane-text clr_outp) (write-to-string (evaluate)))))))) 
                 ;; :selection-callback (lambda () (setf (capi:editor-pane-text outp) (write-to-string (evaluate))))) ;zum testen ohne threading, wegen begrenzter lispworks version
   (clr_inp capi:editor-pane
           :title "Eingabe"
           :text "Noch keine Eingabe vorhanden"
           :visible-min-height '(:character 15)
           :visible-min-width '(:character 50))
   (clr_Anfrage capi:editor-pane
           :title "Anfrage"
           :text "Noch keine Anfrage vorhanden"
           :visible-min-height '(:character 3)
           :visible-max-height '(:character 5)
           :visible-min-width '(:character 50))
   (clr_outp capi:editor-pane
           :title "Ausgabe"
           :text "Noch keine Ausgabe vorhanden"
           :enabled :read-only ;keine Eingabe möglich
           :visible-min-height '(:character 10)
           :visible-max-height '(:character 10)
           :visible-min-width '(:character 50)
))
(:layouts
 (main-layout capi:column-layout '(row-of-buttons clr_inp clr_Anfrage clr_outp))
 (row-of-buttons capi:row-layout '(clr_new-file clr_open-file clr_save-file clr_evaluate-data)))
(:menus
 (clr_file-menu "File"
            (("New"
            :callback-type :none  
            :selection-callback (lambda()
			(and (setf (capi:editor-pane-text clr_inp) "Keine Eingabe vorhanden") 
			(setf (capi:editor-pane-text clr_Anfrage) "Keine Anfrage vorhanden")
			(setf(capi:editor-pane-text clr_outp) "Keine Ausgabe vorhanden") 
			(setq clr_Pfad ""))))
             ("Open..."
             :callback-type :none
             :selection-callback (lambda () 
                                    (and (clr_einlesen)
                                    (setf (capi:editor-pane-text clr_inp) clr_test1) ;Inp auf den "ersten" Wert setzen
                                    (setf (capi:editor-pane-text clr_Anfrage) clr_test2)))) ;Anfrage auf den "zweiten" Wert setzen
            ("Save"
             :callback-type :none
             :selection-callback (lambda (&rest args) (clr_speichern clr_inp clr_Anfrage))) ;Editoren an Speichern übergeben
            ("Save As..."
             :callback-type :none
             :selection-callback (lambda (&rest args) (and (setq clr_Pfad_ausgewaehlt 0)(speichern clr_inp clr_Anfrage)))); Pfad_ausgewählt auf 0 setzen, damit beim Speichern ein neuer Dateiname vergeben werden kann, danach werden die beiden Editoren an Speichern übergeben
            #|("Print..."
             :callback-type :none
             :selection-callback '(drucken inp Anfrage)) ;Muss noch gemacht werden!!!|#
            ("Close"
             :callback-type :none
             :selection-callback (lambda ()(capi:destroy clr_GUI))))) ;Schließen der Gui 
 (clr_predicate-menu "Predicates"
                 (("New Predicate"
                   :callback-type :none
                   :selection-callback '(clr_newPredicate-fenster))
                  ("Show All" 
                   :callback-type :none
                   :selection-callback '(clr_showPredicates-fenster))))
 (clr_help-menu "Help"
            (("?"
              :callback-type :none
              :selection-callback '(clr_help))
             ("About"
              :callback-type :none
              :selection-callback '(clr_About-fenster)))))

(:menu-bar clr_file-menu clr_predicate-menu clr_help-menu)
(:default-initargs :title "CL-Reason"
 :confirm-destroy-function (lambda (mgw)
                                       (declare (ignore mgw))
                                       (capi:confirm-yes-or-no "Möchten Sie CL-Reason wirklich beenden?")))) ;Meldung, ob wirklich verlassen werden soll

;;Erzeugung der Gui
(defun main ()
  (clr_SP_Pfad_einlesen)
  (setq clr_GUI(capi:display (make-instance 'clr_clr))))

;; Funktionen 
(defun evaluate () 
	;(print (list "in evaluate" Pfad))
	(funcall *eval-func* clr_Pfad)
)

(defun clr_einlesen () 
  (defparameter Test "")
  (defparameter test1 "")
  (defparameter test2 "")
  (setq clr_Pfad (capi:prompt-for-file "Datei auswählen" :pathname "C:/Users/Public/Documents/" :filter "*.clre" :filters (list "CL-Reason Datei" "*.clre"))) ;Pfad zum Einlesen auswählen 
  (if clr_Pfad (and ;Falls der Pfad gültig ist 
            (setq clr_Pfad_ausgewaehlt 1) ;Pfad ausgewählt auf 1 setzen, damit Speichern und nicht Speichern_als ausgewählt wird
            (do* ((in (open clr_Pfad :if-does-not-exist :create)) ;Einlesen des Inhaltes der angegebenen Datei
                  lines 
                  (line (read-line in nil 'eof)
                        (read-line in nil 'eof)))
                 ((equal line 'eof) (close in))
              (setq clr_test (setq lines (concatenate 'string lines line (make-string 1 :initial-element #\newline))))) ;nach jeder Zeile wird ein Zeilenumbruch erzwungen
            (defparameter clr_Pos (search "exists" clr_test)) ;Pos von "exists" in test ermitteln, damit an der Stelle die beiden Editoren getrennt werden können 
            (if (not clr_Pos) ;falls Pos nil...
                (and (setq clr_test1 clr_Test) (setq clr_test2 "Keine Anfrage vorhanden")) ;... dann wird der Text komplett in test1 gesetzt und test2 auf "Keine Anfrage vorhanden"
              ;sonst...
              (and (setq clr_test1 (subseq clr_Test 0 clr_Pos)) ;... wird der Text an Pos gespalten und der erste Teil in test1 gespeichert
                   (setq clr_test2 (subseq clr_Test clr_Pos))))))) ;währen der zweite Teil in test2 gespeichert wird

(defun clr_speichern (clr_inst clr_inst2) ;Beide Editoren (inp und Anfrage) werden übergeben
  (defparameter clr_Text "") 
  (setq clr_Text (capi:editor-pane-text clr_inst)) ;Text bekommt den Wert aus inp zugewiesen 
  (setq clr_Text (concatenate 'string clr_Text (make-string 1 :initial-element #\newline) (make-string 1 :initial-element #\newline) (capi:editor-pane-text clr_inst2))) ;anschließend werden Zeilenumbrüche und der Wert von Anfrage zugewiesen
  (when (= clr_Pfad_ausgewaehlt 0) ;Wernn Pfad_Ausgewählt = 0
    (and (setq clr_Pfad (capi:prompt-for-file "Datei auswählen" :pathname "C:/Users/Public/Documents/" :filter "*.clre" :filters (list "CL-Reason Datei" "*.clre") :operation :save)) (setq clr_Pfad_ausgewaehlt 1))) ;wird ein neuer Pfad gesetzt (Speichern als...) und Pfad_ausgewaehlt wird auf 1 gesetzt
  (when clr_pfad ;Falls Pfad nicht nil 
    (let((out(open clr_Pfad :direction :output :if-exists :new-version :if-does-not-exist :create))) ;Wird eine DDatei mit dem Pfad geöffnet. Wenn sie existiert, wird sie überschrieben, falls nicht, wird sie erstellt
      (write-string clr_Text out) ;Text wird in die Datei geschrieben
      (close out)) ;Stream wird geschlossen da es sonst zu Fehlern kommen kann
    ))

(defun clr_help ()
  (sys:call-system (concatenate 'string "\"" (namestring clr_help_path) "\"")) ;Hier wird die Hilfe "extern" aufgerufen
)

;About-fenster wird erzeugt 
(defun clr_About-fenster ()
  (setq clr_about-fenster2 (capi:display (make-instance 'clr_About))))

;newPredicate-fenster wird erzeugt
(defun clr_newPredicate-fenster ()
  (setq clr_newPredicate-fenster2 (capi:display (make-instance 'clr_newPredicate))))

;; About-Interface 
(capi:define-interface clr_About ()
  ()
  (:panes 
   (clr_edit capi:rich-text-pane 
         :text "


Version 0.1
GUI: Kevin Tittebrandt - tittk@hochschule-trier.de
Einbindung Prädikate: Tobias Arens
Resolution: Steven Kutsch "
         :enabled :read-only
         :paragraph-format '(:alignment :center)
         :visible-min-width '(:character 50)
         :visible-max-width '(:character 50)
         :visible-min-height '(:character 10)
         :visible-max-height '(:Character 10)))
  (:layouts
   (main-layout capi:column-layout '(clr_edit)))
  (:default-initargs :title "About"))

;;Neues Prädikat Interface
(capi:define-interface clr_newPredicate ()
  ()
  (:panes
   (clr_PEdit capi:editor-pane
         :text "Hier ein neues Prädikat anlegen"
         :visible-min-width '(:character 50)
         :visible-max-width '(:character 50)
         :visible-min-height '(:character 10)
         :visible-max-height '(:Character 10))
   (clr_PSpeichern capi:push-button
              :Image clr_SaveImage
              :callback-type :none
              :selection-callback (lambda () (clr_newPredicate_save clr_PEdit)))
   (clr_PNeu capi:push-button 
        :image clr_NewImage
        :callback-type :none
        :selection-callback (lambda () (setf (capi:editor-pane-text clr_PEdit) "Hier ein neues Prädikat anlegen" ))))
  (:layouts
   (main-layout capi:column-layout '(row-of-buttons clr_PEdit))
   (row-of-buttons capi:row-layout '(clr_PNeu clr_PSpeichern)))
  (:default-initargs :title "Neues Prädikat anlegen"))

(defparameter clr_newPredicatePath "")
(defun clr_newPredicate_save (clr_ins) ;Editor wird übergeben
  (setq clr_newPredicate_text (capi:editor-pane-text clr_ins)) ;Text aus dem Editor auslesen
  (setq clr_newPredicatePath (capi:prompt-for-file "Bitte wählen Sie eine Datei aus!" :pathname "C:/Users/Public/Documents/" :filter "*.pred" :filters (list "CL-Reason Datei" "*.pred") :operation :save)) ;Speicherpfad auswählen, dabei wird eine Speicherung als ".pred" Datei erzwungen 
  (when clr_newPredicatePath
    (setq out (open clr_newPredicatePath :direction :output :if-exists :new-version :if-does-not-exist :create)) ;Falls die angegebene Datei nicht vorhanden ist, wird sie neu erstellt, ist sie hingegen vorhanden, wird die alte Datei mit dem neuen Inhalt überschrieben
    (write-string clr_newpredicate_text out ) ;Das Prädikat wird in die Datei geschrieben
    (close out)) ;Stream wird geschlossen
)

;;Prädikate anzeigen
(defparameter CLR_SP_Pfad "")
(defparameter clr_select_Data "")

(defun clr_SP_Pfad_einlesen ()
  (setq in (open clr_pfad_datei :if-does-not-exist :create)) ;Pfaddatei öffnen (falls nicht vorhanden, erstellen
  (setq CLR_SP_Pfad (read-line in nil)) ;Eine Zeile einlesen
  (close in)) ;Pfaddatei schließen
  ;(print CLR_SP_Pfad))  ;Anzeige, ob Variable gesetzt wurde

;(SP_Pfad_einlesen) ;um den CLR_SP_Pfad zu setzen

(defun clr_SP_Pfad_aendern (clr_int clr_ins)
  (setq CLR_SP_Pfad (capi:prompt-for-directory "Bitte wählen Sie einen Pfad aus" :pathname clr_pfad_datei)) ;Neuen Pfad auswählen
  (when CLR_SP_Pfad
    (let((out(open clr_pfad_datei :direction :output :if-exists :new-version :if-does-not-exist :create))) ;Pfaddatei öffnen
      (write-string (namestring CLR_SP_Pfad) out) ;Neuen Pfad in Pfaddatei eintragen (überschreiben)
      (close out));Pfaddatei schließen
    (when (= clr_int 1) ;Wenn int = 1...
      (aenderungen_anzeigen clr_ins)))) ; ... wird automatisch ein neues Fenester mit dem neuen Pfad geöffnet

(defun Aenderungen_anzeigen (clr_ins)
  (setf (capi:collection-items clr_ins) (clr_alles clr_ins)))

(defun clr_alles (clr_ins)
  (when (not CLR_SP_Pfad) ;Falls noch kein Pfad in der Pfaddatei existiert...
      (clr_SP_Pfad_aendern 0 clr_ins)) ;... wird hier einer gesetzt ;; durch die 0 wird die Erzeugung eines zweiten Fensters verhindert
  (when clr_sp_pfad
    (reverse(clr_toString (directory (concatenate 'string (namestring CLR_SP_Pfad) "*.pred")))))) 
	
(defun clr_toString (clr_Liste) ;Rekursiv gelöst
  (Cond((not clr_Liste) ()) ;Falls Liste leer, eine leere Liste zurückgeben 
       (T (append (list (subseq (namestring (car clr_Liste)) (length (namestring CLR_SP_Pfad)))) (clr_toString(cdr clr_Liste)))))) ;An eine Liste wird der Dateiname angehangen (dieser beginnt am der Länge des Pfades des Ordners), anschließend rekursiver Aufruf mit dem Rest der Liste 
   
;Definiton des showPredicates-Interface
(capi:define-interface clr_showPredicates ()
  ()
  (:panes 
   (clr_Liste capi:list-panel
          :name "Prädikate"
          :visible-min-width '(:character 75)
          :visible-min-height '(:character 10)
          :items (clr_alles 'clr_Liste)
          :action-callback (lambda (clr_data clr_interface) (clr_edit_Predicate clr_data clr_interface)))
   (clr_Anzeige_aendern capi:push-button 
                    :image clr_RefreshImage
                    :callback-type :none
                    :selection-callback (lambda () (clr_Aenderungen_anzeigen clr_Liste)))) ;Manuelles aktualisieren der Datein im angegebenen Pfad 
  (:menus 
   (clr_File2-menu "File"
               (("Change Path"
                 :callback-type :none
                 :selection-callback (lambda () (clr_SP_Pfad_aendern 1 clr_Liste) ))))) ;die 1 erlaubt eine automatische Aktualisierung des List-Panels

  (:layouts
   (main-layout capi:column-layout '(clr_Liste clr_Anzeige_aendern)))
  (:menu-bar clr_file2-menu)
  (:default-initargs :title "Alle Pädikate anzeigen"))

(defun clr_edit_Predicate (clr_data clr_interface) 
  (setq clr_select_Data clr_data) ; Date (Dateiname) global setzen 
  (capi:display (make-instance 'clr_editPredicate))) ;Gui zum anlegen der Prädikate erzeugen

(defun clr_nP_loadText () ;Text des ausgewählten Prädikates wird geladen und in dem neuen Fenster angezeigt
  (setq clr_Pathname (concatenate 'string (namestring CLR_SP_Pfad) clr_select_Data)) ;Zusammenbauen des Pfades zur Datei
  (defparameter clr_Lines "")
  (do* ((in (open clr_Pathname :if-does-not-exist :create))
        (line (read-line in nil 'eof)
              (read-line in nil 'eof)))
       ((equal line 'eof) (close in))
    (setq clr_Lines(concatenate 'string clr_lines line (make-string 1 :initial-element #\newline))))
  (if (string-equal clr_lines "") ;Falls Lines leer ist,...
      (setq clr_predicate_text "Leeres Prädikat") ; ...wird der Text auf "Leeres Prädikat" gesetzt
    (setq clr_Predicate_text clr_Lines))) ; und sonst auf den Wert von Lines

(defun clr_Predicate_save (clr_ins) 
  (setq clr_SP_out (open (clr_pathname (concatenate 'string (namestring clr_sp_pfad) clr_select_data)) :direction :output :if-exists :new-version)) ;
  (write-string (capi:editor-pane-text clr_ins) clr_SP_out)
  (close clr_SP_out))

(defun clr_showPredicates-fenster ()
  (setq instanz (capi:display (make-instance 'clr_showPredicates))))

(capi:define-interface clr_editPredicate ()
  ()
  (:panes
   (clr_PreEdit capi:editor-pane
            :text (clr_nP_loadText) ;Text aus der Datei laden und anzeigen
            :visible-min-width '(:character 50)
            :visible-max-width '(:character 50)
            :visible-min-height '(:character 10)
            :visible-max-height '(:Character 10))
   (clr_PreSpeichern capi:push-button
               :Image clr_SaveImage
               :callback-type :none
               :selection-callback (lambda () (clr_Predicate_save clr_PreEdit)))
   (clr_PreNeu capi:push-button  
         :image clr_NewImage
         :callback-type :none
         :selection-callback (lambda () (setf (capi:editor-pane-text clr_PreEdit) "Hier ein neues Prädikat anlegen" ))))
  (:layouts
   (main-layout capi:column-layout '(row-of-buttons clr_PreEdit))
   (row-of-buttons capi:row-layout '(clr_PreSpeichern)))
  (:default-initargs :title "Prädikat bearbeiten"))

;;Prädikat suchen
;(capi:define-interface searchPredicate ()
 ; ())
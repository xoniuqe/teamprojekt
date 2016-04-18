;Relative Pfade zu den Icons und Dateien
;(defun Pfadsetzen () 
  (setq OpenImage (current-pathname "Images/open-file-icon.bmp"))
  (setq SaveImage (current-pathname "Images/save.bmp"))
  (setq NewImage (current-pathname "Images/NewPage.bmp"))
  (setq EvalImage (current-pathname "Images/eval.bmp"))
  (setq RefreshImage (current-pathname "Images/Refresh.bmp"))
  (setq Clr_help_path (current-pathname "Teamprojekt - Steven.pdf"))
  (setq Pfad_Datei (current-pathname "Path.pa"))

;;;)
;;Globale Parameter 
(defparameter Pfad "")
(defparameter Pfad_ausgewaehlt 0)
(defvar *eval-func*)

(defun set-eval-func (eval-func)
	(setq *eval-func* eval-func)
)

;;Main-Interface
(capi:define-interface TP()
  ()
  (:panes
   (open-file capi:push-button
              :image OpenImage
              :callback-type :none
              :selection-callback (lambda () 
                                    (einlesen)
                                    (setf (capi:editor-pane-text inp) test1) ;inp auf den "ersten" Wert setzen
                                    (setf (capi:editor-pane-text Anfrage) test2))) ;Anfrage auf den "zweiten" Wert setzen
   (save-file capi:push-button
              :image SaveImage
              :callback-type :none
              :selection-callback (lambda (&rest args) (speichern inp Anfrage))) ;Beide Editoren an Speichern übergeben
   (new-file capi:push-button
             :image NewImage
             :callback-type :none
             :selection-callback (lambda() (setf (capi:editor-pane-text inp) "Keine Eingabe vorhanden") (setf (capi:editor-pane-text Anfrage) "Keine Anfrage vorhanden")(setf(capi:editor-pane-text outp) "Keine Ausgabe vorhanden"))) ;Text der Editoren überschreiben
   (evaluate-data capi:push-button
                  :image EvalImage
                  :callback-type :none
                  :selection-callback (lambda () (mp:process-run-function "run-eval" () (lambda () (setf (capi:editor-pane-text outp) (write-to-string (evaluate))))))) ;Funktion muss noch angelegt werden!!!
   (inp capi:editor-pane
           :title "Eingabe"
           :text "Noch keine Eingabe vorhanden"
           :visible-min-height '(:character 15)
           :visible-min-width '(:character 50))
   (Anfrage capi:editor-pane
           :title "Anfrage"
           :text "Noch keine Anfrage vorhanden"
           :visible-min-height '(:character 3)
           :visible-max-height '(:character 5)
           :visible-min-width '(:character 50))
   (outp capi:editor-pane
           :title "Ausgabe"
           :text "Noch keine Ausgabe vorhanden"
           :enabled :read-only ;keine Eingabe möglich
           :visible-min-height '(:character 10)
           :visible-max-height '(:character 10)
           :visible-min-width '(:character 50)
))
(:layouts
 (main-layout capi:column-layout '(row-of-buttons inp Anfrage outp))
 (row-of-buttons capi:row-layout '(new-file open-file save-file evaluate-data)))
(:menus
 (file-menu "File"
            (("New"
            :callback-type :none  
            :selection-callback (lambda() (and (setf (capi:editor-pane-text inp) "Keine Eingabe vorhanden") (setf (capi:editor-pane-text Anfrage) "Keine Anfrage vorhanden")(setf(capi:editor-pane-text outp) "Keine Ausgabe"))))
             ("Open..."
             :callback-type :none
             :selection-callback (lambda () 
                                    (and (einlesen)
                                    (setf (capi:editor-pane-text inp) test1) ;Inp auf den "ersten" Wert setzen
                                    (setf (capi:editor-pane-text Anfrage) test2)))) ;Anfrage auf den "zweiten" Wert setzen
            ("Save"
             :callback-type :none
             :selection-callback (lambda (&rest args) (speichern inp Anfrage))) ;Editoren an Speichern übergeben
            ("Save As..."
             :callback-type :none
             :selection-callback (lambda (&rest args) (and (setq Pfad_ausgewaehlt 0)(speichern inp Anfrage)))); Pfad_ausgewählt auf 0 setzen, damit beim Speichern ein neuer Dateiname vergeben werden kann, danach werden die beiden Editoren an Speichern übergeben
            #|("Print..."
             :callback-type :none
             :selection-callback '(drucken inp Anfrage)) ;Muss noch gemacht werden!!!|#
            ("Close"
             :callback-type :none
             :selection-callback (lambda ()(capi:destroy myTest))))) ;Schließen der Gui 
 (edit-menu "Edit" ;Wird wahrscheinlich gelöscht
            (("Cut"
              :callback-type :none
              :selection-callback '(cut)) 
             ("Copy")
             ("Paste"
              :callback-type :none
              :selection-callback '(paste))
             ("Select All" 
              :callback-type :none
              :selection-callback '(select-All))))
 (predicate-menu "Predicates"
                 (("New Predicate"
                   :callback-type :none
                   :selection-callback '(newPredicate-fenster))
                  ;("Search Predicate"
                   ;:callback-type :none
                   ;:selection-callback '(search-predicate))
                  ("Show All" 
                   :callback-type :none
                   :selection-callback '(showPredicates-fenster))))
 (help-menu "Help"
            (("?"
              :callback-type :none
              :selection-callback '(clr_help))
             ("About"
              :callback-type :none
              :selection-callback '(About-fenster)))))

(:menu-bar file-menu predicate-menu help-menu)
(:default-initargs :title "CL-Reason"
 :confirm-destroy-function (lambda (mgw)
                                       (declare (ignore mgw))
                                       (capi:confirm-yes-or-no "Would you like to quit Enviroment?")))) ;Meldung, ob wirklich verlassen werden soll

;;Erzeugung der Gui
(defun main ()
  ;;; (Pfadsetzen)
  (SP_Pfad_einlesen)
  (setq myTest(capi:display (make-instance 'TP))))

;; Funktionen 
(defun evaluate () 
	;(print (list "in evaluate" Pfad))
	(funcall *eval-func* Pfad)
)

(defun einlesen () 
  (defparameter Test "")
  (defparameter test1 "")
  (defparameter test2 "")
  (setq Pfad (capi:prompt-for-file "Datei auswählen" :pathname "C:/Users/Public/Documents/" :filter "*.clre" :filters (list "CL-Reason Datei" "*.clre"))) ;Pfad zum Einlesen auswählen 
  (if Pfad (and ;Falls der Pfad gültig ist 
            (setq Pfad_ausgewaehlt 1) ;Pfad ausgewählt auf 1 setzen, damit Speichern und nicht Speichern_als ausgewählt wird
            (do* ((in (open Pfad :if-does-not-exist :create)) ;Einlesen des Inhaltes der angegebenen Datei
                  lines 
                  (line (read-line in nil 'eof)
                        (read-line in nil 'eof)))
                 ((equal line 'eof) (close in))
              (setq test (setq lines (concatenate 'string lines line (make-string 1 :initial-element #\newline))))) ;nach jeder Zeile wird ein Zeilenumbruch erzwungen
            (defparameter Pos (search "exists" test)) ;Pos von "---" in test ermitteln, damit an der Stelle die beiden Editoren getrennt werden können 
            (if (not Pos) ;falls Pos nil...
                (and (setq test1 Test) (setq test2 "Keine Anfrage vorhanden")) ;... dann wird der Text komplett in test1 gesetzt und test2 auf "Keine Anfrage vorhanden"
              ;sonst...
              (and (setq test1 (subseq Test 0 Pos)) ;... wird der Text an Pos gespalten und der erste Teil in test1 gespeichert
                   (setq test2 (subseq Test Pos))))))) ;währen der zweite Teil in test2 gespeichert wird

(defun speichern (inst inst2) ;Beide Editoren (inp und Anfrage) werden übergeben
  (defparameter Text "") 
  (setq Text (capi:editor-pane-text inst)) ;Text bekommt den Wert aus inp zugewiesen 
  (setq Text (concatenate 'string Text (make-string 1 :initial-element #\newline) #|"---"|# (make-string 1 :initial-element #\newline) (capi:editor-pane-text inst2))) ;anschließend wird "---" + Zeilenumbrüche und der Wert von Anfrage zugewiesen
  (when (= Pfad_ausgewaehlt 0) ;Wernn Pfad_Ausgewählt = 0
    (and (setq Pfad (capi:prompt-for-file "Datei auswählen" :pathname "C:/Users/Public/Documents/" :filter "*.clre" :filters (list "CL-Reason Datei" "*.clre") :operation :save)) (setq Pfad_ausgewaehlt 1))) ;wird ein neuer Pfad gesetzt (Speichern als...) und Pfad_ausgewaehlt wird auf 1 gesetzt
  (when pfad ;Falls Pfad nicht nil 
    (let((out(open Pfad :direction :output :if-exists :new-version :if-does-not-exist :create))) ;Wird eine DDatei mit dem Pfad geöffnet. Wenn sie existiert, wird sie überschrieben, falls nicht, wird sie erstellt
      (write-string Text out) ;Text wird in die Datei geschrieben
      (close out)) ;Stream wird geschlossen da es sonst zu Fehlern kommen kann
    ))

(defun clr_help ()
  (sys:call-system (concatenate 'string "\"" (namestring clr_help_path) "\"")) ;Hier wird die Hilfe "extern" aufgerufen
)
(defun drucken (ins1 ins2)
  )

;About-fenster wird erzeugt 
(defun About-fenster ()
  (setq about-fenster2 (capi:display (make-instance 'About))))

;newPredicate-fenster wird erzeugt
(defun newPredicate-fenster ()
  (setq newPredicate-fenster2 (capi:display (make-instance 'newPredicate))))

;; About-Interface 
(capi:define-interface About ()
  ()
  (:panes 
   (edit capi:rich-text-pane 
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
   (main-layout capi:column-layout '(edit)))
  (:default-initargs :title "About"))

;;Neues Prädikat Interface
(capi:define-interface newPredicate ()
  ()
  (:panes
   (PEdit capi:editor-pane
         :text "Hier ein neues Prädikat anlegen"
         :visible-min-width '(:character 50)
         :visible-max-width '(:character 50)
         :visible-min-height '(:character 10)
         :visible-max-height '(:Character 10))
   (PSpeichern capi:push-button
              :Image SaveImage
              :callback-type :none
              :selection-callback (lambda () (newPredicate_save PEdit)))
   (PNeu capi:push-button 
        :image NewImage
        :callback-type :none
        :selection-callback (lambda () (setf (capi:editor-pane-text PEdit) "Hier ein neues Prädikat anlegen" ))))
  (:layouts
   (main-layout capi:column-layout '(row-of-buttons PEdit))
   (row-of-buttons capi:row-layout '(PNeu PSpeichern)))
  (:default-initargs :title "Neues Prädikat anlegen"))

(defparameter newPredicatePath "")
(defun newPredicate_save (ins) ;Editor wird übergeben
  (setq newPredicate_text (capi:editor-pane-text ins)) ;Text aus dem Editor auslesen
  (setq newPredicatePath (capi:prompt-for-file "Bitte wählen Sie eine Datei aus!" :pathname "C:/Users/Public/Documents/" :filter "*.pred" :filters (list "CL-Reason Datei" "*.pred") :operation :save)) ;Speicherpfad auswählen, dabei wird eine Speicherung als ".pred" Datei erzwungen 
  (when newPredicatePath
    (setq out (open newPredicatePath :direction :output :if-exists :new-version :if-does-not-exist :create)) ;Falls die angegebene Datei nicht vorhanden ist, wird sie neu erstellt, ist sie hingegen vorhanden, wird die alte Datei mit dem neuen Inhalt überschrieben
    (write-string newpredicate_text out ) ;Das Prädikat wird in die Datei geschrieben
    (close out)) ;Stream wird geschlossen
)

;;Prädikate anzeigen
(defparameter CLR_SP_Pfad "")
(defparameter select_Data "")

(defun SP_Pfad_einlesen ()
  (setq in (open pfad_datei :if-does-not-exist :create)) ;Pfaddatei öffnen (falls nicht vorhanden, erstellen
  (setq CLR_SP_Pfad (read-line in nil)) ;Eine Zeile einlesen
  (close in)) ;Pfaddatei schließen
  ;(print CLR_SP_Pfad))  ;Anzeige, ob Variable gesetzt wurde

;(SP_Pfad_einlesen) ;um den CLR_SP_Pfad zu setzen

(defun SP_Pfad_aendern (int ins)
  (setq CLR_SP_Pfad (capi:prompt-for-directory "Bitte wählen Sie einen Pfad aus" :pathname pfad_datei)) ;Neuen Pfad auswählen
  (when CLR_SP_Pfad
    (let((out(open pfad_datei :direction :output :if-exists :new-version :if-does-not-exist :create))) ;Pfaddatei öffnen
      (write-string (namestring CLR_SP_Pfad) out) ;Neuen Pfad in Pfaddatei eintragen (überschreiben)
      (close out));Pfaddatei schließen
    (when (= int 1) ;Wenn int = 1...
      (aenderungen_anzeigen ins)))) ; ... wird automatisch ein neues Fenester mit dem neuen Pfad geöffnet

(defun Aenderungen_anzeigen (ins)
  (setf (capi:collection-items ins) (alles)))

(defun alles ()
  (when (not CLR_SP_Pfad) ;Falls noch kein Pfad in der Pfaddatei existiert...
      (SP_Pfad_aendern 0)) ;... wird hier einer gesetzt ;; durch die 0 wird die Erzeugung eines zweiten Fensters verhindert
  (when clr_sp_pfad
    (toString (directory (concatenate 'string (namestring CLR_SP_Pfad) "*.pred"))))) ;Eine Liste mit allen Pfaden an "toString" übergeben

(defun toString (Liste) ;Rekursiv gelöst
  (Cond((not Liste) ()) ;Falls Liste leer, eine leere Liste zurückgeben 
       (T (append (list (subseq (namestring (car Liste)) (length (namestring CLR_SP_Pfad)))) (toString(cdr Liste)))))) ;An eine Liste wird der Dateiname angehangen (dieser beginnt am der Länge des Pfades des Ordners), anschließend rekursiver Aufruf mit dem Rest der Liste 
   
;Definiton des showPredicates-Interface
(capi:define-interface showPredicates ()
  ()
  (:panes 
   (Liste capi:list-panel
          :name "Prädikate"
          :visible-min-width '(:character 75)
          :visible-min-height '(:character 10)
          :items (alles)
          :action-callback (lambda (data interface) (edit_Predicate data interface)))
   (Anzeige_aendern capi:push-button 
                    :image RefreshImage
                    :callback-type :none
                    :selection-callback (lambda () (Aenderungen_anzeigen Liste)))) ;Manuelles aktualisieren der Datein im angegebenen Pfad 
  (:menus 
   (File2-menu "File"
               (("Change Path"
                 :callback-type :none
                 :selection-callback (lambda () (SP_Pfad_aendern 1 Liste) ))))) ;die 1 erlaubt eine automatische Aktualisierung des List-Panels

  (:layouts
   (main-layout capi:column-layout '(Liste Anzeige_aendern)))
  (:menu-bar file2-menu)
  (:default-initargs :title "Alle Pädikate anzeigen"))

(defun edit_Predicate (data interface) 
  (setq select_Data data) ; Date (Dateiname) global setzen 
  (capi:display (make-instance 'editPredicate))) ;Gui zum anlegen der Prädikate erzeugen

(defun nP_loadText () ;Text des ausgewählten Prädikates wird geladen und in dem neuen Fenster angezeigt
  (setq Pathname (concatenate 'string (namestring CLR_SP_Pfad) select_Data)) ;Zusammenbauen des Pfades zur Datei
  (defparameter Lines "")
  (do* ((in (open Pathname :if-does-not-exist :create))
        (line (read-line in nil 'eof)
              (read-line in nil 'eof)))
       ((equal line 'eof) (close in))
    (setq Lines(concatenate 'string lines line (make-string 1 :initial-element #\newline))))
  (if (string-equal lines "") ;Falls Lines leer ist,...
      (setq predicate_text "Leeres Prädikat") ; ...wird der Text auf "Leeres Prädikat" gesetzt
    (setq Predicate_text Lines))) ; und sonst auf den Wert von Lines

(defun Predicate_save (ins) 
  (setq SP_out (open (pathname (concatenate 'string (namestring clr_sp_pfad) select_data)) :direction :output :if-exists :new-version)) ;
  (write-string (capi:editor-pane-text ins) SP_out)
  (close SP_out))

(defun showPredicates-fenster ()
  (setq instanz (capi:display (make-instance 'showPredicates))))

(capi:define-interface editPredicate ()
  ()
  (:panes
   (PreEdit capi:editor-pane
            :text (nP_loadText) ;Text aus der Datei laden und anzeigen
            :visible-min-width '(:character 50)
            :visible-max-width '(:character 50)
            :visible-min-height '(:character 10)
            :visible-max-height '(:Character 10))
   (PreSpeichern capi:push-button
               :Image SaveImage
               :callback-type :none
               :selection-callback (lambda () (Predicate_save PreEdit)))
   (PreNeu capi:push-button  
         :image NewImage
         :callback-type :none
         :selection-callback (lambda () (setf (capi:editor-pane-text PreEdit) "Hier ein neues Prädikat anlegen" ))))
  (:layouts
   (main-layout capi:column-layout '(row-of-buttons PreEdit))
   (row-of-buttons capi:row-layout '(PreSpeichern)))
  (:default-initargs :title "Prädikat bearbeiten"))

;;Prädikat suchen
;(capi:define-interface searchPredicate ()
 ; ())
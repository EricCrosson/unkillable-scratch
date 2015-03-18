(require 'unkillable-scratch)

(unkillable-scratch 1)

(ert-deftest test-unkillable-scratch ()
  ;; test the default '*scratch* only' functionality
  (let ((scratch "*scratch*"))
    (kill-buffer scratch)
    (should (buffer-live-p (get-buffer scratch)))))

(ert-deftest test-unkillable-buffers ()
  ;; test the general functionality
  (let ((general "red-shirt"))
    (get-buffer-create general)
    (should (buffer-live-p (get-buffer general)))
    (kill-buffer general)
    (should (eq nil
		(let ((retval t))
		  (catch 'found
		    (mapc (lambda (buf)
			    (when (string-match general (buffer-name buf)) (throw 'found nil)))
			  (buffer-list))
		    (setq retval nil))
		  retval)))
    ;; now add it to the list and test for indestructability
    (get-buffer-create general)
    (add-to-list 'unkillable-buffers general)
    (kill-buffer general)
    (should (buffer-live-p (get-buffer general)))))

(provide 'unkillable-scratch-test)
;;; unkillable-scratch-test.el ends here

(require 'unkillable-scratch)

(unkillable-scratch 1)

(ert-deftest test-unkillable-scratch ()
  (let ((scratch "*scratch*"))
    (kill-buffer scratch)
    (should (buffer-live-p (get-buffer scratch)))))

(provide 'unkillable-scratch-test)
;;; unkillable-scratch-test.el ends here

#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim)
             (ice-9 regex)
             (spells network))

(define line-end "\r\n")
(define nick "aidalbot")
(define user "aidalgol")
(define name "Aidan Gauland")
(define server "irc.example.com")
(define port 6667)

(define (process-line line)
  "Process a line from the IRC server."
  (cond ((string-match "^PING" line)
         (pong line out))
        ((string-match "^:.*!.*@.* PRIVMSG" line)
         (handle-privmsg line out))))

(define ponged #f) ;; Whether we've responded to a PING.
(define (pong line out)
  "Reply, on OUT, to a ping (which is LINE)."
  (display (string-append "PONG" (substring line 4)) out)
  (set! ponged #t))

(define (handle-privmsg line out)
  (let ((msg-fields (parse-privmsg line)))
    (if (string= (assoc-ref msg-fields 'message) (string-append nick ": quit"))
          (display (string-append "QUIT" line-end) out))
    (display (string-append
              "Message received from "
              (assoc-ref msg-fields 'nick)
              " sent to "
              (assoc-ref msg-fields 'target)
              ": "
              (assoc-ref msg-fields 'message))
             out)))

(define (parse-privmsg line)
  "Parse a PRIGMSG (LINE)."
  (let* ((nick (substring line 1 (string-index line #\!)))
         (privmsg "PRIVMSG")
         (privmsg-end-index (+ (string-contains line privmsg) (string-length privmsg)))
         (target (string-trim-both (substring line privmsg-end-index (string-index line #\: privmsg-end-index))))
         (target-end-index (+ (string-contains line target) (string-length target)))
         (message (string-trim-both (substring line (+ 2 target-end-index)))))
    `((nick . ,nick)
      (target . ,target)
      (message . ,message))))

(define conn (open-tcp-connection server port))
(define in (connection-input-port conn))
(define out (connection-output-port conn))

(let ((read-line-irc (lambda () (read-line in 'concat))))
  (display "Setting up connection...") (newline)
  ;; Setup the connection.
  (display (string-append "NICK " nick line-end) out)
  (display (string-append "USER " user " 0 * :" name line-end) out)

  (do ((line (read-line-irc) (read-line-irc)))
      (ponged)
    (process-line line))

  (display "Joining channels...") (newline)
  ;; Join channels, then enter the message-handling loop.
  (display (string-append "JOIN #aidalbot" line-end) out)

  (do ((line (read-line-irc) (read-line-irc)))
      ((eof-object? line))
    (process-line line)
    (display (format #t "~s" line))))

#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim)
             (ice-9 regex)
             (ice-9 format)
             (spells network))

(define line-end "\r\n")
(define nick "aidalbot")
(define user "aidalgol")
(define name "Aidan Gauland")
(define server "irc.example.com")
(define port 6667)
(define channels '("#aidalbot"))

(define (process-line line)
  "Process a line from the IRC server."
  (cond ((string-match "^PING" line)
         (pong line out))
        ((string-match "^:(.*)!.*@.* PRIVMSG (.*) :(.*)" line) =>
         (lambda (match) (handle-privmsg
                          `((nick . ,(match:substring match 1))
                            (target . ,(match:substring match 2))
                            (message . ,(match:substring match 3)))
                          out)))))

(define ponged #f) ;; Whether we've responded to a PING.
(define (pong line out)
  "Reply, on OUT, to a ping (which is LINE)."
  (display (string-append "PONG" (substring line 4)) out)
  (set! ponged #t))

(define (handle-privmsg msg-fields out)
  (let* ((message (assoc-ref msg-fields 'message)))
    (cond ((string= message (string-append nick ": quit"))
           (display (string-append "QUIT" line-end) out))
          ((and (char=? (string-ref message 0) #\x01)
                (char=? (string-ref message (1- (string-length message))) #\x01))
           (display "CTCP message.")
           (newline)))
    (begin
      (display (string-append
                "Message received from "
                (assoc-ref msg-fields 'nick)
                " sent to "
                (assoc-ref msg-fields 'target)
                ": \""
                (assoc-ref msg-fields 'message)
                "\"")))))

(define conn (open-tcp-connection server port))
(define in (connection-input-port conn))
(define out (connection-output-port conn))

(let ((read-line-irc (lambda ()
                       "Read a line from an IRC connection, dropping the trailing CRLF."
                       (let ((line (string-drop-right (read-line in) 1)))
                         (format #t "Read line ~s\n" line)
                         line))))
  (display "Setting up connection...") (newline)
  ;; Setup the connection.
  (display (string-append "NICK " nick line-end) out)
  (display (string-append "USER " user " 0 * :" name line-end) out)

  ;; We should now have received an responses 001-004 (right after the
  ;; NOTICEs).  If not, then quit.
  (let lp ((line (read-line-irc))
           (last-msg-num #f))
    (if (eof-object? line)
      (begin
        (display "Error: Connection closed.")
        (newline)
        (quit)))
    ;; Start counting responses when we reach the first one.
    (if (string-match "^:.* 001.*" line)
        (set! last-msg-num 0))
    ;; Verify that we received all expected responses.
    (if (and last-msg-num
             (string-match (format #f "^:.* ~3'0d.*" (1+ last-msg-num)) line))
        (if (< last-msg-num 4)
            (lp (1+ last-msg-num)))))

  (display "Joining channels...") (newline)
  ;; Join channels, then enter the message-handling loop.
  (map (lambda (channel)
         (display (string-append "JOIN " channel line-end) out))
       channels)

  (do ((line (read-line-irc) (read-line-irc)))
      ((eof-object? line))
    (process-line line)))

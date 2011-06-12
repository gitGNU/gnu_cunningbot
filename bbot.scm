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

(define (handle-ctcp msg target out)
  "Respond to a CTCP PRIVMSG sent by TARGET."
  (display (string-append "Responding to CTCP message: " (format #f "~s" msg) " sent by " target))
  (newline)
  (if (string= msg "VERSION")
      (display (string-append "NOTICE " target " :bbot v0.1" line-end) out)))

(define (handle-command line out)
  "Handle a command and its arguments on LINE."
  (let* ((line-tokens (string-split line #\space))
         (cmd (car line-tokens))
         (args (cdr line-tokens)))
  (cond
   ((string= cmd "quit")
    (display (string-append "QUIT" line-end) out)))))

(define (handle-privmsg msg-fields out)
  "Parse and respond to PRIVMSGs."
  (let* ((message (assoc-ref msg-fields 'message)))
    (cond
     ;; Handle CTCP messages.
     ((string-match "\x01(.*)\x01" message) =>
      (lambda (match)
        (display "CTCP message.")
        (handle-ctcp (match:substring match 1) (assoc-ref msg-fields 'nick) out)
        (newline)))
     ;; Treat a message of the form "[NICK]: quit" as a command.
     ((string-match (string-append "^" nick ": (.*)") message) =>
      (lambda (match)
        (handle-command (match:substring match 1) out))))
    (begin
      (display (string-append
                "Message received from "
                (assoc-ref msg-fields 'nick)
                " sent to "
                (assoc-ref msg-fields 'target)
                ": \""
                (assoc-ref msg-fields 'message)
                "\""))
      (newline))))

(define conn (open-tcp-connection server port))
(define in (connection-input-port conn))
(define out (connection-output-port conn))

(let ((read-line-irc (lambda ()
                       "Read a line from an IRC connection, dropping the trailing CRLF."
                       (let ((line (read-line in)))
                         (if (not (eof-object? line))
                             (begin
                               (set! line (string-drop-right line 1))
                               (format #t "Read line ~s\n" line)))
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
    (if (not last-msg-num)
        ;; Start counting responses when we reach the first one.
        (if (string-match "^:.* 001.*" line)
            (set! last-msg-num 0)
            (lp (read-line-irc)
                last-msg-num))
        ;; Verify that we received all expected responses.
        (if (string-match (format #f "^:.* ~3'0d.*" (1+ last-msg-num)) line)
            (if (< last-msg-num 4)
                (lp (read-line-irc)
                    (1+ last-msg-num))))))

  (display "Joining channels...") (newline)
  ;; Join channels, then enter the message-handling loop.
  (map (lambda (channel)
         (display (string-append "JOIN " channel line-end) out))
       channels)

  (do ((line (read-line-irc) (read-line-irc)))
      ((eof-object? line))
    (process-line line)))

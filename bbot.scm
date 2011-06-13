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
;;(define server "irc.example.com")
(define server "chat.freenode.net")
(define port 6667)
(define channels '("#aidalbot"))

(define (process-line line)
  "Process a line from the IRC server."
  (cond ((string-match "^PING" line)
         (pong line))
        ((string-match "^:(.*)!.*@.* PRIVMSG (.*) :(.*)" line) =>
         (lambda (match) (handle-privmsg
                          `((nick . ,(match:substring match 1))
                            (target . ,(match:substring match 2))
                            (message . ,(match:substring match 3))))))))

(define (pong line)
  "Reply, on OUT, to a ping (which is LINE)."
  (display (string-append "PONG" (substring line 4)) out))

(define (handle-ctcp msg target)
  "Respond to a CTCP PRIVMSG sent by TARGET."
  (display (string-append "Responding to CTCP message: " (format #f "~s" msg) " sent by " target))
  (newline)
  (if (string= "VERSION" msg)
      (display (string-append "NOTICE " target " :bbot v0.1" line-end) out)))

(define (send-privmsg message target)
  "Send a PRIVMSG MESSAGE to TARGET."
  (display (string-append "PRIVMSG " target " :" message line-end) out))

(define (cmd-quit)
  (display "Quitting...")
  (newline)
  "QUIT")

(define (handle-command line sender target)
  "Handle a command and its arguments on LINE."
  (catch 'unbound-variable
    (lambda ()
      (display
       (string-append (eval-string (string-append "(cmd-" line ")")) line-end)
       out))
    (lambda (key subr message args rest)
        (send-privmsg (apply format (append (list #f message) args))
                      ;; If the command was sent directly to me, then
                      ;; reply directly to the sender, otherwise,
                      ;; assume it was sent to a channel and reply to
                      ;; the channel.
                      (if (string=? nick target)
                          sender
                          target)))))

(define (handle-privmsg msg-fields)
  "Parse and respond to PRIVMSGs."
  (let* ((message (assoc-ref msg-fields 'message)))
    (cond
     ;; Handle CTCP messages.
     ((string-match "\x01(.*)\x01" message) =>
      (lambda (match)
        (display "CTCP message.")
        (handle-ctcp (match:substring match 1) (assoc-ref msg-fields 'nick))
        (newline)))
     ;; Treat a message of the form "[NICK]: quit" as a command.
     ((string-match (string-append "^" nick ": (.*)") message) =>
      (lambda (match)
        (handle-command (match:substring match 1)
                        (assoc-ref msg-fields 'nick)
                        (assoc-ref msg-fields 'target)))))
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

  ;; We should now have received a responses 001-004 (right after the
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

;; Cunning Bot, an IRC bot written in Guile Scheme.
;; Copyright (C) 2011  Aidan Gauland

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (cunning-bot)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (spells network)
  #:export (make-action
            join-channel
            start-bot))

(define line-end "\r\n")
(define version "Cunning Bot v0.1")
(define debugging #f) ;; Whether to print debugging messages.
(define nick "Cunning_Bot")
(define user "Cunning_Bot")
(define name "Cunning Bot")

(define conn)
(define in)
(define out)

(define-syntax-rule (debug s exp ...)
  (when debugging
    (format #t s exp ...)))

;; `privmsg-hook' is run with the arguments (sender target message ctcp).
(define privmsg-hook (make-hook 4))

(define (irc-send string)
  "Send STRING to the IRC server."
  (debug "Sending line: ~s~%" string)
  (format out "~a~a" string line-end))

(define (read-line-irc)
  "Read a line from an IRC connection, dropping the trailing CRLF."
  (let ((line (read-line in)))
    (if (not (eof-object? line))
        (begin
          (set! line (string-drop-right line 1))
          (debug "Read line ~s~%" line)))
    line))

(define (pong line)
  "Reply to a ping represented by LINE.
LINE should be an IRC PING command from the server."
  (irc-send (format #f "PONG~a" ) (substring line 4)))

(define (send-privmsg message target)
  "Send a PRIVMSG MESSAGE to TARGET."
  (irc-send (format #f "PRIVMSG ~a :~a" target message)))

(define (make-action message)
  "Wrap CTCP ACTION markup around MESSAGE."
  (format #f "\x01ACTION ~a\x01" message))

(define (join-channel channel)
  "Send a JOIN request for CHANNEL.

This does not (yet) handle JOIN responses, so errors are silently
ignored."
  (irc-send (format #f "JOIN ~a" channel)))

(define (quit-irc)
  "Send a QUIT message to the server (to cleanly disconnect)."
  (format #t "Quitting...~%")
  (irc-send "QUIT"))

(define (process-line line)
  "Process a line from the IRC server."
  (cond
   ;; PONG PINGs.
   ((string-match "^PING" line)
    (pong line))
   ;; PRIVMSGs
   ((string-match "^:(.*)!.*@.* PRIVMSG (.*) :(.*)" line) =>
    (lambda (match)
      (handle-privmsg (match:substring match 1)
                      (match:substring match 2)
                      (match:substring match 3))))))

(define (handle-privmsg sender target message)
  "Parse and respond to PRIVMSGs."
  (let* ((match (string-match "\x01(.*)\x01" message))
         (ctcp (if match
                   #t #f)))
    (if ctcp
        (set! message (match:substring match 1)))
    (debug "~:[Message~;CTCP message~] received from ~s sent to ~s: ~s~%"
           ctcp sender target message)
    (debug "Running PRIVMSG hook.~%")
    (run-hook privmsg-hook sender target message ctcp)))

;; Command procedure names are the command name prepended with cmd-
(define (handle-commands sender target message ctcp)
  "Parse and execute command invocations.

If MESSAGE is a command invocation, then attempt to execute it,
catching unbound-variable errors.."
  (let* ((match (string-match (format #f "(~a: )?(\\S*)\\s*(.*)" nick)
                                   message))
         (line-prefix (match:substring match 1))
         (direct (string=? nick target))
         (recipient (if direct sender target))
         (cmd-procname
          (symbol-append
           'cmd-
           (string->symbol (match:substring match 2))))
         (args (match:substring match 3)))
    (when (and match (not ctcp))
      (debug "Received command invocation~%")
      (debug "~/line-prefix => ~s~%" line-prefix)
      (debug "~/direct => ~s~%" direct))
    ;; Only respond if the message was sent directly to me or it is
    ;; prefixed with my nick (i.e. "nick: cmd ...").
    (when (and match
               (or direct line-prefix)
               (not ctcp))
     ;; Try to execute the command procudure.  If there is no such
     ;; procedure, then reply with an error message saying so.
     (catch 'unbound-variable
       (lambda ()
         (let ((result (eval (list cmd-procname sender args) (current-module))))
           (if (string? result)
               (send-privmsg result recipient))))
       (lambda (key subr message args rest)
         (send-privmsg (apply format (append (list #f message) args))
                       ;; If the command was sent directly to me, then
                       ;; reply directly to the sender, otherwise,
                       ;; assume it was sent to a channel and reply to
                       ;; the channel.
                       recipient))))))
(add-hook! privmsg-hook handle-commands)

(define (version-respond sender target message ctcp)
  "Respond to CTCP VERSION requests."
  (debug "Responding to CTCP message: ~s sent by ~s~%" message sender)
  (if (string=? "VERSION" message)
      (irc-send (format #f "NOTICE ~a :~a" sender version))))
(add-hook! privmsg-hook version-respond)

(define (start-bot server port channels)
  ;; Establish TCP connection.
  (format #t "Establishing TCP connection to ~a on port ~d..."
          server port)
  (set! conn (open-tcp-connection server port))
  (format #t "done.~%")
  (set! in (connection-input-port conn))
  (set! out (connection-output-port conn))

  ;; Setup the IRC connection.
  (display "Setting up IRC connection...") (debug "~%")
  (irc-send (format #f "NICK ~a" nick))
  (irc-send (format #f "USER ~a 0 * :~a" user name))
  ;; We should now have received responses 001-004 (right after the
  ;; NOTICEs).  If not, then quit.
  (let lp ((line (read-line-irc))
           (last-msg-num #f))
    (if (eof-object? line)
        (begin
          (format #t "Error: Connection closed.~%")
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
  (display "done.") (newline)
  ;; We are now connected to the IRC server.

  ;; Join channels.
  (display "Joining channels...")
  (for-each join-channel channels)
  (format #t "done.~%")

  ;; Enter the message-polling loop.
  (do ((line (read-line-irc) (read-line-irc)))
      ((eof-object? line))
    (process-line line)))

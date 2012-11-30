#!/usr/bin/env guile
!#

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

(use-modules (ice-9 rdelim)
             (ice-9 regex)
             (ice-9 format)
             (spells network))

(define line-end "\r\n")
(define version "Cunning Bot v0.1")
(define debugging #f) ;; Whether to print debugging messages.
(define nick "Cunning_Bot")
(define user "Cunning_Bot")
(define name "Cunning Bot")
(define server "irc.example.net")
(define port 6667)
(define channels '())

(define-syntax debug
  (syntax-rules ()
    ((debug s exp ...)
     (when debugging
       (format #t s exp ...)))))

(primitive-load "init.scm")

;; `handle-privmsg-hook' is run with the arguments SENDER TARGET and
;; MESSAGE.
(define handle-privmsg-hook (make-hook 3))

(define (channel-name? string)
  "Returns whether STRING is a channel name."
  (string-match "^#" string))

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

(define (pong line)
  "Reply to a ping represented by LINE.
LINE should be an IRC PING command from the server."
  (irc-send (format #f "PONG~a" (substring line 4))))

(define (handle-ctcp msg target)
  "Respond to a CTCP PRIVMSG sent by TARGET."
  (debug "Responding to CTCP message: ~s sent by ~s~%" target)
  (if (string=? "VERSION" msg)
      (irc-send (format #f "NOTICE ~a :~a" target version))))

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

(define (send-privmsg message target)
  "Send a PRIVMSG MESSAGE to TARGET."
  (irc-send (format #f "PRIVMSG ~a :~a" target message)))

(define (send-action message target)
  "Send MESSAGE to target as a CTCP ACTION.

Essentially a convenience wrapper around `send-privmsg'."
  (send-privmsg (format #f "\x01ACTION ~a\x01" message)
                target))

(define (join-channel channel)
  "Send a JOIN request for CHANNEL.

This does not (yet) handle JOIN responses, so errors are silently
ignored."
  (irc-send (format #f "JOIN ~a" channel)))

(define (quit-irc)
  "Send a QUIT message to the server (to cleanly disconnect)."
  (format #t "Quitting...~%")
  (irc-send "QUIT"))

;; Command procedure names are the command name prepended with cmd-
(define (handle-command line sender target)
  "Handle a command and its arguments on LINE."
  (let* ((line-match (string-match "(\\S*)\\s*(.*)" line))
         (cmd-procname
          (symbol-append
           'cmd-
           (string->symbol (match:substring line-match 1))))
         (args (match:substring line-match 2)))
    ;; Try to execute the command procudure.  If there is no such
    ;; procedure, then reply with an error message saying so.
    (catch 'unbound-variable
      (lambda ()
        (let ((result (eval (list cmd-procname sender target args) (current-module))))
          (if (string? result)
              (irc-send result))))
      (lambda (key subr message args rest)
        (send-privmsg (apply format (append (list #f message) args))
                      ;; If the command was sent directly to me, then
                      ;; reply directly to the sender, otherwise,
                      ;; assume it was sent to a channel and reply to
                      ;; the channel.
                      (if (string=? nick target)
                          sender
                          target))))))

(define (handle-privmsg sender target message)
  "Parse and respond to PRIVMSGs."
  (let ((match (string-match "\x01(.*)\x01" message)))
    (debug "Message received from ~s sent to ~s: ~s~%"
           sender target message)
    (run-hook handle-privmsg-hook
              sender target message)
    ;; Check whether it's a CTCP message.
    (if match
        (begin ; It is a CTCP message.
          (debug "CTCP message.~%")
          (handle-ctcp (match:substring match 1) sender))
        (begin ; It is a regular PRIVMSG.
          (cond
           ;; If the message was sent to a channel, then respond only
           ;; to messages of the form "NICK: CMD" as a command.
           ((channel-name? target)
            (set! match (string-match (format #f "^~a: (.*)" nick) message))
            (if match
                (handle-command (match:substring match 1)
                                sender
                                target)))
           ;; If the message was sent to the us directly, then treat
           ;; the whole line as a command.
           ((string=? nick target)
            (handle-command message
                            sender
                            sender)))))))

;; Establish TCP connection.
(format #t "Establishing TCP connection to ~a on port ~d..."
        server port)
(define conn (open-tcp-connection server port))
(format #t "done.~%")
(define in (connection-input-port conn))
(define out (connection-output-port conn))

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
  (process-line line))

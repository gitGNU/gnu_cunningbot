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
(define version " :Cunning Bot v0.1")
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
  (cond ((string-match "^PING" line)
         (pong line))
        ((string-match "^:(.*)!.*@.* PRIVMSG (.*) :(.*)" line) =>
         (lambda (match) (handle-privmsg
                          `((nick . ,(match:substring match 1))
                            (target . ,(match:substring match 2))
                            (message . ,(match:substring match 3))))))))

(define (pong line)
  "Reply to a ping represented by LINE.
LINE should be an IRC PING command from the server."
  (display (string-append "PONG" (substring line 4)) out))

(define (handle-ctcp msg target)
  "Respond to a CTCP PRIVMSG sent by TARGET."
  (debug "Responding to CTCP message: ~s sent by ~s~%" target)
  (if (string=? "VERSION" msg)
      (display (string-append "NOTICE " target version line-end) out)))

(define (send-privmsg message target)
  "Send a PRIVMSG MESSAGE to TARGET."
  (let ((line (string-append "PRIVMSG " target " :" message)))
    (debug "Sending line: ~s~%" line)
    (display (string-append line line-end) out)))

(define (send-action message target)
  "Send MESSAGE to target as a CTCP ACTION.

Essentially a convenience wrapper around `send-privmsg'."
  (send-privmsg (string-append "\x01ACTION " message "\x01")
                target))

(define (join-channel channel)
  "Send a JOIN request for CHANNEL.

This does not (yet) handle JOIN responses, so errors are silently
ignored."
  (display (string-append "JOIN " channel line-end) out))

(define (quit-irc)
  "Send a QUIT message to the server (to cleanly disconnect)."
  (format #t "Quitting...~%")
  (display (string-append "QUIT" line-end) out))

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
              (display (string-append result line-end) out))))
      (lambda (key subr message args rest)
        (send-privmsg (apply format (append (list #f message) args))
                      ;; If the command was sent directly to me, then
                      ;; reply directly to the sender, otherwise,
                      ;; assume it was sent to a channel and reply to
                      ;; the channel.
                      (if (string=? nick target)
                          sender
                          target))))))

(define (handle-privmsg msg-fields)
  "Parse and respond to PRIVMSGs."
  (let* ((message (assoc-ref msg-fields 'message))
         (match #f))
    (debug "Message received from ~s sent to ~s: ~s~%"
           (assoc-ref msg-fields 'nick)
           (assoc-ref msg-fields 'target)
           (assoc-ref msg-fields 'message))
    (run-hook handle-privmsg-hook
              (assoc-ref msg-fields 'nick)
              (assoc-ref msg-fields 'target)
              (assoc-ref msg-fields 'message))
    ;; Check whether it's a CTCP message.
    (set! match (string-match "\x01(.*)\x01" message))
    (if match
        (begin ; It is a CTCP message.
          (debug "CTCP message.~%")
          (handle-ctcp (match:substring match 1) (assoc-ref msg-fields 'nick)))
        (begin ; It is a regular PRIVMSG.
          (cond
           ;; If the message was sent to a channel, then respond only
           ;; to messages of the form "NICK: CMD" as a command.
           ((channel-name? (assoc-ref msg-fields 'target))
            (set! match (string-match (string-append "^" nick ": (.*)") message))
            (if match
                (handle-command (match:substring match 1)
                                (assoc-ref msg-fields 'nick)
                                (assoc-ref msg-fields 'target))))
           ;; If the message was sent to the us directly, then treat
           ;; the whole line as a command.
           ((string=? nick (assoc-ref msg-fields 'target))
            (handle-command message
                            (assoc-ref msg-fields 'nick)
                            (assoc-ref msg-fields 'nick))))))))

;; Establish TCP connection.
(format #t "Establishing TCP connection to ~a on port ~d..."
        server port)
(define conn (open-tcp-connection server port))
(define in (connection-input-port conn))
(define out (connection-output-port conn))
(format #t "done.~%")
(define (read-line-irc)
  "Read a line from an IRC connection, dropping the trailing CRLF."
  (let ((line (read-line in)))
    (if (not (eof-object? line))
        (begin
          (set! line (string-drop-right line 1))
          (debug "Read line ~s~%" line)))
    line))

;; Setup the IRC connection.
(display "Setting up IRC connection...") (debug "~%")
(display (string-append "NICK " nick line-end) out)
(display (string-append "USER " user " 0 * :" name line-end) out)

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

(display "Joining channels...")
;; Join channels, then enter the message-handling loop.
(map join-channel
     channels)
(format #t "done.~%")

(do ((line (read-line-irc) (read-line-irc)))
    ((eof-object? line))
  (process-line line))

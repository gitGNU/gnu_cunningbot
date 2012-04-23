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

(load "init.scm")

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
  (if debugging
      (begin
        (display (string-append "Responding to CTCP message: " (format #f "~s" msg) " sent by " target))
        (newline)))
  (if (string= "VERSION" msg)
      (display (string-append "NOTICE " target version line-end) out)))

(define (send-privmsg message target)
  "Send a PRIVMSG MESSAGE to TARGET."
  (let ((line (string-append "PRIVMSG " target " :" message)))
    (if debugging
        (begin
          (display (string-append "Sending line: \"" line "\""))
          (newline)))
    (display (string-append line line-end) out)))

(define (join-channel channel)
  (display (string-append "JOIN " channel line-end) out))

(define (quit-irc)
  (display "Quitting...")
  (newline)
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
        (let ((result (eval (list cmd-procname target args) (current-module))))
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
  (let* ((message (assoc-ref msg-fields 'message)))
    (if debugging
        (begin
          (display (string-append
                    "Message received from "
                    (assoc-ref msg-fields 'nick)
                    " sent to "
                    (assoc-ref msg-fields 'target)
                    ": \""
                    (assoc-ref msg-fields 'message)
                    "\""))
          (newline)))
    (cond
     ;; Handle CTCP messages.
     ((string-match "\x01(.*)\x01" message) =>
      (lambda (match)
        (if debugging 
            (begin (display "CTCP message.") (newline)))
        (handle-ctcp (match:substring match 1) (assoc-ref msg-fields 'nick))))
     ;; Treat a message of the form "NICK: CMD" as a command.
     ((string-match (string-append "^" nick ": (.*)") message) =>
      (lambda (match)
        (handle-command (match:substring match 1)
                        (assoc-ref msg-fields 'nick)
                        (assoc-ref msg-fields 'target)))))))

(define conn (open-tcp-connection server port))
(define in (connection-input-port conn))
(define out (connection-output-port conn))
(define (read-line-irc)
  "Read a line from an IRC connection, dropping the trailing CRLF."
  (let ((line (read-line in)))
    (if (not (eof-object? line))
        (begin
          (set! line (string-drop-right line 1))
          (if debugging
              (format #t "Read line ~s\n" line))))
    line))

(display "Setting up connection...")
;; Setup the connection.
(display (string-append "NICK " nick line-end) out)
(display (string-append "USER " user " 0 * :" name line-end) out)

;; We should now have received responses 001-004 (right after the
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
(display "done.") (newline)

(display "Joining channels...")
;; Join channels, then enter the message-handling loop.
(map join-channel
     channels)
(display "done.") (newline)

(do ((line (read-line-irc) (read-line-irc)))
    ((eof-object? line))
  (process-line line))

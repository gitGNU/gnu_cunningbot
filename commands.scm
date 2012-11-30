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

(define (cmd-flay sender target args)
  (if (string-null? args)
      (send-privmsg "Who do you want me to flay?" target)
      (send-action (string-append "flays " args ".") target)))

(define (cmd-say-hello sender target args)
  (send-privmsg
   (if (channel-name? target)
       (string-append sender ": Hello!")
       "Hello!")
   target))

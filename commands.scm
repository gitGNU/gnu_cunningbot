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

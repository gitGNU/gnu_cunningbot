(define (cmd-flay target args)
  (if (string-null? args)
      (send-privmsg "Who do you want me to flay?" target)
      (send-action (string-append "flays " args ".") target)))

(define (cmd-say-hello target args)
  (send-privmsg
   "Hello!" target))

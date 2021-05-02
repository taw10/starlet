(define-module (starlet guile-ola)
  #:export (<OlaDmxBuffer>
             <OlaStreamingClient>
             send-streaming-dmx-data!
             make-ola-dmx-buffer
             set-ola-dmx-buffer!
             make-ola-streaming-client
             ola-dmx-buffers-equal?))

(if (not (provided? 'guile-ola))
    (load-extension "libguile-ola"
                    "init_guile_ola"))


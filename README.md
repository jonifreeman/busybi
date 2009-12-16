How to test
===========

1. Clone the stuff: git clone git://github.com/jonifreeman/busybi.git
2. cd busybi
3. cp src/test/scala/examples/chat/*[.html,.js] /var/www/  (or wherever your HTTP server is)
4. sbt console
5. scala> Chat.main(Array())
6. Open http://localhost/chat.html with websocket enabled browser (for instance Chrome)


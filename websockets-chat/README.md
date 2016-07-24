# Websockets chat

Barebones websocket chat application for [Reflex-DOM](https://github.com/reflex-frp/reflex-dom/) using [wai-websockets](https://hackage.haskell.org/package/wai-websockets) on the backend.

Chat usernames are auto-assigned upon successful connection to the server.

## Server

The server uses in-memory storage to keep track of connected clients and sent messages.

It can be launched by running the executable resulting from compiling `server/Server.hs` (or directly via runhaskell/runghc).
The server starts on port 3000 and does not do anything particularly interesting if you attempt to make
requests outside of the ws protocol.

## Client

To launch the client, just open `client/source.jsexe/index.html` in your browser.

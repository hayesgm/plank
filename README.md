
# The Plank Game Engine

Plank is a game engine written in Elm to run on CloudFlare Workers.

## Getting Started

For local development, you need to start a client and a server.

### Running the client

```sh
npm run dev:app
```

You should have a local server running on (http://localhost:2222/)[http://localhost:2222/].

### Running the server

The server runs via Wrangler from CloudFlare. First, [install wrangler](https://developers.cloudflare.com/workers/wrangler/install-and-update/):

```sh
npm install -g wrangler
```

Next, build the Elm code and run wrangler.

```sh
npm run dev:server
```

You should have a server running on [http://127.0.0.1:2233](http://127.0.0.1:2233).

This will automatically reload whenever your JavaScript or Elm code changes. Note: your code reload my break any running game connections.

## Developing

### Types

It's strongly recommended that you put your game types (e.g. State, Model, Msg, etc) in `Game/{Game}/Types.elm` and use automatic code generation to build encoders and decoders. Since nearly everything needs to be serializable, this will save your hours of effort and potential bugs.

E.g.

**Game/TicTacToe/Types.elm**

```elm
-- [generator-start]


type EngineMsg
    = JoinGame
    | Claim Int
    | Tick Posix


type ViewMsg
    = EngineMsg EngineMsg
    | Tock Posix

-- ...
-- [generator-end]
```

Then run:

```sh
npm run build-types src/Game/TicTacToe/Types.elm
```

There may be some types that need custom encoders / decoders, but you can easily append those at the end of `Types.elm`.

## License

All rights reserved.

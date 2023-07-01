
# The Plank Game Engine

Plank is a game engine written in Elm to run on CloudFlare Workers.

## Getting Started

For local development, you need to start a client and a server.

### Running the client

```sh
npm run dev
```

You should have a local server running on (http://localhost:2222/)[http://localhost:2222/].

### Running the server

The server runs via Wrangler from CloudFlare. First, [install wrangler](https://developers.cloudflare.com/workers/wrangler/install-and-update/):

```sh
npm install -g wrangler
```

Next, build the Elm code and run wrangler:

```sh
npm run build && wrangler dev --local
```

Note: this server does not currently auto-reload. You will need to reload it when you change code, esp. your Elm code.

You should have a server running on [http://127.0.0.1:2233](http://127.0.0.1:2233).


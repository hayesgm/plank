import { Elm } from './src/Server.elm'

// Worker

const GUEST_TTL = 60 * 60; // 1 hour

const corsHeaders = {
  'Access-Control-Allow-Origin': '*',
  'Access-Control-Allow-Methods': 'GET,HEAD,POST,OPTIONS',
  'Access-Control-Max-Age': '86400',
};

export default {
  async fetch(request, env) {
    return await handleRequest(request, env);
  }
}

function getRandomId(sz = 8) {
  let id = new Uint8Array(sz);
  crypto.getRandomValues(id);
  return [...id].map((x) => x.toString(16)).join('');
}

function getRandomNonce() {
  return getRandomId(30);
}

// TODO: Check for uniqueness
function generateGameId() {
  return `game_${getRandomId()}`
}

function generatePlayerId() {
  return `user_${getRandomId()}`
}

function jsonResp(res) {
  return new Response(JSON.stringify(res, null, 4), {
    headers: {
      'Content-Type': 'application/json',
      'Access-Control-Allow-Origin': '*'
    }
  });
}

function notFound() {
  return new Response("Not found", {status: 404});
}

function forbidden() {
  return new Response("Forbidden", {status: 403});
}

function getPlank(gameId, env) {
  return env.plank.get(env.plank.idFromName(gameId));
}

async function handleRequest(request, env) {
  let url = new URL(request.url);
  let pathnames = url.pathname.split('/').filter((x) => x !== '');
  let prefix = pathnames.shift();

  switch (prefix) {
  case undefined:
    return new Response("Welcome to Plank.");
  case "login":
    if (pathnames[0] === 'guest') {
      // Create a new guest account for user
      let playerId = generatePlayerId();
      let nonce = getRandomNonce();
      await env.plank_guests.put(nonce, playerId, {expirationTtl: GUEST_TTL});
      return jsonResp({playerId, nonce});
    } else {
      return notFound();
    }
  case "game":
      if (pathnames[0] === 'new') {
        let gameName = pathnames[1];

        // Generate a unique new Game ID
        let gameId = generateGameId();

        // New blank
        let plank = getPlank(gameId, env);

        let plankRequest = new Request(
          `${url.origin}/initialize/${gameName}/${gameId}`
          , request);

        return await plank.fetch(plankRequest);
      } else if (pathnames[0] === 'connect') {
        let gameId = pathnames[1];

        // Should be existing
        // TODO: Check?
        let plank = getPlank(gameId, env);
        return await plank.fetch(request);
      } else {
        return notFound();
      }
  default:
    return notFound();
  }
}

// The Plank Durable Object
export class Plank {
  constructor(state, env) {
    this.state = state;
    this.env = env;
  }

  log(...msg) {
    let header = `[Plank]`;
    if (this.gameName) {
      header += `[${this.gameName}]`;
    }
    if (this.gameId) {
      header += `[${this.gameId}]`
    }
    console.log(header, ...msg);
  }

  async auth(request) {
    let nonce;
    
    let authHeader = request.headers.get("Authorization");
    if (authHeader !== null) {
      let match = authHeader.match(/Bearer (?<token>\w+)/i);
      if (match) {
        nonce = match.groups.token;
      }
    }

    let wsAuthHeader = request.headers.get("Sec-WebSocket-Protocol");
    if (nonce === undefined && wsAuthHeader !== null) {
      nonce = wsAuthHeader
    }

    if (nonce) {
      let playerId = await this.env.plank_guests.get(nonce);
      this.log('Authenticated Guest:', playerId);
      if (playerId !== null) {
        return {
          playerId
        };
      }
    }

    return {
      response: forbidden()
    }
  }

  async initialize(playerId, gameId, gameName) {
    await this.state.blockConcurrencyWhile(async () => {
      await Promise.all([
        this.state.storage.put('gameId', gameId),
        this.state.storage.put('gameName', gameName),
        this.state.storage.put('playerId', playerId)
      ]);

      this.gameId = gameId;
      this.gameName = gameName;

      this.game = Elm.Server.init({
        flags: gameName
      });

      this.game.ports.log.subscribe((msg) => {
        this.log(msg);
      });

      this.gameState = await this.state.storage.get('state');

      // TODO: Is there a cleaner way to make sure we get state first?
      let resolve;
      let gameStateSet = new Promise((resolve_, reject_) => {
        resolve = resolve_;
      });

      if (this.gameState !== undefined) {
        resolve(); // Don't need to wait for game state from app
      }

      this.game.ports.giveState.subscribe(async (state) => {
        this.gameState = state;
        this.state.storage.put('state', JSON.stringify(state));
        resolve();
      });

      // TODO: We shouldn't really add ports, but it's worth understanding
      this.game.ports.wordleFetch.subscribe(async () => {
        console.log("wordle fetch!!");
        let res = await fetch('https://www.nytimes.com/svc/wordle/v2/2023-07-02.json');
        let json = await res.json();
        console.log("solution", json.solution);
        this.game.ports.wordleGot.send(json.solution);
      })

      await gameStateSet;
    });

    return jsonResp({gameId});
  }

  async ensureGameRunning() {
    if (this.game === undefined) {
      let [gameId, gameName, playerId] = await Promise.all([
        this.state.storage.get('gameId'),
        this.state.storage.get('gameName'),
        this.state.storage.get('playerId'),
      ]);

      if (gameId === undefined || gameName === undefined) {
        this.log("Game not found");
        return notFound();
      }

      await this.initialize(playerId, gameId, gameName);
    }
  }

  async handlePlayerMessage(playerId, event) {
    let action = JSON.parse(event.data);
    console.log("[handlePlayerMessage]", "playerId", playerId, "action", action);
    // TODO: Extra things with event.data?
    this.game.ports.receiveAction.send([playerId, action]);
  }

  async connect(playerId, request) {
    let upgradeHeader = request.headers.get("Upgrade")
    if (upgradeHeader !== "websocket") {
      return new Response("Expected websocket", { status: 400 })
    }

    let webSocketPair = new WebSocketPair();
    let [client, server] = Object.values(webSocketPair);

    let ensureRes = await this.ensureGameRunning();
    if (ensureRes !== undefined) {
      return ensureRes;
    }

    // TODO: Maybe group these calls?
    this.game.ports.giveState.subscribe((state) => {
      this.log(playerId, "New Game State: ", state);
      server.send(JSON.stringify({state}));
    })

    server.accept();
    server.addEventListener('message', event => {
      this.log(playerId, "Player Message: ", event.data);
      this.handlePlayerMessage(playerId, event);
    });

    server.send(JSON.stringify({
      connected: {
        gameName: this.gameName,
        gameId: this.gameId,
        gameState: this.gameState,
        playerId
      }
    }));

    return new Response(null, {
      status: 101,
      webSocket: client,
    });
  }

  async handleOptions(request) {
    if (
      request.headers.get("Origin") !== null &&
      request.headers.get("Access-Control-Request-Method") !== null &&
      request.headers.get("Access-Control-Request-Headers") !== null
    ) {
      // Handle CORS preflight requests.
      return new Response(null, {
        headers: {
          ...corsHeaders,
          "Access-Control-Allow-Headers": request.headers.get(
            "Access-Control-Request-Headers"
          ),
        },
      });
    } else {
      // Handle standard OPTIONS request.
      return new Response(null, {
        headers: {
          Allow: "GET, HEAD, POST, OPTIONS",
        },
      });
    }
  }

  // Handle HTTP requests from clients.
  async fetch(request) {
    if (request.method === "OPTIONS") {
      // Handle CORS preflight requests
      return this.handleOptions(request);
    }

    let authRes = await this.auth(request);
    if ('response' in authRes) {
      return authRes.response;
    }
    let { playerId } = authRes;

    let url = new URL(request.url);
    let pathnames = url.pathname.split('/').filter((x) => x !== '');
    let prefix = pathnames.shift();
    if (prefix === 'initialize') {
      let [gameName, gameId] = pathnames;
      return this.initialize(playerId, gameId, gameName);
    } else {
      return this.connect(playerId, request);
    }
  }
}

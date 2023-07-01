import { Elm } from './src/Server.elm'

// Worker

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

// TODO: Check for uniqueness
function generateGameId() {
  return `game_${getRandomId()}`
}

function generatePlayerId() {
  return `user_${getRandomId()}`
}

function jsonResp(res) {
  return new Response(JSON.stringify(res, null, 4), {
    'Content-Type': 'application/json'
  });
}

async function handleRequest(request, env) {
  console.log("env", env);
  let url = new URL(request.url);
  let pathnames = url.pathname.split('/').filter((x) => x !== '');
  let prefix = pathnames.shift();
  console.log({prefix, pathnames});

  switch (prefix) {
  case undefined:
    return new Response("Welcome to Plank.");
  case "game":
      if (pathnames[0] === 'new') {
        let gameName = pathnames[1];

        // Generate a unique new Game ID
        let gameId = generateGameId();

        let plank = env.PLANK.get(env.PLANK.idFromName(gameId));
        return await plank.fetch(`${url.origin}/initialize/${gameName}/${gameId}`);
      } else if (pathnames[0] === 'connect') {
        let gameId = pathnames[1];
        let plank = env.PLANK.get(env.PLANK.idFromName(gameId));
        return await plank.fetch(request.url);
      }
  default:
    return new Response("Not found", {status: 404});
  }
}

// The Plank Durable Object
export class Plank {
  constructor(state, env) {
    this.state = state;
  }

  log(...msg) {
    console.log(`[Plank][${this.gameName}][${this.gameId}]`, ...msg);
  }

  initialize(gameId, gameName) {
    this.gameId = gameId;
    this.gameName = gameName;

    this.game = Elm.Server.init({
      flags: "Initial Message"
    });

    this.game.ports.log.subscribe((msg) => {
      this.log(msg);
    });

    return jsonResp({gameId});
  }

  async handlePlayerMessage(playerId, event) {
    let action = JSON.parse(event.data);
    console.log("playerId", playerId, "action", action);
    // TODO: Extra things with event.data?
    this.game.ports.receiveAction.send(action);
  }

  async upgrade(request) {
    // TODO: Double check the client is trying to upgrade?
    const webSocketPair = new WebSocketPair();
    const [client, server] = Object.values(webSocketPair);
    const playerId = generatePlayerId();

    // TODO: Pass this state back
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
        playerId
      }
    }));

    return new Response(null, {
      status: 101,
      webSocket: client,
    });
  }

  // Handle HTTP requests from clients.
  async fetch(request) {
    let url = new URL(request.url);
    let pathnames = url.pathname.split('/').filter((x) => x !== '');
    let prefix = pathnames.shift();
    if (prefix === 'initialize') {
      let [gameName, gameId] = pathnames;
      return this.initialize(gameId, gameName);
    } else {
      return this.upgrade(request);
    }
  }
}

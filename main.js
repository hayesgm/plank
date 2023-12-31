import './style.css'
import { Elm } from './src/Main.elm'

function getSession() {
  let sessionEnc = sessionStorage.getItem('session');
  if (sessionEnc) {
    return JSON.parse(sessionEnc);
  } else {
    return null;
  }
}

// TODO: Improve session invalidation flow, maybe after we have full
//       user accounts.
function invalidateSession(maybeGameName) {
  sessionStorage.removeItem('session');
  app.ports.sessionInvalid.send(maybeGameName);
}

const assetMapping =  import.meta.glob("./src/Game/*/assets/*.(jpg|JPG|png|PNG|svg|css)", { as: "url", eager: true });
const ssl = import.meta.env.VITE_PLANK_SSL === 'true' ?? false;
const host = import.meta.env.VITE_PLANK_HOST ?? 'localhost:2233';
const session = getSession();

console.log("session", session);
const app = Elm.Main.init({
  flags: {
    assetMapping,
    session
  },
  node: document.getElementById('root'),
});

app.ports.log.subscribe((msg) => {
  console.log("[Plank][Log] " + msg);
});

let websocket;

app.ports.sessionGuestLogin.subscribe(async () => {
  console.log("Login as guest");
  let resp = await fetch(`http${ssl ? 's' : ''}://${host}/login/guest`);
  let json = await resp.json();
  sessionStorage.setItem('session', JSON.stringify(json));
  app.ports.sessionReceive.send(json);
});

app.ports.joinGame.subscribe(([nonce, gameId]) => {
  console.log(`Joining game: ${gameId}`);
  websocket = new WebSocket(`ws${ssl ? 's' : ''}://${host}/game/connect/${gameId}`, [ nonce ]);

  websocket.addEventListener('error', (error) => {
    console.error('Websocket error', error);

    // TODO: It might be nice to work on looking into the
    //       error a bit more.
    invalidateSession(null)

  })

  websocket.addEventListener('message', (event) => {
    console.log(`Message received from ${gameId} server`, event.data);
    let data = JSON.parse(event.data);
    if ('connected' in data) {
      console.log("Game connected", data.connected);
      app.ports.gameConnected.send(data.connected);
    } else if ('state' in data) {
      console.log("Received new game state", data.state);
      app.ports.receiveState.send(data.state);
    } else if ('action' in data) {
      console.log("Received player action", data.action);
      app.ports.receiveAction.send(data.action);
    } else if ('error' in data) {
      let { error } = data;
      console.error("Server error", error);
      if (error.status === 403) {
        invalidateSession(null);
      }
    } else {
      console.log('Ignoring unknown server message', event.data);
    }
  });
});

app.ports.sendAction.subscribe((action) => {
  console.log("Send action", action, websocket);
  if (websocket) {
    websocket.send(JSON.stringify(action));
  }
});

app.ports.disconnect.subscribe(() => {
  console.log("Disconnecting from websocket");
  if (websocket) {
    websocket.close();
    websocket = undefined;
  }
});

let cssEl;

app.ports.loadCss.subscribe((asset) => {
  console.log("Load css", asset);

  // TODO: Clear out if exists?
  cssEl = document.createElement('link');
  cssEl.setAttribute('rel', 'stylesheet');
  cssEl.setAttribute('type', 'text/css');
  cssEl.setAttribute('href', asset);
  document.getElementsByTagName('head')[0].appendChild(cssEl);
});

app.ports.newGame.subscribe(async ([nonce, gameName]) => {
  console.log("New game", gameName, nonce);
  let resp = await fetch(`http${ssl ? 's' : ''}://${host}/game/new/${gameName}`, { headers: {'Authorization': `Bearer ${nonce}`}});
  console.log(resp);
  if (resp.status === 200) {
    let json = await resp.json();
    app.ports.newGameResp.send(json);
  } else if (resp.status === 403) {
    invalidateSession(gameName);
  }
});

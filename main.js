import './style.css'
import { Elm } from './src/Main.elm'

const imagesUrl =  import.meta.glob("./src/Game/*/assets/*.(jpg|JPG|png|PNG|svg|css)", { as: "url", eager: true });

const ssl = import.meta.env.VITE_PLANK_SSL === 'true' ?? false;
const host = import.meta.env.VITE_PLANK_HOST ?? 'localhost:2233';
const app = Elm.Main.init({
  flags: imagesUrl,
  node: document.getElementById('root'),
});

let websocket;

app.ports.joinGame.subscribe((gameId) => {
  console.log(`Joining game: ${gameId}`);
  websocket = new WebSocket(`ws${ssl ? 's' : ''}://${host}/game/connect/${gameId}`);

  websocket.addEventListener('message', event => {
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

app.ports.newGame.subscribe(async (gameName) => {
  console.log("New game", gameName);
  let resp = await fetch(`http${ssl ? 's' : ''}://${host}/game/new/${gameName}`);
  let json = await resp.json();
  app.ports.newGameResp.send(json);
});

import './style.css'
import { Elm } from './src/Main.elm'

const ssl = false;
const host = 'localhost:2233';
const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: "Initial Message"
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

// app.ports.receiveState.send({
//   turn: 'o',
//   tiles: [
//     'open',
//     'open',
//     'open',
//   ],
//   winner: null
// });

app.ports.sendAction.subscribe((action) => {
  console.log("Send action", action, websocket);
  if (websocket) {
    websocket.send(JSON.stringify(action));
  }
});

app.ports.newGame.subscribe(async (gameName) => {
  console.log("New game", gameName);
  let resp = await fetch(`http${ssl ? 's' : ''}://${host}/game/new/${gameName}`);
  let json = await resp.json();
  app.ports.newGameResp.send(json);
});

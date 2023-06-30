import './style.css'
import { Elm } from './src/Main.elm'
const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: "Initial Message"
});

app.ports.connectWC.subscribe(function(uri) {
  console.log("connect wc", uri);
  connectWC(uri);
});

async function locationHashChanged() {
  console.log("location.hash", location.hash);
  if (location.hash === "#tic-tac-toe") {
    let gameElm = await import('./src/Game/TicTacToe/Main.elm');
    console.log({gameElm});
    const game = gameElm.Elm.Game.TicTacToe.Main.init({
      node: document.getElementById('game'),
      flags: "Initial Message"
    });
  }
};

window.onhashchange = locationHashChanged;

locationHashChanged();